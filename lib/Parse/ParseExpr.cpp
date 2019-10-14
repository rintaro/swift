//===--- ParseExpr.cpp - Swift Language Parser for Expressions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Expression Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "ParseList.h"
#include "swift/Parse/Parser.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/EditorPlaceholder.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/ParsedSyntaxBuilders.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxKind.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::syntax;

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExpressionSyntax(Diag<> ID) {
  SourceLoc ExprLoc = Tok.getLoc();
  SyntaxParsingContext ExprParsingContext(SyntaxContext,
                                          SyntaxContextKind::Expr);
  ExprParsingContext.setTransparent();
  ParserResult<Expr> Result = parseExpr(ID);
  if (auto ParsedExpr = ExprParsingContext.popIf<ParsedExprSyntax>()) {
    Generator.addExpr(Result.getPtrOrNull(), ExprLoc);
    return makeParsedResult(std::move(*ParsedExpr), Result.getStatus());
  }
  return Result.getStatus();
}

/// parseExpr
///
///   expr:
///     expr-sequence(basic | trailing-closure)
///
/// \param isExprBasic Whether we're only parsing an expr-basic.
ParserResult<Expr> Parser::parseExprImpl(Diag<> Message,
                                         bool isExprBasic) {
  // Start a context for creating expression syntax.
  SyntaxParsingContext ExprParsingContext(SyntaxContext, SyntaxContextKind::Expr);

  // If we are parsing a refutable pattern, check to see if this is the start
  // of a let/var/is pattern.  If so, parse it to an UnresolvedPatternExpr and
  // name binding will perform final validation.
  //
  // Only do this if we're parsing a pattern, to improve QoI on malformed
  // expressions followed by (e.g.) let/var decls.
  //
  if (InVarOrLetPattern && isOnlyStartOfMatchingPattern()) {
    ParserResult<Pattern> pattern = parseMatchingPattern(/*isExprBasic*/false);
    if (pattern.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (pattern.isNull())
      return nullptr;
    SyntaxContext->setCreateSyntax(SyntaxKind::UnresolvedPatternExpr);
    return makeParserResult(new (Context) UnresolvedPatternExpr(pattern.get()));
  }
  
  auto expr = parseExprSequence(Message, isExprBasic,
                                /*forConditionalDirective*/false);
  if (expr.hasCodeCompletion())
    return expr;
  if (expr.isNull())
    return nullptr;
  
  return makeParserResult(expr.get());
}

/// parseExprIs
///   expr-is:
///     'is' type
ParserResult<Expr> Parser::parseExprIs() {
  SourceLoc isLoc = consumeToken(tok::kw_is);

  ParserResult<TypeRepr> type = parseType(diag::expected_type_after_is);
  if (type.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (type.isNull())
    return nullptr;

  return makeParserResult(new (Context) IsExpr(isLoc, type.get()));
}

/// parseExprAs
///   expr-as:
///     'as' type
///     'as?' type
///     'as!' type
ParserResult<Expr> Parser::parseExprAs() {
  // Parse the 'as'.
  SourceLoc asLoc = consumeToken(tok::kw_as);

  // Parse the postfix '?'.
  SourceLoc questionLoc;
  SourceLoc exclaimLoc;
  if (Tok.is(tok::question_postfix)) {
    questionLoc = consumeToken(tok::question_postfix);
  } else if (Tok.is(tok::exclaim_postfix)) {
    exclaimLoc = consumeToken(tok::exclaim_postfix);
  }

  ParserResult<TypeRepr> type = parseType(diag::expected_type_after_as);

  if (type.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();
  if (type.isNull())
    return nullptr;

  Expr *parsed;
  if (questionLoc.isValid()) {
    parsed = new (Context) ConditionalCheckedCastExpr(asLoc, questionLoc,
                                                      type.get());
  } else if (exclaimLoc.isValid()) {
    parsed = new (Context) ForcedCheckedCastExpr(asLoc, exclaimLoc, type.get());
  } else {
    parsed = new (Context) CoerceExpr(asLoc, type.get());
  }
  return makeParserResult(parsed);
}

/// parseExprArrow
///
///   expr-arrow:
///     '->'
///     'throws' '->'
ParserResult<Expr> Parser::parseExprArrow() {
  SourceLoc throwsLoc, arrowLoc;
  if (Tok.is(tok::kw_throws)) {
    throwsLoc = consumeToken(tok::kw_throws);
    if (!Tok.is(tok::arrow)) {
      diagnose(throwsLoc, diag::throws_in_wrong_position);
      return nullptr;
    }
  }
  arrowLoc = consumeToken(tok::arrow);
  if (Tok.is(tok::kw_throws)) {
    diagnose(Tok.getLoc(), diag::throws_in_wrong_position);
    throwsLoc = consumeToken(tok::kw_throws);
  }
  auto arrow = new (Context) ArrowExpr(throwsLoc, arrowLoc);
  return makeParserResult(arrow);
}

/// parseExprSequence
///
///   expr-sequence(Mode):
///     expr-sequence-element(Mode) expr-binary(Mode)*
///   expr-binary(Mode):
///     operator-binary expr-sequence-element(Mode)
///     '?' expr-sequence(Mode) ':' expr-sequence-element(Mode)
///     '=' expr-unary
///     expr-is
///     expr-as
///
/// The sequencing for binary exprs is not structural, i.e., binary operators
/// are not inherently right-associative. If present, '?' and ':' tokens must
/// match.
///
/// Similarly, the parsing of 'try' as part of expr-sequence-element
/// is not structural.  'try' is not permitted at arbitrary points in
/// a sequence; in the places it's permitted, it's hoisted out to
/// apply to everything to its right.
ParserResult<Expr> Parser::parseExprSequence(Diag<> Message,
                                             bool isExprBasic,
                                             bool isForConditionalDirective) {
  SyntaxParsingContext ExprSequnceContext(SyntaxContext, SyntaxContextKind::Expr);

  SmallVector<Expr*, 8> SequencedExprs;
  SourceLoc startLoc = Tok.getLoc();
  bool HasCodeCompletion = false;
  bool PendingTernary = false;

  while (true) {
    if (isForConditionalDirective && Tok.isAtStartOfLine())
      break;

    // Parse a unary expression.
    ParserResult<Expr> Primary =
      parseExprSequenceElement(Message, isExprBasic);

    if (Primary.hasCodeCompletion()) {
      HasCodeCompletion = true;
      if (CodeCompletion)
        CodeCompletion->setLeadingSequenceExprs(SequencedExprs);
    }
    if (Primary.isNull())
      return Primary;

    SequencedExprs.push_back(Primary.get());

    // We know we can make a syntax node for ternary expression.
    if (PendingTernary) {
      SyntaxContext->createNodeInPlace(SyntaxKind::TernaryExpr);
      PendingTernary = false;
    }

    if (isForConditionalDirective && Tok.isAtStartOfLine())
      break;
    
parse_operator:
    switch (Tok.getKind()) {
    case tok::oper_binary_spaced:
    case tok::oper_binary_unspaced: {
      // If this is an "&& #available()" expression (or related things that
      // show up in a stmt-condition production), then don't eat it.
      //
      // These are not general expressions, and && is an infix operator,
      // so the code is invalid.  We get better recovery if we bail out from
      // this, because then we can produce a fixit to rewrite the && into a ,
      // if we're in a stmt-condition.
      if (Tok.getText() == "&&" &&
          peekToken().isAny(tok::pound_available,
                            tok::kw_let, tok::kw_var, tok::kw_case))
        goto done;
      
      // Parse the operator.
      SyntaxParsingContext OperatorContext(SyntaxContext,
                                           SyntaxKind::BinaryOperatorExpr);
      Expr *Operator = parseExprOperator();
      SequencedExprs.push_back(Operator);

      // The message is only valid for the first subexpr.
      Message = diag::expected_expr_after_operator;
      break;
    }
    
    case tok::question_infix: {
      // Save the '?'.
      SourceLoc questionLoc = consumeToken();
      
      // Parse the middle expression of the ternary.
      ParserResult<Expr> middle =
          parseExprSequence(diag::expected_expr_after_if_question, isExprBasic);
      ParserStatus Status = middle;
      if (middle.hasCodeCompletion())
        HasCodeCompletion = true;
      if (middle.isNull())
        return nullptr;
      
      // Make sure there's a matching ':' after the middle expr.
      if (!Tok.is(tok::colon)) {
        diagnose(questionLoc, diag::expected_colon_after_if_question);

      Status.setIsParseError();
      return makeParserResult(Status, new (Context) ErrorExpr(
          {startLoc, middle.get()->getSourceRange().End}));
      }
      
      SourceLoc colonLoc = consumeToken();
      
      auto *unresolvedIf
        = new (Context) IfExpr(questionLoc,
                               middle.get(),
                               colonLoc);
      SequencedExprs.push_back(unresolvedIf);
      Message = diag::expected_expr_after_if_colon;

      // Wait for the next expression to make a syntax node for ternary
      // expression.
      PendingTernary = true;
      break;
    }
        
    case tok::equal: {
      // If we're parsing an expression as the body of a refutable var/let
      // pattern, then an assignment doesn't make sense.  In a "if let"
      // statement the equals is the start of the condition, so don't parse it
      // as a binary operator.
      if (InVarOrLetPattern)
        goto done;
      SyntaxParsingContext AssignContext(SyntaxContext,
                                         SyntaxKind::AssignmentExpr);
      SourceLoc equalsLoc = consumeToken();
      auto *assign = new (Context) AssignExpr(equalsLoc);
      SequencedExprs.push_back(assign);
      Message = diag::expected_expr_assignment;
      break;
    }
        
    case tok::kw_is: {
      SyntaxParsingContext IsContext(SyntaxContext, SyntaxKind::IsExpr);
      // Parse a type after the 'is' token instead of an expression.
      ParserResult<Expr> is = parseExprIs();
      if (is.isNull() || is.hasCodeCompletion())
        return is;
      
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(is.get());
      SequencedExprs.push_back(is.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }
        
    case tok::kw_as: {
      SyntaxParsingContext AsContext(SyntaxContext, SyntaxKind::AsExpr);
      ParserResult<Expr> as = parseExprAs();
      if (as.isNull() || as.hasCodeCompletion())
        return as;
        
      // Store the expr itself as a placeholder RHS. The real RHS is the
      // type parameter stored in the node itself.
      SequencedExprs.push_back(as.get());
      SequencedExprs.push_back(as.get());
      
      // We already parsed the right operand as part of the 'is' production.
      // Jump directly to parsing another operator.
      goto parse_operator;
    }

    case tok::arrow:
    case tok::kw_throws: {
      SyntaxParsingContext ArrowContext(SyntaxContext, SyntaxKind::ArrowExpr);
      ParserResult<Expr> arrow = parseExprArrow();
      if (arrow.isNull() || arrow.hasCodeCompletion())
        return arrow;
      SequencedExprs.push_back(arrow.get());
      break;
    }
        
    default:
      // If the next token is not a binary operator, we're done.
      goto done;
    }
  }
done:

  // For conditional directives, we stop parsing after a line break.
  if (isForConditionalDirective && (SequencedExprs.size() & 1) == 0) {
    diagnose(getEndOfPreviousLoc(),
             diag::incomplete_conditional_compilation_directive);
    return makeParserError();
  }

  // If we had semantic errors, just fail here.
  assert(!SequencedExprs.empty());

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1) {
    auto Result = makeParserResult(SequencedExprs[0]);
    if (HasCodeCompletion)
      Result.setHasCodeCompletion();
    return Result;
  }

  ExprSequnceContext.createNodeInPlace(SyntaxKind::ExprList);
  ExprSequnceContext.setCreateSyntax(SyntaxKind::SequenceExpr);
  auto Result = makeParserResult(SequenceExpr::create(Context, SequencedExprs));
  if (HasCodeCompletion)
    Result.setHasCodeCompletion();
  return Result;
}

/// parseExprSequenceElement
///
///   expr-sequence-element(Mode):
///     'try' expr-unary(Mode)
///     'try' '?' expr-unary(Mode)
///     'try' '!' expr-unary(Mode)
///     expr-unary(Mode)
///
/// 'try' is not actually allowed at an arbitrary position of a
/// sequence, but this isn't enforced until sequence-folding.
ParserResult<Expr> Parser::parseExprSequenceElement(Diag<> message,
                                                    bool isExprBasic) {
  SyntaxParsingContext ElementContext(SyntaxContext,
                                      SyntaxContextKind::Expr);
  SourceLoc tryLoc;
  bool hadTry = consumeIf(tok::kw_try, tryLoc);
  Optional<Token> trySuffix;
  if (hadTry && Tok.isAny(tok::exclaim_postfix, tok::question_postfix)) {
    trySuffix = Tok;
    consumeToken();
  }

  // Try to parse '@' sign or 'inout' as a attributed typerepr.
  if (Tok.isAny(tok::at_sign, tok::kw_inout)) {
    bool isType = false;
    {
      BacktrackingScope backtrack(*this);
      isType = canParseType();
    }
    if (isType) {
      ParserResult<TypeRepr> ty = parseType();
      if (ty.isNonNull())
        return makeParserResult(
            new (Context) TypeExpr(TypeLoc(ty.get(), Type())));
      checkForInputIncomplete();
      return nullptr;
    }
  }

  ParserResult<Expr> sub = parseExprUnary(message, isExprBasic);

  if (hadTry && !sub.hasCodeCompletion() && !sub.isNull()) {
    ElementContext.setCreateSyntax(SyntaxKind::TryExpr);
    switch (trySuffix ? trySuffix->getKind() : tok::NUM_TOKENS) {
    case tok::exclaim_postfix:
      sub = makeParserResult(
          new (Context) ForceTryExpr(tryLoc, sub.get(), trySuffix->getLoc()));
      break;
    case tok::question_postfix:
      sub = makeParserResult(
          new (Context) OptionalTryExpr(tryLoc, sub.get(),
                                        trySuffix->getLoc()));
      break;
    default:
      // If this is a simple "try expr" situation, where the expr is a closure
      // literal, and the next token is a 'catch', then the user wrote
      // try/catch instead of do/catch.  Emit a fixit hint to rewrite to the
      // correct do/catch construct.
      if (Tok.is(tok::kw_catch) && isa<ClosureExpr>(sub.get())) {
        diagnose(tryLoc, diag::docatch_not_trycatch)
          .fixItReplace(tryLoc, "do");
        
        // Eat all of the catch clauses, so we don't trip over them in error
        // recovery.
        while (Tok.is(tok::kw_catch)) {
          ParserResult<CatchStmt> clause = parseStmtCatch();
          if (clause.hasCodeCompletion() && clause.isNull())
            break;
        }

        return makeParserResult(new (Context) ErrorExpr(tryLoc));
      }
        
      sub = makeParserResult(new (Context) TryExpr(tryLoc, sub.get()));
      break;
    }
  }

  return sub;
}

static Expr *formUnaryArgument(ASTContext &context, Expr *argument) {
  if (isa<ParenExpr>(argument))
    return argument;

  auto *arg = new (context)
      ParenExpr(argument->getStartLoc(), argument, argument->getEndLoc(),
                /*hasTrailingClosure*/ false);
  arg->setImplicit();
  return arg;
}

/// parseExprUnary
///
///   expr-unary(Mode):
///     expr-postfix(Mode)
///     operator-prefix expr-unary(Mode)
///     '&' expr-unary(Mode)
///
ParserResult<Expr> Parser::parseExprUnary(Diag<> Message, bool isExprBasic) {
  SyntaxParsingContext UnaryContext(SyntaxContext, SyntaxContextKind::Expr);
  UnresolvedDeclRefExpr *Operator;
  switch (Tok.getKind()) {
  default:
    // If the next token is not an operator, just parse this as expr-postfix.
    return parseExprPostfix(Message, isExprBasic);

  case tok::amp_prefix: {
    SyntaxParsingContext AmpCtx(SyntaxContext, SyntaxKind::InOutExpr);
    SourceLoc Loc = consumeToken(tok::amp_prefix);

    ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
    if (SubExpr.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (SubExpr.isNull())
      return nullptr;
    return makeParserResult(
        new (Context) InOutExpr(Loc, SubExpr.get(), Type()));
  }

  case tok::backslash:
    return parseExprKeyPath();

  case tok::oper_postfix:
    // Postfix operators cannot start a subexpression, but can happen
    // syntactically because the operator may just follow whatever precedes this
    // expression (and that may not always be an expression).
    diagnose(Tok, diag::invalid_postfix_operator);
    Tok.setKind(tok::oper_prefix);
    LLVM_FALLTHROUGH;
  case tok::oper_prefix:
    Operator = parseExprOperator();
    break;
  case tok::oper_binary_spaced:
  case tok::oper_binary_unspaced: {
    // For recovery purposes, accept an oper_binary here.
    SourceLoc OperEndLoc = Tok.getLoc().getAdvancedLoc(Tok.getLength());
    Tok.setKind(tok::oper_prefix);
    Operator = parseExprOperator();

    if (OperEndLoc == Tok.getLoc())
      diagnose(PreviousLoc, diag::expected_expr_after_unary_operator);
    else
      diagnose(PreviousLoc, diag::expected_prefix_operator)
          .fixItRemoveChars(OperEndLoc, Tok.getLoc());
    break;
  }
  }

  ParserResult<Expr> SubExpr = parseExprUnary(Message, isExprBasic);
  ParserStatus Status = SubExpr;
  if (SubExpr.isNull())
    return Status;

  // We are sure we can create a prefix prefix operator expr now.
  UnaryContext.setCreateSyntax(SyntaxKind::PrefixOperatorExpr);

  // Check if we have a unary '-' with number literal sub-expression, for
  // example, "-42" or "-1.25".
  if (auto *LE = dyn_cast<NumberLiteralExpr>(SubExpr.get())) {
    if (Operator->hasName() && Operator->getName().getBaseName() == "-") {
      LE->setNegative(Operator->getLoc());
      return makeParserResult(Status, LE);
    }
  }

  return makeParserResult(
      Status, new (Context) PrefixUnaryExpr(
                  Operator, formUnaryArgument(Context, SubExpr.get())));
}

/// expr-keypath-swift:
///   \ type? . initial-key-path-component key-path-components
///
/// key-path-components:
//    key-path-component*
///   <empty>
///
/// key-path-component:
///   .identifier
///   ?
///   !
///   [ expression ]
///
/// initial-key-path-component:
///   identifier
///   ?
///   !
///   [ expression ]
ParserResult<Expr> Parser::parseExprKeyPath() {
  SyntaxParsingContext KeyPathCtx(SyntaxContext, SyntaxKind::KeyPathExpr);
  // Consume '\'.
  SourceLoc backslashLoc = consumeToken(tok::backslash);
  llvm::SaveAndRestore<bool> S(InSwiftKeyPath, true);

  // FIXME: diagnostics
  ParserResult<Expr> rootResult, pathResult;
  if (!startsWithSymbol(Tok, '.')) {
    rootResult = parseExprPostfix(diag::expr_keypath_expected_expr,
                                  /*isBasic=*/true);

    if (rootResult.isParseError())
      return rootResult;
  }

  if (startsWithSymbol(Tok, '.')) {
    SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);

    auto dotLoc = Tok.getLoc();
    // For uniformity, \.foo is parsed as if it were MAGIC.foo, so we need to
    // make sure the . is there, but parsing the ? in \.? as .? doesn't make
    // sense. This is all made more complicated by .?. being considered an
    // operator token. Since keypath allows '.!' '.?' and '.[', consume '.'
    // the token is a operator starts with '.', or the following token is '['.
    if ((Tok.isAnyOperator() && Tok.getLength() != 1) ||
        peekToken().is(tok::l_square)) {
      SyntaxParsingContext KeyPathBaseContext(SyntaxContext,
                                              SyntaxKind::KeyPathBaseExpr);
      consumeStartingCharacterOfCurrentToken(tok::period);
    }

    auto inner = makeParserResult(new (Context) KeyPathDotExpr(dotLoc));
    bool unusedHasBindOptional = false;

    // Inside a keypath's path, the period always behaves normally: the key path
    // behavior is only the separation between type and path.
    pathResult = parseExprPostfixSuffix(inner, /*isExprBasic=*/true,
                                        /*periodHasKeyPathBehavior=*/false,
                                        unusedHasBindOptional);
    if (pathResult.isParseError())
      return pathResult;
  }

  auto keypath = new (Context) KeyPathExpr(
      backslashLoc, rootResult.getPtrOrNull(), pathResult.getPtrOrNull());

  // Handle code completion.
  if ((Tok.is(tok::code_complete) && !Tok.isAtStartOfLine()) ||
      (Tok.is(tok::period) && peekToken().isAny(tok::code_complete))) {
    SourceLoc DotLoc;
    consumeIf(tok::period, DotLoc);
    if (CodeCompletion)
      CodeCompletion->completeExprKeyPath(keypath, DotLoc);
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult(keypath);
  }

  return makeParserResult(keypath);
}

///   expr-keypath-objc:
///     '#keyPath' '(' unqualified-name ('.' unqualified-name) * ')'
///
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprObjcKeyPathSyntax() {
  ParsedObjcKeyPathExprSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Consume '#keyPath'.
  builder.useKeyPath(consumeTokenSyntax(tok::pound_keyPath));

  // Parse the leading '('.
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expr_keypath_expected_lparen);
    return makeParsedError(builder.build());
  }
  auto LParenLoc = Tok.getLoc();
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));

  // Parse the sequence of unqualified-names.
  bool isFirst = true;
  bool hasNext = true;
  do {
    // Parse the next name
    Optional<ParsedTokenSyntax> identTok;
    Optional<ParsedDeclNameArgumentsSyntax> declNameArgs;
    status |= parseUnqualifiedDeclNameSyntax(
        identTok, declNameArgs, /*afterDot=*/!isFirst,
        diag::expr_keypath_expected_property_or_type);
    isFirst = false;
    if (status.isError())
      break;

    ParsedObjcNamePieceSyntaxBuilder elemBuilder(*SyntaxContext);

    elemBuilder.useName(std::move(*identTok));
    if (declNameArgs)
      elemBuilder.useDeclNameArguments(std::move(*declNameArgs));

    hasNext = Tok.is(tok::period);
    if (hasNext)
      elemBuilder.useDot(consumeTokenSyntax(tok::period));
    builder.addNameMember(elemBuilder.build());
  } while (hasNext);

  if (Tok.is(tok::code_complete)) {
    return makeParsedCodeCompletion(
        ParsedSyntaxRecorder::makeCodeCompletionExpr(
            builder.build(), None, consumeTokenSyntax(tok::code_complete),
            *SyntaxContext));
  }

  if (status.isError()) {
    while (!Tok.isAny(tok::r_paren, tok::eof, tok::r_brace, tok::pound_endif,
                      tok::pound_else, tok::pound_elseif) &&
           !isStartOfDecl() && !isStartOfStmt())
      ignoreSingle();
  }

  // Parse the closing ')'.
  auto RParen =
      parseMatchingTokenSyntax(tok::r_paren, diag::expr_keypath_expected_rparen,
                               LParenLoc, /*silenceDiag=*/status.isError());
  status |= RParen.getStatus();
  if (!RParen.isNull())
    builder.useRightParen(RParen.get());

  return makeParsedResult(builder.build(), status);
}

/// parseExprSelector
///
///   expr-selector:
///     '#selector' '(' expr ')'
///     '#selector' '(' 'getter' ':' expr ')'
///     '#selector' '(' 'setter' ':' expr ')'
///
ParserResult<Expr> Parser::parseExprSelector() {
  SyntaxParsingContext ExprCtxt(SyntaxContext, SyntaxKind::ObjcSelectorExpr);
  // Consume '#selector'.
  SourceLoc keywordLoc = consumeToken(tok::pound_selector);

  // Parse the leading '('.
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expr_selector_expected_lparen);
    return makeParserError();
  }
  SourceLoc lParenLoc = consumeToken(tok::l_paren);
  SourceLoc modifierLoc;

  // Parse possible 'getter:' or 'setter:' modifiers, and determine
  // the kind of selector we're working with.
  ObjCSelectorExpr::ObjCSelectorKind selectorKind;
  if (peekToken().is(tok::colon) &&
      (Tok.isContextualKeyword("getter") ||
       Tok.isContextualKeyword("setter"))) {
    // Parse the modifier.
    if (Tok.isContextualKeyword("getter"))
      selectorKind = ObjCSelectorExpr::Getter;
    else
      selectorKind = ObjCSelectorExpr::Setter;

    Tok.setKind(tok::contextual_keyword);
    modifierLoc = consumeToken();
    (void)consumeToken(tok::colon);
  } else {
    selectorKind = ObjCSelectorExpr::Method;
  }

  ObjCSelectorContext selectorContext;
  switch (selectorKind) {
  case ObjCSelectorExpr::Getter:
    selectorContext = ObjCSelectorContext::GetterSelector;
    break;
  case ObjCSelectorExpr::Setter:
    selectorContext = ObjCSelectorContext::SetterSelector;
    break;
  case ObjCSelectorExpr::Method:
    selectorContext = ObjCSelectorContext::MethodSelector;
  }

  // Parse the subexpression.
  CodeCompletionCallbacks::InObjCSelectorExprRAII
    InObjCSelectorExpr(CodeCompletion, selectorContext);
  ParserResult<Expr> subExpr =
    parseExpr(selectorKind == ObjCSelectorExpr::Method
                ? diag::expr_selector_expected_method_expr
                : diag::expr_selector_expected_property_expr);
  if (subExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Expr>();

  // Parse the closing ')'.
  SourceLoc rParenLoc;
  if (subExpr.isParseError()) {
    skipUntilDeclStmtRBrace(tok::r_paren);
    if (Tok.is(tok::r_paren))
      rParenLoc = consumeToken();
    else
      rParenLoc = PreviousLoc;
  } else {
    parseMatchingToken(tok::r_paren, rParenLoc,
                       diag::expr_selector_expected_rparen, lParenLoc);
  }

  // If the subexpression was in error, just propagate the error.
  if (subExpr.isParseError())
    return makeParserResult<Expr>(
      new (Context) ErrorExpr(SourceRange(keywordLoc, rParenLoc)));

  return makeParserResult<Expr>(
    new (Context) ObjCSelectorExpr(selectorKind, keywordLoc, lParenLoc,
                                   modifierLoc, subExpr.get(), rParenLoc));
}

ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprObjcSelectorSyntax() {
  SyntaxParsingContext TmpContext(SyntaxContext);
  TmpContext.setTransparent();

  SourceLoc ExprLoc = Tok.getLoc();
  ParserResult<Expr> Result;
  {
    SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);
    Result = parseExprSelector();
  }
  if (auto ParsedExpr = TmpContext.popIf<ParsedExprSyntax>()) {
    Generator.addExpr(Result.getPtrOrNull(), ExprLoc);
    return makeParsedResult(std::move(*ParsedExpr), Result.getStatus());
  }
  return Result.getStatus();
}

static DeclRefKind getDeclRefKindForOperator(tok kind) {
  switch (kind) {
  case tok::oper_binary_spaced:
  case tok::oper_binary_unspaced:  return DeclRefKind::BinaryOperator;
  case tok::oper_postfix: return DeclRefKind::PostfixOperator;
  case tok::oper_prefix:  return DeclRefKind::PrefixOperator;
  default: llvm_unreachable("bad operator token kind");
  }
}

/// parseExprOperator - Parse an operator reference expression.  These
/// are not "proper" expressions; they can only appear in binary/unary
/// operators.
UnresolvedDeclRefExpr *Parser::parseExprOperator() {
  assert(Tok.isAnyOperator());
  DeclRefKind refKind = getDeclRefKindForOperator(Tok.getKind());
  SourceLoc loc = Tok.getLoc();
  Identifier name = Context.getIdentifier(Tok.getText());
  consumeToken();
  // Bypass local lookup.
  return new (Context) UnresolvedDeclRefExpr(name, refKind, DeclNameLoc(loc));
}

/// parseExprSuper
///
///   expr-super:
///     expr-super-member
///     expr-super-init
///     expr-super-subscript
///   expr-super-member:
///     'super' '.' identifier
///   expr-super-init:
///     'super' '.' 'init'
///   expr-super-subscript:
///     'super' '[' expr ']'
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprSuperSyntax() {
  auto superTok = consumeTokenSyntax(tok::kw_super);

  // 'super.' must be followed by a member ref, explicit initializer ref, or
  // subscript call.
  if (!Tok.isAny(tok::period, tok::period_prefix, tok::code_complete) &&
      !Tok.isFollowingLSquare()) {
    SmallVector<ParsedSyntax, 2> junk;
    junk.emplace_back(std::move(superTok));
    if (auto unknown = consumeTokenSyntaxIf(tok::unknown)) {
      junk.emplace_back(std::move(*unknown));
    } else {
      diagnose(Tok, diag::expected_dot_or_subscript_after_super);
    }

    return makeParsedError(
        ParsedSyntaxRecorder::makeUnknownExpr(junk, *SyntaxContext));
  }

  return makeParsedResult(ParsedSyntaxRecorder::makeSuperRefExpr(
      std::move(superTok), *SyntaxContext));
}

StringRef Parser::copyAndStripUnderscores(StringRef orig) {
  return ASTGen::copyAndStripUnderscores(orig, Context);
}

/// Disambiguate the parse after '{' token that is in a place that might be
/// the start of a trailing closure, or start the variable accessor block.
///
/// Check to see if the '{' is followed by a 'didSet' or a 'willSet' label,
/// possibly preceded by attributes.  If so, we disambiguate the parse as the
/// start of a get-set block in a variable definition (not as a trailing
/// closure).
static bool isStartOfGetSetAccessor(Parser &P) {
  assert(P.Tok.is(tok::l_brace) && "not checking a brace?");
  
  // The only case this can happen is if the accessor label is immediately after
  // a brace (possibly preceded by attributes).  "get" is implicit, so it can't
  // be checked for.  Conveniently however, get/set properties are not allowed
  // to have initializers, so we don't have an ambiguity, we just have to check
  // for observing accessors.
  //
  // If we have a 'didSet' or a 'willSet' label, disambiguate immediately as
  // an accessor block.
  Token NextToken = P.peekToken();
  if (NextToken.isContextualKeyword("didSet") ||
      NextToken.isContextualKeyword("willSet"))
    return true;

  // If we don't have attributes, then it cannot be an accessor block.
  if (NextToken.isNot(tok::at_sign))
    return false;

  Parser::BacktrackingScope Backtrack(P);

  // Eat the "{".
  P.consumeToken(tok::l_brace);

  // Eat attributes, if present.
  while (P.consumeIf(tok::at_sign)) {
    if (!P.consumeIf(tok::identifier)) return false;
    // Eat paren after attribute name; e.g. @foo(x)
    if (P.Tok.is(tok::l_paren)) P.skipSingle();
  }

  // Check if we have 'didSet'/'willSet' after attributes.
  return P.Tok.isContextualKeyword("didSet") ||
         P.Tok.isContextualKeyword("willSet");
}

/// Recover invalid uses of trailing closures in a situation
/// where the parser requires an expr-basic (which does not allow them).  We
/// handle this by doing some lookahead in common situations. And later, Sema
/// will emit a diagnostic with a fixit to add wrapping parens.
static bool isValidTrailingClosure(bool isExprBasic, Parser &P){
  assert(P.Tok.is(tok::l_brace) && "Couldn't be a trailing closure");
  
  // If this is the start of a get/set accessor, then it isn't a trailing
  // closure.
  if (isStartOfGetSetAccessor(P))
    return false;

  // If this is a normal expression (not an expr-basic) then trailing closures
  // are allowed, so this is obviously one.
  // TODO: We could handle try to disambiguate cases like:
  //   let x = foo
  //   {...}()
  // by looking ahead for the ()'s, but this has been replaced by do{}, so this
  // probably isn't worthwhile.
  //
  if (!isExprBasic)
    return true;
  
  // If this is an expr-basic, then a trailing closure is not allowed.  However,
  // it is very common for someone to write something like:
  //
  //    for _ in numbers.filter {$0 > 4} {
  //
  // and we want to recover from this very well.   We need to perform arbitrary
  // look-ahead to disambiguate this case, so we only do this in the case where
  // the token after the { is on the same line as the {.
  if (P.peekToken().isAtStartOfLine())
    return false;
  
  
  // Determine if the {} goes with the expression by eating it, and looking
  // to see if it is immediately followed by '{', 'where', or comma.  If so,
  // we consider it to be part of the proceeding expression.
  Parser::BacktrackingScope backtrack(P);
  P.consumeToken(tok::l_brace);
  P.skipUntil(tok::r_brace);
  SourceLoc endLoc;
  if (!P.consumeIf(tok::r_brace, endLoc) ||
      P.Tok.isNot(tok::l_brace, tok::kw_where, tok::comma)) {
    return false;
  }

  // Recoverable case. Just return true here and Sema will emit a diagnostic
  // later. see: Sema/MiscDiagnostics.cpp#checkStmtConditionTrailingClosure
  return true;
}

ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprUnresolvedMemberSyntax(bool isExprBasic) {
  assert(Tok.isAny(tok::period, tok::period_prefix));

  // Parse '.'
  Tok.setKind(tok::period_prefix);
  auto dotTok = consumeTokenSyntax(tok::period_prefix);

  // Handle code completion; '.' <cc-token>
  if (Tok.is(tok::code_complete)) {
    ParsedCodeCompletionExprSyntaxBuilder ccBuilder(*SyntaxContext);
    ccBuilder.usePeriodOrParen(std::move(dotTok));
    ccBuilder.useCodeCompletionToken(consumeTokenSyntax(tok::code_complete));
    return makeParsedCodeCompletion(ccBuilder.build());
  }

  ParserStatus status;

  // Parse the name.
  Optional<ParsedTokenSyntax> identTok;
  Optional<ParsedDeclNameArgumentsSyntax> declNameArgs;
  status |=
      parseUnqualifiedDeclNameSyntax(identTok, declNameArgs, /*afterDot=*/true,
                                     diag::expected_identifier_after_dot_expr);
  if (status.isError()) {
    // If the name is missing. It makes no sense to construct a member access
    // expression.
    assert(!identTok && !declNameArgs);
    return makeParsedError(
        ParsedSyntaxRecorder::makeUnknownExpr({&dotTok, 1}, *SyntaxContext));
  }

  ParsedMemberAccessExprSyntaxBuilder builder(*SyntaxContext);
  builder.useDot(std::move(dotTok));
  builder.useName(std::move(*identTok));
  if (declNameArgs)
    builder.useDeclNameArguments(std::move(*declNameArgs));

  // FIXME: These calling suffix parsings are not necessary for Syntax parsing.
  // Remove this block after full expression parsing migration.

  // Check for a () suffix, which indicates a call when constructing
  // this member.  Note that this cannot be the start of a new line.
  if (Tok.isFollowingLParen()) {
    ParsedFunctionCallExprSyntaxBuilder callBuilder(*SyntaxContext);
    callBuilder.useCalledExpression(builder.build());

    status |= parseExprListSyntax(
        tok::l_paren, tok::r_paren,
        /*isPostfix=*/true, isExprBasic,
        [&](ParsedTokenSyntax &&leftTok,
            ParsedTupleExprElementListSyntax &&args,
            Optional<ParsedTokenSyntax> &&rightTok,
            Optional<ParsedClosureExprSyntax> &&closure) {
          callBuilder.useLeftParen(std::move(leftTok));
          callBuilder.useArgumentList(std::move(args));
          if (rightTok)
            callBuilder.useRightParen(std::move(*rightTok));
          if (closure)
            callBuilder.useTrailingClosure(std::move(*closure));
        });
    return makeParsedResult(callBuilder.build(), status);
  }

  // Check for a trailing closure, if allowed.
  if (Tok.is(tok::l_brace) && isValidTrailingClosure(isExprBasic, *this)) {
    ParsedFunctionCallExprSyntaxBuilder callBuilder(*SyntaxContext);
    callBuilder.useCalledExpression(builder.build());

    auto closure = parseTrailingClosureSyntax({PreviousLoc, PreviousLoc});
    status |= closure.getStatus();
    assert(!closure.isNull());
    callBuilder.useTrailingClosure(closure.get());

    return makeParsedResult(callBuilder.build(), status);
  }

  return makeParsedResult(builder.build(), status);
}

ParserResult<Expr>
Parser::parseExprPostfixSuffix(ParserResult<Expr> Result, bool isExprBasic,
                               bool periodHasKeyPathBehavior,
                               bool &hasBindOptional) {
  hasBindOptional = false;

  // Handle suffix expressions.
  while (1) {
    // FIXME: Better recovery.
    if (Result.isNull())
      return Result;

    if (Result.hasCodeCompletion() &&
        SourceMgr.getCodeCompletionLoc() == PreviousLoc) {
      // Don't parse suffixes if the expression ended with code completion
      // token. Because, for example, given:
      //   [.foo(), .bar()]
      // If user want to insert another element in between:
      //   [.foo(), <HERE> .bar()]
      // '.bar()' is probably not a part of the inserting element. Moreover,
      // having suffixes doesn't help type inference in any way.
      return Result;
    }

    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    if (Tok.is(tok::period) || Tok.is(tok::period_prefix)) {
      // A key path is special, because it allows .[, unlike anywhere else. The
      // period itself should be left in the token stream. (.? and .! end up
      // being operators, and so aren't handled here.)
      if (periodHasKeyPathBehavior && peekToken().is(tok::l_square)) {
        break;
      }
      // Completion for keyPath expression is handled in parseExprKeyPath.
      if (InSwiftKeyPath && peekToken().is(tok::code_complete))
        break;

      Tok.setKind(tok::period);
      consumeToken();

      // Handle "x.42" - a tuple index.
      if (Tok.is(tok::integer_literal)) {
        DeclName name = Context.getIdentifier(Tok.getText());
        SourceLoc nameLoc = consumeToken(tok::integer_literal);
        SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);

        // Don't allow '.<integer literal>' following a numeric literal
        // expression (unless in #if env, for 1.2.3.4 version numbers)
        if (!InPoundIfEnvironment && Result.isNonNull() &&
            isa<NumberLiteralExpr>(Result.get())) {
          diagnose(nameLoc, diag::numeric_literal_numeric_member)
              .highlight(Result.get()->getSourceRange());
          continue;
        }

        Result = makeParserResult(
            Result, new (Context) UnresolvedDotExpr(Result.get(), TokLoc, name,
                                                    DeclNameLoc(nameLoc),
                                                    /*Implicit=*/false));
        continue;
      }

      // Handle "x.self" expr.
      if (Tok.is(tok::kw_self)) {
        Result = makeParserResult(
            Result,
            new (Context) DotSelfExpr(Result.get(), TokLoc, consumeToken()));
        SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);
        continue;
      }

      // Handle the deprecated 'x.dynamicType' and migrate it to `type(of: x)`
      if (Tok.getText() == "dynamicType") {
        auto range = Result.get()->getSourceRange();
        auto dynamicTypeExprRange = SourceRange(TokLoc, Tok.getLoc());
        diagnose(TokLoc, diag::expr_dynamictype_deprecated)
            .highlight(dynamicTypeExprRange)
            .fixItReplace(dynamicTypeExprRange, ")")
            .fixItInsert(range.Start, "type(of: ");

        // fallthrough to an UnresolvedDotExpr.
      }

      // Handle "x.<tab>" for code completion.
      if (Tok.is(tok::code_complete)) {
        assert(!InSwiftKeyPath);
        if (CodeCompletion) {
          CodeCompletion->completeDotExpr(Result.get(), /*DotLoc=*/TokLoc);
        }
        // Eat the code completion token because we handled it.
        consumeToken(tok::code_complete);
        Result.setHasCodeCompletion();
        return Result;
      }

      DeclNameLoc NameLoc;
      Diag<> D = isa<SuperRefExpr>(Result.get())
                     ? diag::expected_identifier_after_super_dot_expr
                     : diag::expected_member_name;
      DeclName Name = parseUnqualifiedDeclName(/*afterDot=*/true, NameLoc, D);
      if (!Name)
        return nullptr;
      SyntaxContext->createNodeInPlace(SyntaxKind::MemberAccessExpr);
      Result = makeParserResult(Result, new (Context) UnresolvedDotExpr(
                                            Result.get(), TokLoc, Name, NameLoc,
                                            /*Implicit=*/false));

      if (canParseAsGenericArgumentList()) {
        SmallVector<TypeRepr *, 8> args;
        SourceLoc LAngleLoc, RAngleLoc;
        auto argStat = parseGenericArguments(args, LAngleLoc, RAngleLoc);
        if (argStat.isError())
          diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);

        SmallVector<TypeLoc, 8> locArgs;
        for (auto ty : args)
          locArgs.push_back(ty);
        SyntaxContext->createNodeInPlace(SyntaxKind::SpecializeExpr);
        Result = makeParserResult(
            Result, UnresolvedSpecializeExpr::create(
                        Context, Result.get(), LAngleLoc, locArgs, RAngleLoc));
      }

      continue;
    }

    // If there is an expr-call-suffix, parse it and form a call.
    if (Tok.isFollowingLParen()) {
      Result = parseExprCallSuffix(Result, isExprBasic);
      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);
      continue;
    }

    // Check for a [expr] suffix.
    // Note that this cannot be the start of a new line.
    if (Tok.isFollowingLSquare()) {
      SourceLoc lSquareLoc, rSquareLoc;
      SmallVector<Expr *, 2> indexArgs;
      SmallVector<Identifier, 2> indexArgLabels;
      SmallVector<SourceLoc, 2> indexArgLabelLocs;
      Expr *trailingClosure;

      ParserStatus status = parseExprList(
          tok::l_square, tok::r_square,
          /*isPostfix=*/true, isExprBasic, lSquareLoc, indexArgs,
          indexArgLabels, indexArgLabelLocs, rSquareLoc, trailingClosure);
      Result = makeParserResult(
          status | Result,
          SubscriptExpr::create(Context, Result.get(), lSquareLoc, indexArgs,
                                indexArgLabels, indexArgLabelLocs, rSquareLoc,
                                trailingClosure, ConcreteDeclRef(),
                                /*implicit=*/false));
      SyntaxContext->createNodeInPlace(SyntaxKind::SubscriptExpr);
      continue;
    }

    // Check for a trailing closure, if allowed.
    if (Tok.is(tok::l_brace) && isValidTrailingClosure(isExprBasic, *this)) {
      // FIXME: if Result has a trailing closure, break out.

      // Stop after literal expressions, which may never have trailing closures.
      const auto *callee = Result.get();
      if (isa<LiteralExpr>(callee) || isa<CollectionExpr>(callee) ||
          isa<TupleExpr>(callee))
        break;

      // Add dummy blank argument list to the call expression syntax.
      SyntaxContext->addSyntax(
          ParsedSyntaxRecorder::makeBlankTupleExprElementList(
              Tok.getLoc(), *SyntaxContext));

      ParserResult<Expr> closure =
          parseTrailingClosure(callee->getSourceRange());
      if (closure.isNull())
        return nullptr;

      // Trailing closure implicitly forms a call.
      Result = makeParserResult(
          ParserStatus(closure) | ParserStatus(Result),
          CallExpr::create(Context, Result.get(), SourceLoc(), {}, {}, {},
                           SourceLoc(), closure.get(), /*implicit=*/false));
      SyntaxContext->createNodeInPlace(SyntaxKind::FunctionCallExpr);

      // We only allow a single trailing closure on a call.  This could be
      // generalized in the future, but needs further design.
      if (Tok.is(tok::l_brace))
        break;
      continue;
    }

    // Check for a ? suffix.
    if (consumeIf(tok::question_postfix)) {
      Result = makeParserResult(Result, new (Context) BindOptionalExpr(
                                            Result.get(), TokLoc, /*depth*/ 0));
      SyntaxContext->createNodeInPlace(SyntaxKind::OptionalChainingExpr);
      hasBindOptional = true;
      continue;
    }

    // Check for a ! suffix.
    if (consumeIf(tok::exclaim_postfix)) {
      Result = makeParserResult(
          Result, new (Context) ForceValueExpr(Result.get(), TokLoc));
      SyntaxContext->createNodeInPlace(SyntaxKind::ForcedValueExpr);
      continue;
    }

    // Check for a postfix-operator suffix.
    if (Tok.is(tok::oper_postfix)) {
      // KeyPaths are more restricted in what can go after a ., and so we treat
      // them specially.
      if (periodHasKeyPathBehavior && startsWithSymbol(Tok, '.'))
        break;

      Expr *oper = parseExprOperator();

      Result = makeParserResult(
          Result, new (Context) PostfixUnaryExpr(
                      oper, formUnaryArgument(Context, Result.get())));
      SyntaxContext->createNodeInPlace(SyntaxKind::PostfixUnaryExpr);
      continue;
    }

    if (Tok.is(tok::code_complete)) {
      if (InSwiftKeyPath)
        return Result;

      if (Tok.isAtStartOfLine()) {
        // Postfix expression is located on a different line than the code
        // completion token, and thus they are not related.
        return Result;
      }

      if (CodeCompletion && Result.isNonNull()) {
        bool hasSpace = Tok.getLoc() != getEndOfPreviousLoc();
        CodeCompletion->completePostfixExpr(Result.get(), hasSpace);
      }
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      Result.setHasCodeCompletion();
      return Result;
    }

    // If we end up with an unknown token on this line, return an ErrorExpr
    // covering the range of the token.
    if (!Tok.isAtStartOfLine() && consumeIf(tok::unknown)) {
      Result = makeParserResult(
          Result, new (Context) ErrorExpr(Result.get()->getSourceRange()));
      continue;
    }

    // Otherwise, we don't know what this token is, it must end the expression.
    break;
  }

  return Result;
}

/// parseExprPostfix
///
///   expr-dot:
///     expr-postfix '.' 'type'
///     expr-postfix '.' (identifier|keyword) generic-args? expr-call-suffix?
///     expr-postfix '.' integer_literal
///
///   expr-subscript:
///     expr-postfix '[' expr ']'
///
///   expr-call:
///     expr-postfix expr-paren
///
///   expr-force-value:
///     expr-postfix '!'
///
///   expr-trailing-closure:
///     expr-postfix(trailing-closure) expr-closure
///
///   expr-postfix(Mode):
///     expr-postfix(Mode) operator-postfix
///
///   expr-postfix(basic):
///     expr-primary
///     expr-dot
///     expr-metatype
///     expr-init
///     expr-subscript
///     expr-call
///     expr-force-value
///
///   expr-postfix(trailing-closure):
///     expr-postfix(basic)
///     expr-trailing-closure
///
ParserResult<Expr> Parser::parseExprPostfix(Diag<> ID, bool isExprBasic) {
  SyntaxParsingContext ExprContext(SyntaxContext, SyntaxContextKind::Expr);
  auto Result = parseExprPrimary(ID, isExprBasic);
  // If we couldn't parse any expr, don't attempt to parse suffixes.
  if (Result.isNull())
    return Result;

  bool hasBindOptional = false;
  Result = parseExprPostfixSuffix(Result, isExprBasic,
                                  /*periodHasKeyPathBehavior=*/InSwiftKeyPath,
                                  hasBindOptional);
  if (Result.isParseError() || Result.hasCodeCompletion())
    return Result;

  // If we had a ? suffix expression, bind the entire postfix chain
  // within an OptionalEvaluationExpr.
  if (hasBindOptional) {
    Result = makeParserResult(new (Context) OptionalEvaluationExpr(Result.get()));
  }

  return Result;
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprPoundFileSyntax() {
  if (Tok.getKind() == tok::kw___FILE__) {
    StringRef fixit = "#file";
    diagnose(Tok.getLoc(), diag::snake_case_deprecated, Tok.getText(), fixit)
        .fixItReplace(Tok.getLoc(), fixit);

    auto Token = consumeTokenSyntax(tok::kw___FILE__);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownExpr({&Token, 1}, *SyntaxContext));
  }

  auto Token = consumeTokenSyntax(tok::pound_file);
  return makeParsedResult(ParsedSyntaxRecorder::makePoundFileExpr(
      std::move(Token), *SyntaxContext));
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprPoundLineSyntax() {
  if (Tok.getKind() == tok::kw___LINE__) {
    StringRef fixit = "#line";
    diagnose(Tok.getLoc(), diag::snake_case_deprecated, Tok.getText(), fixit)
        .fixItReplace(Tok.getLoc(), fixit);

    auto Token = consumeTokenSyntax(tok::kw___LINE__);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownExpr({&Token, 1}, *SyntaxContext));
  }

  // FIXME: #line was renamed to #sourceLocation
  auto Token = consumeTokenSyntax(tok::pound_line);
  return makeParsedResult(ParsedSyntaxRecorder::makePoundLineExpr(
      std::move(Token), *SyntaxContext));
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprPoundColumnSyntax() {
  if (Tok.getKind() == tok::kw___COLUMN__) {
    StringRef fixit = "#column";
    diagnose(Tok.getLoc(), diag::snake_case_deprecated, Tok.getText(), fixit)
        .fixItReplace(Tok.getLoc(), fixit);

    auto Token = consumeTokenSyntax(tok::kw___COLUMN__);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownExpr({&Token, 1}, *SyntaxContext));
  }

  auto Token = consumeTokenSyntax(tok::pound_column);
  return makeParsedResult(ParsedSyntaxRecorder::makePoundColumnExpr(
      std::move(Token), *SyntaxContext));
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprPoundFunctionSyntax() {
  if (Tok.getKind() == tok::kw___FUNCTION__) {
    StringRef fixit = "#function";
    diagnose(Tok.getLoc(), diag::snake_case_deprecated, Tok.getText(), fixit)
        .fixItReplace(Tok.getLoc(), fixit);

    auto Token = consumeTokenSyntax(tok::kw___FUNCTION__);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownExpr({&Token, 1}, *SyntaxContext));
  }

  auto Token = consumeTokenSyntax(tok::pound_function);
  return makeParsedResult(ParsedSyntaxRecorder::makePoundFunctionExpr(
      std::move(Token), *SyntaxContext));
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprPoundDsohandleSyntax() {
  if (Tok.getKind() == tok::kw___DSO_HANDLE__) {
    StringRef fixit = "#dsohandle";
    diagnose(Tok.getLoc(), diag::snake_case_deprecated, Tok.getText(), fixit)
        .fixItReplace(Tok.getLoc(), fixit);

    auto Token = consumeTokenSyntax(tok::kw___DSO_HANDLE__);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownExpr({&Token, 1}, *SyntaxContext));
  }

  auto Token = consumeTokenSyntax(tok::pound_dsohandle);
  return makeParsedResult(ParsedSyntaxRecorder::makePoundDsohandleExpr(
      std::move(Token), *SyntaxContext));
}

/// parseExprPrimary
///
///   expr-literal:
///     integer_literal
///     floating_literal
///     string_literal
///     nil
///     true
///     false
///     #file
///     #line
///     #column
///     #function
///     #dsohandle
///
///   expr-delayed-identifier:
///     '.' identifier
///
///   expr-discard:
///     '_'
///
///   expr-primary:
///     expr-literal
///     expr-identifier expr-call-suffix?
///     expr-closure
///     expr-anon-closure-argument
///     expr-delayed-identifier
///     expr-paren
///     expr-super
///     expr-discard
///     expr-selector
///
ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprPrimarySyntax(Diag<> ErrorID, bool isExprBasic) {
  switch (Tok.getKind()) {
  case tok::integer_literal: // 12
    return makeParsedResult(ParsedSyntaxRecorder::makeIntegerLiteralExpr(
        consumeTokenSyntax(tok::integer_literal), *SyntaxContext));

  case tok::floating_literal: // 12.34
    return makeParsedResult(ParsedSyntaxRecorder::makeFloatLiteralExpr(
        consumeTokenSyntax(tok::floating_literal), *SyntaxContext));

  case tok::kw_nil: // nil
    return makeParsedResult(ParsedSyntaxRecorder::makeNilLiteralExpr(
        consumeTokenSyntax(tok::kw_nil), *SyntaxContext));

  case tok::kw_true:  // true
  case tok::kw_false: // false
    return makeParsedResult(ParsedSyntaxRecorder::makeBooleanLiteralExpr(
        consumeTokenSyntax(), *SyntaxContext));

  case tok::kw___FILE__: // __FILE__
  case tok::pound_file:  // #file
    return parseExprPoundFileSyntax();

  case tok::kw___LINE__: // __LINE__
  case tok::pound_line:  // #line
    return parseExprPoundLineSyntax();

  case tok::kw___COLUMN__: // __COLUMN__
  case tok::pound_column:  // #column
    return parseExprPoundColumnSyntax();

  case tok::kw___FUNCTION__: // __FUNCTION__
  case tok::pound_function:  // #function
    return parseExprPoundFunctionSyntax();

  case tok::kw___DSO_HANDLE__: // __DSO_HANDLE__
  case tok::pound_dsohandle:   // #dsohanlde
    return parseExprPoundDsohandleSyntax();

  case tok::at_sign:
    // Objective-C programmers habitually type @"foo", so recover gracefully
    // with a fixit.  If this isn't @"foo", just handle it like an unknown
    // input.
    if (peekToken().isNot(tok::string_literal))
      goto UnknownCharacter;

    diagnose(Tok.getLoc(), diag::string_literal_no_atsign)
        .fixItRemove(Tok.getLoc());
    ignoreToken(tok::at_sign);
    LLVM_FALLTHROUGH;

  case tok::string_literal: // "foo"
    return parseExprStringLiteralSyntax();

  case tok::identifier: // foo
  case tok::kw_self:    // self

    // If we are parsing a refutable pattern and are inside a let/var pattern,
    // the identifiers change to be value bindings instead of decl references.
    // Parse and return this as an UnresolvedPatternExpr around a binding.  This
    // will be resolved (or rejected) by sema when the overall refutable pattern
    // it transformed from an expression into a pattern.
    if ((InVarOrLetPattern == IVOLP_ImplicitlyImmutable ||
         InVarOrLetPattern == IVOLP_InVar ||
         InVarOrLetPattern == IVOLP_InLet) &&
        // If we have "case let x." or "case let x(", we parse x as a normal
        // name, not a binding, because it is the start of an enum pattern or
        // call pattern.
        peekToken().isNot(tok::period, tok::period_prefix, tok::l_paren)) {

      // Defer so that the pattern parser can extract the pattern from the expr.
      DeferringContextRAII defering(*SyntaxContext);

      auto tok = consumeIdentifierSyntax(/*allowDollarIdentifier=*/true);
      auto pat = ParsedSyntaxRecorder::makeIdentifierPattern(std::move(tok),
                                                             *SyntaxContext);
      auto expr = ParsedSyntaxRecorder::makeUnresolvedPatternExpr(
          std::move(pat), *SyntaxContext);
      return makeParsedResult(std::move(expr));
    }
    return parseExprIdentifierSyntax();

  case tok::kw_Self: // Self
    return parseExprIdentifierSyntax();

  case tok::kw_Any: // Any
    return makeParsedResult(ParsedSyntaxRecorder::makeTypeExpr(
        parseAnyType().get(), *SyntaxContext));

  case tok::dollarident: // $1
    return makeParsedResult(ParsedSyntaxRecorder::makeIdentifierExpr(
        consumeTokenSyntax(tok::dollarident), None, *SyntaxContext));

  case tok::kw__: // _
    return makeParsedResult(ParsedSyntaxRecorder::makeDiscardAssignmentExpr(
        consumeTokenSyntax(tok::kw__), *SyntaxContext));

  case tok::pound_selector: // expr-selector
    return parseExprObjcSelectorSyntax();

  case tok::pound_keyPath:
    return parseExprObjcKeyPathSyntax();

  case tok::l_brace: // expr-closure
    return parseExprClosureSyntax();

  case tok::period:          //=.foo
  case tok::period_prefix: { // .foo
    // Special case ".<integer_literal>" like ".4".  This isn't valid, but the
    // developer almost certainly meant to use "0.4".  Diagnose this, and
    // Parse it as a floating number literal.
    if (peekToken().is(tok::integer_literal) &&
        !peekToken().isAtStartOfLine()) {
      SourceLoc dotLoc = Tok.getLoc();
      ignoreToken(); // period or period_prefix.

      diagnose(dotLoc, diag::invalid_float_literal_missing_leading_zero,
               Tok.getText())
          .fixItInsert(dotLoc, "0")
          .highlight({dotLoc, Tok.getLoc()});

      // Consume it as floating literal expression.
      Tok.setKind(tok::floating_literal);
      return makeParsedResult(ParsedSyntaxRecorder::makeFloatLiteralExpr(
          consumeTokenSyntax(), *SyntaxContext));
    }

    return parseExprUnresolvedMemberSyntax(isExprBasic);
  }

  case tok::kw_super: // 'super'
    return parseExprSuperSyntax();

  case tok::l_paren:
    return parseExprTupleSyntax();

  case tok::l_square:
    return parseExprCollectionSyntax();

  case tok::pound_available: {
    // For better error recovery, parse but reject #available in an expr
    // context.
    diagnose(Tok.getLoc(), diag::availability_query_outside_if_stmt_guard);
    auto loc = Tok.getLoc();
    ParserResult<PoundAvailableInfo> res;
    {
      SyntaxParsingContext tmpContext(SyntaxContext, SyntaxContextKind::Expr);
      res = parseStmtConditionPoundAvailable();
    }
    if (res.isNonNull()) {
      auto expr = new (Context) ErrorExpr(res.get()->getSourceRange());
      Generator.addExpr(expr, loc);
    }
    return makeParsedResult(*SyntaxContext->popIf<ParsedExprSyntax>(),
                            res.getStatus());
  }

#define POUND_OBJECT_LITERAL(Name, Desc, Proto)                                \
  case tok::pound_##Name:
#include "swift/Syntax/TokenKinds.def"
    return parseExprObjectLiteralSyntax(isExprBasic);

  case tok::code_complete:
    return makeParsedCodeCompletion(
        ParsedSyntaxRecorder::makeCodeCompletionExpr(
            None, None, consumeTokenSyntax(tok::code_complete),
            *SyntaxContext));

  case tok::pound:
    if (peekToken().is(tok::identifier) && !peekToken().isEscapedIdentifier() &&
        Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
      return parseExprPoundUnknownSyntax(None, SourceLoc());
    }
    if (peekToken().is(tok::code_complete) &&
        Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
      return makeParsedCodeCompletion(
          ParsedSyntaxRecorder::makeCodeCompletionExpr(
              None, consumeTokenSyntax(tok::pound),
              consumeTokenSyntax(tok::code_complete), *SyntaxContext));
    }
    goto UnknownCharacter;

  // Eat an invalid token in an expression context.  Error tokens are diagnosed
  // by the lexer, so there is no reason to emit another diagnostic.
  case tok::unknown: {
    if (Tok.getText().startswith("\"\"\"")) {
      // This was due to unterminated multi-line string.
      IsInputIncomplete = true;
    }
    auto tok = consumeTokenSyntax(tok::unknown);
    return makeParsedError(
        ParsedSyntaxRecorder::makeUnknownExpr({&tok, 1}, *SyntaxContext));
  }

  default:
  UnknownCharacter:
    checkForInputIncomplete();
    diagnose(Tok, ErrorID);
    return makeParsedError<ParsedExprSyntax>();
  }
}

ParserResult<Expr> Parser::parseExprPrimary(Diag<> ID, bool isExprBasic) {
  auto leadingLoc = leadingTriviaLoc();
  auto parsed = parseExprPrimarySyntax(ID, isExprBasic);
  if (parsed.isNull())
    return parsed.getStatus();
  SyntaxContext->addSyntax(parsed.get());
  auto syntax = SyntaxContext->topNode<ExprSyntax>();
  return makeParserResult(parsed.getStatus(),
                          Generator.generate(syntax, leadingLoc));
}

///   expr-literal:
///     string_literal
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprStringLiteralSyntax() {
  // The start location of the entire string literal.
  const auto Range = Tok.getRange();
  const auto tokText = Tok.getRawText();
  const auto commentStart = Tok.getCommentStart();

  const unsigned delimiterLength = Tok.getCustomDelimiterLen();
  const bool hasCustomDelimiter = delimiterLength > 0;
  const unsigned quoteLength = Tok.isMultilineString() ? 3 : 1;
  const tok quoteKind =
      Tok.isMultilineString()
          ? tok::multiline_string_quote
          : Tok.getRawText()[0] == '\'' ? tok::single_quote : tok::string_quote;
  const unsigned closeQuoteBegin =
      Tok.getLength() - delimiterLength - quoteLength;

  const StringRef OpenDelimiterStr = tokText.take_front(delimiterLength);
  const StringRef OpenQuoteStr = tokText.substr(delimiterLength, quoteLength);
  const StringRef CloseQuoteStr = tokText.substr(closeQuoteBegin, quoteLength);
  const StringRef CloseDelimiterStr = tokText.take_back(delimiterLength);

  ParsedStringLiteralExprSyntaxBuilder builder(*SyntaxContext);

  // Build the leading quote.
  Token openQuote(quoteKind, OpenQuoteStr);
  if (hasCustomDelimiter) {
    Token openDelimiter(tok::raw_string_delimiter, OpenDelimiterStr);
    // When a custom delimiter is present, it owns the leading trivia.
    builder.useOpenDelimiter(ParsedSyntaxRecorder::makeToken(
        openDelimiter, LeadingTrivia, {}, *SyntaxContext));
    builder.useOpenQuote(ParsedSyntaxRecorder::makeToken(
        openQuote, {}, {}, *SyntaxContext));
  } else {
    // Without custom delimiter the quote owns the leading trivia.
    builder.useOpenQuote(ParsedSyntaxRecorder::makeToken(
        openQuote, LeadingTrivia, {}, *SyntaxContext));
  }

  // Build the trailing quote.
  Token closeQuote(quoteKind, CloseQuoteStr);
  if (hasCustomDelimiter) {
    builder.useCloseQuote(ParsedSyntaxRecorder::makeToken(
        closeQuote, {}, {}, *SyntaxContext));
    Token CloseDelimiter(tok::raw_string_delimiter, CloseDelimiterStr);
    // When a custom delimiter is present, it owns the leading trivia.
    builder.useCloseDelimiter(ParsedSyntaxRecorder::makeToken(
        CloseDelimiter, TrailingTrivia, {}, *SyntaxContext));
  } else {
    // Without custom delimiter the quote owns the leading trivia.
    builder.useCloseQuote(ParsedSyntaxRecorder::makeToken(
        closeQuote, TrailingTrivia, {}, *SyntaxContext));
  }

  // Build the segments.
  SmallVector<Lexer::StringSegment, 1> segments;
  L->getStringLiteralSegments(Tok, segments);

  // Special handle pure string literal.
  if (segments.size() == 1 &&
      segments[0].Kind == Lexer::StringSegment::Literal) {
    consumeTokenWithoutFeedingReceiver();
    consumeExtraToken(Tok);

    auto &segment = segments[0];
    auto literalTok = ParsedSyntaxRecorder::makeToken(
        Token(tok::string_segment,
              CharSourceRange(segment.Loc, segment.Length).str()),
        {}, {}, *SyntaxContext);
    builder.addSegmentsMember(ParsedSyntaxRecorder::makeStringSegment(
        std::move(literalTok), *SyntaxContext));
    return makeParsedResult(builder.build());
  }

  consumeTokenWithoutFeedingReceiver();

  // Restore 'Tok' state on exit.
  llvm::SaveAndRestore<Token> SavedTok(Tok);
  llvm::SaveAndRestore<ParsedTrivia> SavedLeadingTrivia(LeadingTrivia);
  llvm::SaveAndRestore<ParsedTrivia> SavedTrailingTrivia(TrailingTrivia);
  llvm::SaveAndRestore<SourceLoc> SavedPreviousLoc(PreviousLoc);

  for (const auto &segment : segments) {
    switch (segment.Kind) {
    case Lexer::StringSegment::Literal: {
      // Normal string literals.
      auto literalTok = ParsedSyntaxRecorder::makeToken(
          Token(tok::string_segment,
                CharSourceRange(segment.Loc, segment.Length).str()),
          {}, {}, *SyntaxContext);
      builder.addSegmentsMember(ParsedSyntaxRecorder::makeStringSegment(
          std::move(literalTok), *SyntaxContext));

      // For TokenReceiver.
      auto segmentStart =
          segment.IsFirstSegment ? Range.getStart() : segment.Loc;
      auto segmentEnd =
          segment.IsLastSegment ? Range.getEnd() : segment.getEndLoc();
      auto segmentRange = CharSourceRange(SourceMgr, segmentStart, segmentEnd);
      unsigned commentLength = 0;
      if (segment.IsFirstSegment && commentStart.isValid())
        commentLength = SourceMgr.getByteDistance(commentStart, Range.getStart());
      consumeExtraToken(Token(tok::string_literal, segmentRange.str(), commentLength));
      break;
    }
    case Lexer::StringSegment::Expr: {
      // Interpolated expressions.
      //
      // expression-segment:
      //   '\' '#'* '(' expression-segment-arg-list? ')'
      // expression-segment-arg-list:
      //   expression-segment-arg (',' expression-segment-arg)?
      // expression-segment-arg:
      //   (identifier ':')? expression
      ParsedExpressionSegmentSyntaxBuilder segmentBuilder(*SyntaxContext);

      // Build '\'
      CharSourceRange backslashRange(
          segment.Loc.getAdvancedLoc(-delimiterLength - 1), 1);
      Token backslash(tok::backslash, backslashRange.str());
      segmentBuilder.useBackslash(
          ParsedSyntaxRecorder::makeToken(backslash, {}, {}, *SyntaxContext));

      // Build optional string delimiter '#'*.
      if (hasCustomDelimiter) {
        CharSourceRange delimiterRange(
            segment.Loc.getAdvancedLoc(-delimiterLength), delimiterLength);
        Token delimiter(tok::raw_string_delimiter, delimiterRange.str());
        segmentBuilder.useDelimiter(
            ParsedSyntaxRecorder::makeToken(delimiter, {}, {}, *SyntaxContext));
      }

      // Create a temporary lexer that lexes from the body of the string.
      LexerState BeginState =
          L->getStateForBeginningOfTokenLoc(segment.Loc);
      // We need to set the EOF at r_paren, to prevent the Lexer from eagerly
      // trying to lex the token beyond it. Parser::parseList() does a special
      // check for a tok::EOF that is spelled with a ')'.
      // FIXME: This seems like a hack, there must be a better way..
      LexerState EndState = BeginState.advance(segment.Length-1);
      Lexer LocalLex(*L, BeginState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

      // Prime the new lexer with a '(' as the first token.
      // We might be at tok::eof now, so ensure that consumeToken() does not
      // assert about lexing past eof.
      Tok.setKind(tok::NUM_TOKENS);
      consumeTokenWithoutFeedingReceiver();
      assert(Tok.is(tok::l_paren));

      // Parse leading '('.
      Tok.setKind(tok::string_interpolation_anchor);
      segmentBuilder.useLeftParen(
          consumeTokenSyntax(tok::string_interpolation_anchor));

      // Parse interpolated expressions.
      SmallVector<ParsedTupleExprElementSyntax, 2> args;
      auto status = parseExprTupleElementListSyntax(
          args, [&](){ return Tok.is(tok::eof); });
      for (auto &arg : args)
        segmentBuilder.addExpressionsMember(std::move(arg));

      if (status.isError())
        ignoreUntil(tok::eof);

      // Parse trailing ')'.
      // Clear the trailing trivia because they belong to the next literal
      // segment.
      Tok.setKind(tok::string_interpolation_anchor);
      TrailingTrivia.clear();
      segmentBuilder.useRightParen(
          consumeTokenSyntax(tok::string_interpolation_anchor));
      builder.addSegmentsMember(segmentBuilder.build());
    }
    }
  }

  return makeParsedResult(builder.build());
}

ParserResult<Expr> Parser::parseExprStringLiteral() {
  auto leadingLoc = leadingTriviaLoc();
  auto parsed = parseExprStringLiteralSyntax();
  if (parsed.isNull())
    return parsed.getStatus();
  SyntaxContext->addSyntax(parsed.get());
  auto syntax = SyntaxContext->topNode<ExprSyntax>();
  return makeParserResult(parsed.getStatus(),
                          Generator.generate(syntax, leadingLoc));
}

/// Parse an optional argument label and ':'. Returns \c true if it's parsed.
/// Returns \c false it it's not found.
bool Parser::parseOptionalArgumentLabelSyntax(
    Optional<ParsedTokenSyntax> &name, Optional<ParsedTokenSyntax> &colon) {
  if (!Tok.canBeArgumentLabel() || !peekToken().is(tok::colon))
    return false;

  // If this was an escaped identifier that need not have been escaped, say
  // so. Only _ needs escaping, because we take foo(_: 3) to be equivalent
  // to foo(3), to be more uniform with _ in function declaration as well as
  // the syntax for referring to the function pointer (foo(_:)),
  auto text = Tok.getText();
  auto escaped = Tok.isEscapedIdentifier();
  auto underscore = Tok.is(tok::kw__) || (escaped && text == "_");
  if (escaped && !underscore && canBeArgumentLabel(text)) {
    SourceLoc start = Tok.getLoc();
    SourceLoc end = start.getAdvancedLoc(Tok.getLength());
    diagnose(Tok, diag::escaped_parameter_name, text)
        .fixItRemoveChars(start, start.getAdvancedLoc(1))
        .fixItRemoveChars(end.getAdvancedLoc(-1), end);
  }
  name = consumeArgumentLabelSyntax();
  colon = consumeTokenSyntax(tok::colon);
  return true;
}

void Parser::parseOptionalArgumentLabel(Identifier &name, SourceLoc &loc) {
  // Check to see if there is an argument label.
  if (Tok.canBeArgumentLabel() && peekToken().is(tok::colon)) {
    auto text = Tok.getText();

    // If this was an escaped identifier that need not have been escaped, say
    // so. Only _ needs escaping, because we take foo(_: 3) to be equivalent
    // to foo(3), to be more uniform with _ in function declaration as well as
    // the syntax for referring to the function pointer (foo(_:)),
    auto escaped = Tok.isEscapedIdentifier();
    auto underscore = Tok.is(tok::kw__) || (escaped && text == "_");
    if (escaped && !underscore && canBeArgumentLabel(text)) {
      SourceLoc start = Tok.getLoc();
      SourceLoc end = start.getAdvancedLoc(Tok.getLength());
      diagnose(Tok, diag::escaped_parameter_name, text)
          .fixItRemoveChars(start, start.getAdvancedLoc(1))
          .fixItRemoveChars(end.getAdvancedLoc(-1), end);
    }

    loc = consumeArgumentLabel(name);
    consumeToken(tok::colon);
  }
}

/// Parse unqualified decl name.
///
///   decl-name:
///     identifier decl-name-arguments?
///   decl-name-arguments:
///     '(' decl-name-argument* ')'
///   decl-name-argument:
///     (identifier | '_') ':'
ParserStatus Parser::parseUnqualifiedDeclNameSyntax(
    Optional<ParsedTokenSyntax> &identTok,
    Optional<ParsedDeclNameArgumentsSyntax> &declNameArg, bool afterDot,
    const Diagnostic &diag, bool allowOperators, bool allowZeroArgCompoundNames,
    bool allowDeinitAndSubscript) {

  if (Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_self)) {
    identTok = consumeTokenSyntax();
  } else if (allowOperators && Tok.isAnyOperator()) {
    identTok = consumeTokenSyntax();
  } else if (afterDot && Tok.isKeyword()) {
    if (Tok.is(tok::kw_init) ||
        (allowDeinitAndSubscript &&
         Tok.isAny(tok::kw_deinit, tok::kw_subscript))) {
      // Parse 'init', 'deinit' and 'subscript' as a keyword.
    } else {
      Tok.setKind(tok::identifier);
    }
    identTok = consumeTokenSyntax();
  } else {
    checkForInputIncomplete();
    diagnose(Tok, diag);
    return makeParserError();
  }

  // If the next token isn't a following '(', we don't have a compound name.
  if (!Tok.isFollowingLParen())
    return makeParserSuccess();

  // If the next token is a ')' then we have a 0-arg compound name. This is
  // explicitly differentiated from "simple" (non-compound) name in DeclName.
  // Unfortunately only some places in the grammar are ok with accepting this
  // kind of name; in other places it's ambiguous with trailing calls.
  if (allowZeroArgCompoundNames && peekToken().is(tok::r_paren)) {
    ParsedDeclNameArgumentsSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftParen(consumeTokenSyntax(tok::l_paren));
    builder.useRightParen(consumeTokenSyntax(tok::r_paren));
    declNameArg = builder.build();
    return makeParserSuccess();
  }

  // If the token after that isn't an argument label or ':', we don't have a
  // compound name.
  if ((!peekToken().canBeArgumentLabel() && !peekToken().is(tok::colon)) ||
      Identifier::isEditorPlaceholder(peekToken().getText()))
    return makeParserSuccess();

  ParsedDeclNameArgumentsSyntaxBuilder builder(*SyntaxContext);

  // Try to parse a compound name.
  BacktrackingScope backtrack(*this);

  // Parse '('.
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));
  while (Tok.isNot(tok::r_paren)) {
    ParsedDeclNameArgumentSyntaxBuilder argBuilder(*SyntaxContext);

    // If we see a ':', the user forgot the '_';
    if (Tok.is(tok::colon)) {
      diagnose(Tok, diag::empty_arg_label_underscore)
          .fixItInsert(Tok.getLoc(), "_");
      argBuilder.useColon(consumeTokenSyntax(tok::colon));
      builder.addArgumentsMember(argBuilder.build());
      continue;
    }

    Optional<ParsedTokenSyntax> name;
    Optional<ParsedTokenSyntax> colon;
    if (parseOptionalArgumentLabelSyntax(name, colon)) {
      argBuilder.useName(std::move(*name));
      argBuilder.useColon(std::move(*colon));
      builder.addArgumentsMember(argBuilder.build());
      continue;
    }

    // This is not a compound name.
    // FIXME: Could recover better if we "know" it's a compound name.
    return makeParserSuccess();
  }

  // We have a compound name. Cancel backtracking and build that name.
  backtrack.cancelBacktrack();

  // Parse ')'.
  builder.useRightParen(consumeTokenSyntax(tok::r_paren));
  declNameArg = builder.build();

  return makeParserSuccess();
}

DeclName Parser::parseUnqualifiedDeclName(bool afterDot, DeclNameLoc &loc,
                                          const Diagnostic &diag,
                                          bool allowOperators,
                                          bool allowZeroArgCompoundNames,
                                          bool allowDeinitAndSubscript) {
  auto leadingLoc = leadingTriviaLoc();
  Optional<ParsedTokenSyntax> parsedIdentTok;
  Optional<ParsedDeclNameArgumentsSyntax> parsedDeclNameArgs;
  auto status = parseUnqualifiedDeclNameSyntax(
      parsedIdentTok, parsedDeclNameArgs, afterDot, diag, allowOperators,
      allowZeroArgCompoundNames, allowDeinitAndSubscript);
  if (status.isError())
    return DeclName();

  SyntaxContext->addSyntax(std::move(*parsedIdentTok));
  auto identTok = SyntaxContext->topNode<TokenSyntax>();

  Optional<DeclNameArgumentsSyntax> declNameArgs;
  if (parsedDeclNameArgs) {
    SyntaxContext->addSyntax(std::move(*parsedDeclNameArgs));
    declNameArgs.emplace(SyntaxContext->topNode<DeclNameArgumentsSyntax>());
  }

  DeclName name;
  std::tie(name, loc) =
      Generator.generateUnqualifiedDeclName(identTok, declNameArgs, leadingLoc);
  return name;
}

///   expr-identifier:
///     unqualified-decl-name generic-argument-clause?
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprIdentifierSyntax() {
  assert(Tok.isAny(tok::identifier, tok::kw_self, tok::kw_Self));

  if (swift::isEditorPlaceholder(Tok.getRawText())) {
    return makeParsedResult(ParsedSyntaxRecorder::makeEditorPlaceholderExpr(
        consumeTokenSyntax(), *SyntaxContext));
  }

  Optional<ParsedTokenSyntax> idTok;
  Optional<ParsedDeclNameArgumentsSyntax> declNameArgs;
  auto status = parseUnqualifiedDeclNameSyntax(idTok, declNameArgs,
                                               /*afterDot=*/false,
                                               diag::expected_expr);
  assert(status.isSuccess() && idTok.hasValue());
  (void)status;

  ParsedIdentifierExprSyntaxBuilder builder(*SyntaxContext);
  builder.useIdentifier(std::move(*idTok));
  if (declNameArgs)
    builder.useDeclNameArguments(std::move(*declNameArgs));

  if (canParseAsGenericArgumentList())
    return parseExprSpecializeSyntax(builder.build());

  return makeParsedResult(builder.build());
}

/// Parse generic argument suffix for the base expression.
///
///   expr-specialize:
///     expr generic-argument-clause
ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprSpecializeSyntax(ParsedExprSyntax &&base) {
  assert(startsWithLess(Tok));

  auto LAngleLoc = Tok.getLoc();
  auto genericArgs = parseGenericArgumentClauseSyntax();
  auto status = genericArgs.getStatus();

  if (status.isError())
    diagnose(LAngleLoc, diag::while_parsing_as_left_angle_bracket);
  assert(!genericArgs.isNull());

  auto specializeExpr = ParsedSyntaxRecorder::makeSpecializeExpr(
      std::move(base), genericArgs.get(), *SyntaxContext);
  return makeParsedResult(std::move(specializeExpr), status);
}

Expr *Parser::parseExprIdentifier() {
  auto leadingLoc = leadingTriviaLoc();
  auto parsed = parseExprIdentifierSyntax();
  assert(!parsed.isNull());
  SyntaxContext->addSyntax(parsed.get());

  auto syntax = SyntaxContext->topNode<ExprSyntax>();
  return Generator.generate(syntax, leadingLoc);
}

Expr *Parser::parseExprEditorPlaceholder(SourceLoc loc, StringRef text) {
  auto parseTypeForPlaceholder = [&](TypeLoc &TyLoc, TypeRepr *&ExpansionTyR) {
    Optional<EditorPlaceholderData> DataOpt =
        swift::parseEditorPlaceholder(text);
    if (!DataOpt)
      return;
    StringRef TypeStr = DataOpt->Type;
    if (TypeStr.empty())
      return;

    // Ensure that we restore the parser state at exit.
    ParserPositionRAII PPR(*this);

    auto parseTypeString = [&](StringRef TyStr) -> TypeRepr* {
      unsigned Offset = TyStr.data() - text.data();
      SourceLoc TypeStartLoc = loc.getAdvancedLoc(Offset);
      SourceLoc TypeEndLoc = TypeStartLoc.getAdvancedLoc(TyStr.size());

      LexerState StartState = L->getStateForBeginningOfTokenLoc(TypeStartLoc);
      LexerState EndState = L->getStateForBeginningOfTokenLoc(TypeEndLoc);

      // Create a lexer for the type sub-string.
      Lexer LocalLex(*L, StartState, EndState);

      // Temporarily swap out the parser's current lexer with our new one.
      llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

      // Don't feed to syntax token recorder.
      ConsumeTokenReceiver DisabledRec;
      llvm::SaveAndRestore<ConsumeTokenReceiver *> R(TokReceiver, &DisabledRec);
      SyntaxParsingContext SContext(SyntaxContext);
      SContext.setDiscard();

      Tok.setKind(tok::unknown); // we might be at tok::eof now.
      consumeTokenWithoutFeedingReceiver();
      return parseType().getPtrOrNull();
    };

    TypeRepr *TyR = parseTypeString(TypeStr);
    TyLoc = TyR;
    if (DataOpt->TypeForExpansion == TypeStr) {
      ExpansionTyR = TyR;
    } else {
      ExpansionTyR = parseTypeString(DataOpt->TypeForExpansion);
    }
  };

  TypeLoc TyLoc;
  TypeRepr *ExpansionTyR = nullptr;
  parseTypeForPlaceholder(TyLoc, ExpansionTyR);
  return new (Context) EditorPlaceholderExpr(Context.getIdentifier(text),
                                             loc, TyLoc, ExpansionTyR);
}

// Extract names of the tuple elements and preserve the structure
// of the tuple (with any nested tuples inside) to be able to use
// it in the fix-it without any type information provided by user.
static void printTupleNames(const TypeRepr *typeRepr, llvm::raw_ostream &OS) {
  if (!typeRepr)
    return;
  
  auto tupleRepr = dyn_cast<TupleTypeRepr>(typeRepr);
  if (!tupleRepr)
    return;
  
  OS << "(";
  unsigned elementIndex = 0;
  llvm::SmallVector<TypeRepr *, 10> elementTypes;
  tupleRepr->getElementTypes(elementTypes);
  interleave(elementTypes,
             [&](const TypeRepr *element) {
               if (isa<TupleTypeRepr>(element)) {
                 printTupleNames(element, OS);
               } else {
                 auto name = tupleRepr->getElementName(elementIndex);
                 // If there is no label from the element
                 // it means that it's malformed and we can
                 // use the type instead.
                 if (name.empty())
                   element->print(OS);
                 else
                   OS << name;
               }
               
               ++elementIndex;
             },
             [&] { OS << ", "; });
  OS << ")";
}

bool Parser::
parseClosureSignatureIfPresent(SmallVectorImpl<CaptureListEntry> &captureList,
                               ParameterList *&params, SourceLoc &throwsLoc,
                               SourceLoc &arrowLoc,
                               TypeRepr *&explicitResultType, SourceLoc &inLoc){
  // Clear out result parameters.
  params = nullptr;
  throwsLoc = SourceLoc();
  arrowLoc = SourceLoc();
  explicitResultType = nullptr;
  inLoc = SourceLoc();

  // If we have a leading token that may be part of the closure signature, do a
  // speculative parse to validate it and look for 'in'.
  if (Tok.isAny(tok::l_paren, tok::l_square, tok::identifier, tok::kw__)) {
    BacktrackingScope backtrack(*this);

    // Skip by a closure capture list if present.
    if (consumeIf(tok::l_square)) {
      skipUntil(tok::r_square);
      if (!consumeIf(tok::r_square))
        return false;
    }

    // Parse pattern-tuple func-signature-result? 'in'.
    if (consumeIf(tok::l_paren)) {      // Consume the ')'.

      // While we don't have '->' or ')', eat balanced tokens.
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof))
        skipSingle();

      // Consume the ')', if it's there.
      if (consumeIf(tok::r_paren)) {
        consumeIf(tok::kw_throws) || consumeIf(tok::kw_rethrows);
        // Parse the func-signature-result, if present.
        if (consumeIf(tok::arrow)) {
          if (!canParseType())
            return false;
        }
      }

      // Okay, we have a closure signature.
    } else if (Tok.isIdentifierOrUnderscore()) {
      // Parse identifier (',' identifier)*
      consumeToken();
      while (consumeIf(tok::comma)) {
        if (Tok.isIdentifierOrUnderscore()) {
          consumeToken();
          continue;
        }

        return false;
      }
      
      consumeIf(tok::kw_throws) || consumeIf(tok::kw_rethrows);

      // Parse the func-signature-result, if present.
      if (consumeIf(tok::arrow)) {
        if (!canParseType())
          return false;
      }
    }
    
    // Parse the 'in' at the end.
    if (Tok.isNot(tok::kw_in))
      return false;

    // Okay, we have a closure signature.
  } else {
    // No closure signature.
    return false;
  }
  SyntaxParsingContext ClosureSigCtx(SyntaxContext, SyntaxKind::ClosureSignature);
  if (Tok.is(tok::l_square) && peekToken().is(tok::r_square)) {
    SyntaxParsingContext CaptureCtx(SyntaxContext,
                                    SyntaxKind::ClosureCaptureSignature);
    consumeToken(tok::l_square);
    consumeToken(tok::r_square);
  } else if (Tok.is(tok::l_square) && !peekToken().is(tok::r_square)) {
    SyntaxParsingContext CaptureCtx(SyntaxContext,
                                    SyntaxKind::ClosureCaptureSignature);
    consumeToken(tok::l_square);
    // At this point, we know we have a closure signature. Parse the capture list
    // and parameters.
    bool HasNext;
    do {
      SyntaxParsingContext CapturedItemCtx(SyntaxContext,
                                           SyntaxKind::ClosureCaptureItem);
      SWIFT_DEFER { HasNext = consumeIf(tok::comma); };
      // Check for the strength specifier: "weak", "unowned", or
      // "unowned(safe/unsafe)".
      SourceLoc ownershipLocStart, ownershipLocEnd;
      auto ownershipKind = ReferenceOwnership::Strong;
      if (Tok.isContextualKeyword("weak")){
        ownershipLocStart = ownershipLocEnd = consumeToken(tok::identifier);
        ownershipKind = ReferenceOwnership::Weak;
      } else if (Tok.isContextualKeyword("unowned")) {
        ownershipLocStart = ownershipLocEnd = consumeToken(tok::identifier);
        ownershipKind = ReferenceOwnership::Unowned;

        // Skip over "safe" and "unsafe" if present.
        if (consumeIf(tok::l_paren)) {
          if (Tok.getText() == "safe")
            ownershipKind =
                ReferenceOwnership::Unowned; // FIXME: No "safe" variant.
          else if (Tok.getText() == "unsafe")
            ownershipKind = ReferenceOwnership::Unmanaged;
          else
            diagnose(Tok, diag::attr_unowned_invalid_specifier);
          consumeIf(tok::identifier, ownershipLocEnd);
          if (!consumeIf(tok::r_paren, ownershipLocEnd))
            diagnose(Tok, diag::attr_unowned_expected_rparen);
        }
      } else if (Tok.isAny(tok::identifier, tok::kw_self) &&
                 peekToken().isAny(tok::equal, tok::comma, tok::r_square)) {
        // "x = 42", "x," and "x]" are all strong captures of x.
      } else {
        diagnose(Tok, diag::expected_capture_specifier);
        skipUntil(tok::comma, tok::r_square);
        continue;
      }

      if (Tok.isNot(tok::identifier, tok::kw_self)) {
        diagnose(Tok, diag::expected_capture_specifier_name);
        skipUntil(tok::comma, tok::r_square);
        continue;
      }

      // Squash all tokens, if any, as the specifier of the captured item.
      CapturedItemCtx.collectNodesInPlace(SyntaxKind::TokenList);

      // The thing being capture specified is an identifier, or as an identifier
      // followed by an expression.
      Expr *initializer;
      Identifier name;
      SourceLoc nameLoc = Tok.getLoc();
      SourceLoc equalLoc;
      if (peekToken().isNot(tok::equal)) {
        // If this is the simple case, then the identifier is both the name and
        // the expression to capture.
        name = Context.getIdentifier(Tok.getText());
        initializer = parseExprIdentifier();

        // It is a common error to try to capture a nested field instead of just
        // a local name, reject it with a specific error message.
        if (Tok.isAny(tok::period, tok::exclaim_postfix,tok::question_postfix)){
          diagnose(Tok, diag::cannot_capture_fields);
          skipUntil(tok::comma, tok::r_square);
          continue;
        }

      } else {
        // Otherwise, the name is a new declaration.
        consumeIdentifier(&name);
        equalLoc = consumeToken(tok::equal);

        auto ExprResult = parseExpr(diag::expected_init_capture_specifier);
        if (ExprResult.isNull())
          continue;
        initializer = ExprResult.get();
      }

      // Create the VarDecl and the PatternBindingDecl for the captured
      // expression.  This uses the parent declcontext (not the closure) since
      // the initializer expression is evaluated before the closure is formed.
      auto introducer = (ownershipKind != ReferenceOwnership::Weak
                         ? VarDecl::Introducer::Let
                         : VarDecl::Introducer::Var);
      auto *VD = new (Context) VarDecl(/*isStatic*/false, introducer,
                                       /*isCaptureList*/true,
                                       nameLoc, name, CurDeclContext);

      // Attributes.
      if (ownershipKind != ReferenceOwnership::Strong)
        VD->getAttrs().add(new (Context) ReferenceOwnershipAttr(
          SourceRange(ownershipLocStart, ownershipLocEnd), ownershipKind));

      auto pattern = new (Context) NamedPattern(VD, /*implicit*/true);

      auto *PBD = PatternBindingDecl::create(
          Context, /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
          /*VarLoc*/ nameLoc, pattern, /*EqualLoc*/ equalLoc, initializer,
          CurDeclContext);

      captureList.push_back(CaptureListEntry(VD, PBD));
    } while (HasNext);

    SyntaxContext->collectNodesInPlace(SyntaxKind::ClosureCaptureItemList);
    // The capture list needs to be closed off with a ']'.
    if (!consumeIf(tok::r_square)) {
      diagnose(Tok, diag::expected_capture_list_end_rsquare);
      skipUntil(tok::r_square);
      if (Tok.is(tok::r_square))
        consumeToken(tok::r_square);
    }
  }
  
  bool invalid = false;
  if (Tok.isNot(tok::kw_in)) {
    if (Tok.is(tok::l_paren)) {
      // Parse the closure arguments.
      auto pattern = parseSingleParameterClause(ParameterContextKind::Closure);
      if (pattern.isNonNull())
        params = pattern.get();
      else
        invalid = true;
    } else {
      SyntaxParsingContext ClParamListCtx(SyntaxContext,
                                          SyntaxKind::ClosureParamList);
      // Parse identifier (',' identifier)*
      SmallVector<ParamDecl*, 4> elements;
      bool HasNext;
      do {
        SyntaxParsingContext ClParamCtx(SyntaxContext, SyntaxKind::ClosureParam);
        if (Tok.isNot(tok::identifier, tok::kw__)) {
          diagnose(Tok, diag::expected_closure_parameter_name);
          invalid = true;
          break;
        }

        Identifier name;
        SourceLoc nameLoc;
        if (Tok.is(tok::identifier)) {
          nameLoc = consumeIdentifier(&name);
        } else {
          nameLoc = consumeToken(tok::kw__);
        }
        auto var = new (Context)
            ParamDecl(SourceLoc(), SourceLoc(),
                      Identifier(), nameLoc, name, nullptr);
        var->setSpecifier(ParamSpecifier::Default);
        elements.push_back(var);

        // Consume a comma to continue.
        HasNext = consumeIf(tok::comma);
      } while (HasNext);

      params = ParameterList::create(Context, elements);
    }
    
    if (Tok.is(tok::kw_throws)) {
      throwsLoc = consumeToken();
    } else if (Tok.is(tok::kw_rethrows)) {
      throwsLoc = consumeToken();
      diagnose(throwsLoc, diag::rethrowing_function_type)
        .fixItReplace(throwsLoc, "throws");
    }

    // Parse the optional explicit return type.
    if (Tok.is(tok::arrow)) {
      SyntaxParsingContext ReturnCtx(SyntaxContext, SyntaxKind::ReturnClause);
      // Consume the '->'.
      arrowLoc = consumeToken();

      // Parse the type.
      explicitResultType =
          parseType(diag::expected_closure_result_type).getPtrOrNull();
      if (!explicitResultType) {
        // If we couldn't parse the result type, clear out the arrow location.
        arrowLoc = SourceLoc();
        invalid = true;
      }
    }
  }

  // Parse the 'in'.
  if (Tok.is(tok::kw_in)) {
    inLoc = consumeToken();
  } else {
    // Scan forward to see if we can find the 'in'. This re-synchronizes the
    // parser so we can at least parse the body correctly.
    SourceLoc startLoc = Tok.getLoc();
    ParserPosition pos = getParserPosition();
    while (Tok.isNot(tok::eof) && !Tok.is(tok::kw_in) &&
           Tok.isNot(tok::r_brace)) {
      skipSingle();
    }

    if (Tok.is(tok::kw_in)) {
      // We found the 'in'. If this is the first error, complain about the
      // junk tokens in-between but re-sync at the 'in'.
      if (!invalid) {
        diagnose(startLoc, diag::unexpected_tokens_before_closure_in);
      }
      inLoc = consumeToken();
    } else {
      // We didn't find an 'in', backtrack to where we started. If this is the
      // first error, complain about the missing 'in'.
      backtrackToPosition(pos);
      if (!invalid) {
        diagnose(Tok, diag::expected_closure_in)
          .fixItInsert(Tok.getLoc(), "in ");
      }
      inLoc = Tok.getLoc();
    }
  }

  if (!params)
    return invalid;

  // If this was a closure declaration (maybe even trailing)
  // tuple parameter destructuring is one of the common
  // problems, and is misleading to users, so it's imperative
  // to detect any tuple splat or destructuring as early as
  // possible and give a proper fix-it. See SE-0110 for more details.
  auto isTupleDestructuring = [](ParamDecl *param) -> bool {
    if (!param->isInvalid())
      return false;
    auto &typeLoc = param->getTypeLoc();
    if (auto typeRepr = typeLoc.getTypeRepr())
      return !param->hasName() && isa<TupleTypeRepr>(typeRepr);
    return false;
  };

  for (unsigned i = 0, e = params->size(); i != e; ++i) {
    auto *param = params->get(i);
    if (!isTupleDestructuring(param))
      continue;

    auto argName = "arg" + std::to_string(i);
    auto typeLoc = param->getTypeLoc();

    SmallString<64> fixIt;
    llvm::raw_svector_ostream OS(fixIt);
    auto isMultiLine = Tok.isAtStartOfLine();
    StringRef indent = Lexer::getIndentationForLine(SourceMgr, Tok.getLoc());
    if (isMultiLine)
      OS << '\n' << indent;

    OS << "let ";
    printTupleNames(typeLoc.getTypeRepr(), OS);
    OS << " = " << argName << (isMultiLine ? "\n" + indent : "; ");

    diagnose(param->getStartLoc(), diag::anon_closure_tuple_param_destructuring)
        .fixItReplace(param->getSourceRange(), argName)
        .fixItInsert(Tok.getLoc(), OS.str());

    invalid = true;
  }

  return invalid;
}

ParserResult<Expr> Parser::parseExprClosure() {
  assert(Tok.is(tok::l_brace) && "Not at a left brace?");
  SyntaxParsingContext ClosureContext(SyntaxContext, SyntaxKind::ClosureExpr);
  // We may be parsing this closure expr in a matching pattern context.  If so,
  // reset our state to not be in a pattern for any recursive pattern parses.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
  T(InVarOrLetPattern, IVOLP_NotInVarOrLet);
  
  // Parse the opening left brace.
  SourceLoc leftBrace = consumeToken();

  // Parse the closure-signature, if present.
  ParameterList *params = nullptr;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  TypeRepr *explicitResultType;
  SourceLoc inLoc;
  SmallVector<CaptureListEntry, 2> captureList;
  parseClosureSignatureIfPresent(captureList, params, throwsLoc, arrowLoc,
                                 explicitResultType, inLoc);

  // If the closure was created in the context of an array type signature's
  // size expression, there will not be a local context. A parse error will
  // be reported at the signature's declaration site.
  if (!CurLocalContext) {
    skipUntil(tok::r_brace);
    if (Tok.is(tok::r_brace))
      consumeToken();
    return makeParserError();
  }
  
  unsigned discriminator = CurLocalContext->claimNextClosureDiscriminator();

  // Create the closure expression and enter its context.
  auto *closure = new (Context) ClosureExpr(params, throwsLoc, arrowLoc, inLoc,
                                            explicitResultType,
                                            discriminator, CurDeclContext);
  // The arguments to the func are defined in their own scope.
  Scope S(this, ScopeKind::ClosureParams);
  ParseFunctionBody cc(*this, closure);

  // Handle parameters.
  if (params) {
    // Add the parameters into scope.
    addParametersToScope(params);
    setLocalDiscriminatorToParamList(params);
  } else {
    // There are no parameters; allow anonymous closure variables.
    // FIXME: We could do this all the time, and then provide Fix-Its
    // to map $i -> the appropriately-named argument. This might help
    // users who are refactoring code by adding names.
    AnonClosureVars.push_back({ leftBrace, {}});
  }
  
  // Add capture list variables to scope.
  for (auto c : captureList)
    addToScope(c.Var);

  // Parse the body.
  SmallVector<ASTNode, 4> bodyElements;
  ParserStatus Status;
  Status |= parseBraceItems(bodyElements, BraceItemListKind::Brace);

  // Parse the closing '}'.
  SourceLoc rightBrace;
  if (parseMatchingToken(tok::r_brace, rightBrace,
                         diag::expected_closure_rbrace, leftBrace)) {
    // Synthesize an r_brace syntax node if the token is absent
    SyntaxContext->synthesize(tok::r_brace, rightBrace);
  }

  // If we didn't have any parameters, create a parameter list from the
  // anonymous closure arguments.
  if (!params) {
    // Create a parameter pattern containing the anonymous variables.
    auto &anonVars = AnonClosureVars.back().second;
    SmallVector<ParamDecl*, 4> elements;
    for (auto anonVar : anonVars)
      elements.push_back(anonVar);
    
    params = ParameterList::create(Context, leftBrace, elements, leftBrace);

    // Pop out of the anonymous closure variables scope.
    AnonClosureVars.pop_back();

    // Attach the parameters to the closure.
    closure->setParameterList(params);
    closure->setHasAnonymousClosureVars();
  }

  // If the body consists of a single expression, turn it into a return
  // statement.
  //
  // But don't do this transformation during code completion, as the source
  // may be incomplete and the type mismatch in return statement will just
  // confuse the type checker.
  bool hasSingleExpressionBody = false;
  if (!Status.hasCodeCompletion() && bodyElements.size() == 1) {
    // If the closure's only body element is a single return statement,
    // use that instead of creating a new wrapping return expression.
    Expr *returnExpr = nullptr;
    
    if (bodyElements[0].is<Stmt *>()) {
      if (auto returnStmt =
                  dyn_cast<ReturnStmt>(bodyElements[0].get<Stmt*>())) {
        
        if (!returnStmt->hasResult()) {
          
          returnExpr = TupleExpr::createEmpty(Context,
                                              SourceLoc(),
                                              SourceLoc(),
                                              /*implicit*/true);
          
          returnStmt->setResult(returnExpr);
        }
        
        hasSingleExpressionBody = true;
      }
    }
    
    // Otherwise, create the wrapping return.
    if (bodyElements[0].is<Expr *>()) {
      hasSingleExpressionBody = true;
      returnExpr = bodyElements[0].get<Expr*>();
      bodyElements[0] = new (Context) ReturnStmt(SourceLoc(),
                                                 returnExpr);
    }
  }

  // Set the body of the closure.
  closure->setBody(BraceStmt::create(Context, leftBrace, bodyElements,
                                     rightBrace),
                   hasSingleExpressionBody);

  // If the closure includes a capture list, create an AST node for it as well.
  Expr *result = closure;
  if (!captureList.empty())
    result = CaptureListExpr::create(Context, captureList, closure);

  return makeParserResult(Status, result);
}

ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprClosureSyntax() {
  SyntaxParsingContext TmpContext(SyntaxContext);
  TmpContext.setTransparent();

  SourceLoc ExprLoc = Tok.getLoc();
  ParserResult<Expr> Result = parseExprClosure();
  if (auto ParsedExpr = TmpContext.popIf<ParsedExprSyntax>()) {
    Generator.addExpr(Result.getPtrOrNull(), ExprLoc);
    return makeParsedResult(std::move(*ParsedExpr), Result.getStatus());
  }
  return Result.getStatus();
}

/// Parse a tuple expression or a paren expression.
///
///   expr-tuple:
///     '(' ')'
///     '(' expr-tuple-element-list ')'
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprTupleSyntax() {
  ParsedTupleExprSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  StructureMarkerRAII ParsingExprList(*this, Tok);
  if (ParsingExprList.isFailed())
    return makeParserError();

  // Parse '('.
  auto LParenLoc = Tok.getLoc();
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));

  // Parse the elements.
  SmallVector<ParsedTupleExprElementSyntax, 4> elements;
  status |= parseExprTupleElementListSyntax(
      elements, [&]() { return Tok.is(tok::r_paren); });
  for (auto &elem : elements)
    builder.addElementListMember(std::move(elem));

  // Parse ')'.
  auto RParen =
      parseMatchingTokenSyntax(tok::r_paren, diag::expected_rparen_expr_list,
                               LParenLoc, /*silenceDiag=*/status.isError());
  status |= RParen.getStatus();
  if (!RParen.isNull())
    builder.useRightParen(RParen.get());

  return makeParsedResult(builder.build(), status);
}

/// parseExprList - Parse a list of expressions.
///
///   expr-tuple-element-list:
///     expr-tuple-element (',' expr-tuple-element)*
///   expr-tuple-element:
///     (identifier ':')? (expr | binary-operator)
///
ParserStatus Parser::parseExprTupleElementListSyntax(
    SmallVectorImpl<ParsedTupleExprElementSyntax> &elements,
    llvm::function_ref<bool()> isAtCloseTok) {
  return parseListSyntax(
      elements, /*AllowEmpty=*/true, /*AllowSepAfterLast=*/false, isAtCloseTok,
      [&](ParsedTupleExprElementSyntaxBuilder &elemBuilder) {
        Optional<ParsedTokenSyntax> label;
        Optional<ParsedTokenSyntax> colon;
        if (parseOptionalArgumentLabelSyntax(label, colon)) {
          elemBuilder.useLabel(std::move(*label));
          elemBuilder.useColon(std::move(*colon));
        }

        // See if we have an operator decl ref '(<op>)'. The operator token in
        // this case lexes as a binary operator because it neither leads nor
        // follows a proper subexpression.
        if (Tok.isBinaryOperator() &&
            peekToken().isAny(tok::r_paren, tok::r_square, tok::eof,
                              tok::comma)) {
          elemBuilder.useExpression(ParsedSyntaxRecorder::makeIdentifierExpr(
              consumeTokenSyntax(), None, *SyntaxContext));
          return makeParserSuccess();
        }

        auto subExpr = parseExpressionSyntax(diag::expected_expr_in_expr_list);
        if (!subExpr.isNull())
          elemBuilder.useExpression(subExpr.get());
        else
          elemBuilder.useExpression(
              ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));
        return subExpr.getStatus();
      });
}

/// Parse a parenthesized expression list for a call-like expressions including
/// subscripts.
///
///   expr-list(left, right):
///     left right expr-closure?
///     left expr-tuple-element-list right expr-closure?
ParserStatus Parser::parseExprListSyntax(
    tok leftK, tok rightK, bool isPostfix, bool isExprBasic,
    llvm::function_ref<void(
        ParsedTokenSyntax &&, ParsedTupleExprElementListSyntax &&,
        Optional<ParsedTokenSyntax> &&, Optional<ParsedClosureExprSyntax> &&)>
        callback) {
  StructureMarkerRAII ParsingExprList(*this, Tok);
  if (ParsingExprList.isFailed())
    return makeParserError();

  ParserStatus status;

  auto TokIsStringInterpolationEOF = [&]() -> bool {
    return Tok.is(tok::eof) && Tok.getText() == ")" && rightK == tok::r_paren;
  };

  // Parse '(' or '['.
  auto leftLoc = Tok.getLoc();
  auto leftTok = consumeTokenSyntax(tok::l_paren);

  // Parse the elements.
  SmallVector<ParsedTupleExprElementSyntax, 4> elements;
  status |= parseExprTupleElementListSyntax(elements, [&] {
    return Tok.is(rightK) || TokIsStringInterpolationEOF();
  });
  auto list =
      ParsedSyntaxRecorder::makeTupleExprElementList(elements, *SyntaxContext);

  if (TokIsStringInterpolationEOF()) {
    callback(std::move(leftTok), std::move(list), None, None);
    return status;
  }

  // Parse ')' or ']'.
  auto rightTok =
      parseMatchingTokenSyntax(rightK, rightK == tok::r_paren
                                         ? diag::expected_rparen_expr_list
                                         : diag::expected_rsquare_expr_list,
                               leftLoc, /*silenceDiag=*/status.isError());
  status |= rightTok.getStatus();
  auto rightLoc = PreviousLoc;

  // If we aren't interested in trailing closures, or there isn't a valid one,
  // we're done.
  if (rightTok.isNull() || !isPostfix || Tok.isNot(tok::l_brace) ||
      !isValidTrailingClosure(isExprBasic, *this)) {
    callback(std::move(leftTok), std::move(list), rightTok.getOrNull(), None);
    return status;
  }

  auto closure = parseTrailingClosureSyntax({leftLoc, rightLoc});
  status |= closure.getStatus();
  callback(std::move(leftTok), std::move(list), rightTok.getOrNull(), closure.getOrNull());
  return status;
}

ParserStatus Parser::parseExprList(tok leftTok, tok rightTok,
                                   bool isPostfix,
                                   bool isExprBasic,
                                   SourceLoc &leftLoc,
                                   SmallVectorImpl<Expr *> &exprs,
                                   SmallVectorImpl<Identifier> &exprLabels,
                                   SmallVectorImpl<SourceLoc> &exprLabelLocs,
                                   SourceLoc &rightLoc,
                                   Expr *&trailingClosure) {
  trailingClosure = nullptr;

  StructureMarkerRAII ParsingExprList(*this, Tok);
  if (ParsingExprList.isFailed())
    return makeParserError();

  auto TokIsStringInterpolationEOF = [&]() -> bool {
    return Tok.is(tok::eof) && Tok.getText() == ")" && rightTok == tok::r_paren;
  };

  leftLoc = consumeToken(leftTok);

  ParserStatus status;
  // Parse the parenthesized expression list.
  {
    auto leadingLoc = leadingTriviaLoc();
    SmallVector<ParsedTupleExprElementSyntax, 4> parsedElements;
    status |= parseExprTupleElementListSyntax(parsedElements, [&] {
      return Tok.is(rightTok) || TokIsStringInterpolationEOF();
    });

    // Elements.
    SyntaxContext->addSyntax(ParsedSyntaxRecorder::makeTupleExprElementList(
        parsedElements, *SyntaxContext));
    auto elements = SyntaxContext->topNode<TupleExprElementListSyntax>();
    Generator.generateExprTupleElementList(elements, leadingLoc,
                                           /*isForCallArguments=*/true, exprs,
                                           exprLabels, exprLabelLocs);
  }

  if (TokIsStringInterpolationEOF()) {
    rightLoc = Tok.getLoc();
    return status;
  }

  if (status.isError()) {
    // If we've already got errors, don't emit missing RightK diagnostics.
    rightLoc =
        Tok.is(rightTok) ? consumeToken() : getLocForMissingMatchingToken();
  } else if (parseMatchingToken(rightTok, rightLoc,
                                rightTok == tok::r_paren
                                    ? diag::expected_rparen_expr_list
                                    : diag::expected_rsquare_expr_list,
                                leftLoc)) {
    status.setIsParseError();
  }

  // If we aren't interested in trailing closures, or there isn't a valid one,
  // we're done.
  if (!isPostfix || Tok.isNot(tok::l_brace) ||
      !isValidTrailingClosure(isExprBasic, *this))
    return status;

  // Parse the closure.
  ParserResult<Expr> closure =
    parseTrailingClosure(SourceRange(leftLoc, rightLoc));
  status |= closure;
  if (closure.isNull())
    return status;

  // Record the trailing closure.
  trailingClosure = closure.get();

  return status;
}

ParserResult<Expr> Parser::parseTrailingClosure(SourceRange calleeRange) {
  SourceLoc braceLoc = Tok.getLoc();

  // Record the line numbers for the diagnostics below.
  // Note that *do not* move this to after 'parseExprClosure()' it slows down
  // 'getLineNumber()' call because of cache in SourceMgr.
  auto origLine = SourceMgr.getLineNumber(calleeRange.End);
  auto braceLine = SourceMgr.getLineNumber(braceLoc);

  // Parse the closure.
  ParserResult<Expr> closure = parseExprClosure();
  if (closure.isNull())
    return makeParserError();

  // Warn if the trailing closure is separated from its callee by more than
  // one line. A single-line separation is acceptable for a trailing closure
  // call, and will be diagnosed later only if the call fails to typecheck.
  if (braceLine > origLine + 1) {
    diagnose(braceLoc, diag::trailing_closure_after_newlines);
    diagnose(calleeRange.Start, diag::trailing_closure_callee_here);
    
    auto *CE = dyn_cast<ClosureExpr>(closure.get());
    if (CE && CE->hasAnonymousClosureVars() &&
        CE->getParameters()->size() == 0) {
      diagnose(braceLoc, diag::brace_stmt_suggest_do)
        .fixItInsert(braceLoc, "do ");
    }
  }

  return closure;
}

ParsedSyntaxResult<ParsedClosureExprSyntax>
Parser::parseTrailingClosureSyntax(SourceRange calleeRange) {
  SyntaxParsingContext TmpContext(SyntaxContext);
  TmpContext.setTransparent();

  SourceLoc ExprLoc = Tok.getLoc();
  ParserResult<Expr> Result = parseTrailingClosure(calleeRange);
  if (auto ParsedExpr = TmpContext.popIf<ParsedClosureExprSyntax>()) {
    Generator.addExpr(Result.getPtrOrNull(), ExprLoc);
    return makeParsedResult(std::move(*ParsedExpr), Result.getStatus());
  }
  return Result.getStatus();
}

/// Parse an object literal expression.
///
/// expr-literal:
///   '#' identifier expr-paren
ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprObjectLiteralSyntax(bool isExprBasic) {
  ParsedObjectLiteralExprSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  builder.useIdentifier(consumeTokenSyntax());

  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expected_arg_list_in_object_literal);
    return makeParsedError(builder.build());
  }

  status |= parseExprListSyntax(
      tok::l_paren, tok::r_paren,
      /*isPostfix=*/true, isExprBasic,
      [&](ParsedTokenSyntax &&leftTok, ParsedTupleExprElementListSyntax &&args,
          Optional<ParsedTokenSyntax> &&rightTok,
          Optional<ParsedClosureExprSyntax> &&closure) {
        builder.useLeftParen(std::move(leftTok));
        builder.useArguments(std::move(args));
        if (rightTok)
          builder.useRightParen(std::move(*rightTok));
        if (closure)
          builder.useTrailingClosure(std::move(*closure));
      });
  return makeParsedResult(builder.build(), status);
}

/// Parse and diagnose unknown pound expression
///
/// If it look like a legacy (Swift 2) object literal expression, suggest fix-it
/// to use new object literal syntax.
///
/// expr-unknown-pound:
///   '#' identifier expr-paren?
///   '[' '#' identifier expr-paren? '#' ']' ; Legacy object literal
ParserResult<Expr> Parser::parseExprPoundUnknown(SourceLoc LSquareLoc) {
  SourceLoc PoundLoc = consumeToken(tok::pound);

  assert(Tok.is(tok::identifier) && !Tok.isEscapedIdentifier() &&
         PoundLoc.getAdvancedLoc(1) == Tok.getLoc());

  Identifier Name;
  SourceLoc NameLoc = consumeIdentifier(&Name);

  // Parse arguments if exist.
  SourceLoc LParenLoc, RParenLoc;
  SmallVector<SourceLoc, 2> argLabelLocs;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  Expr *trailingClosure;
  if (Tok.isFollowingLParen()) {
    // Parse arguments.
    ParserStatus status =
        parseExprList(tok::l_paren, tok::r_paren,
                      /*isPostfix=*/true, /*isExprBasic*/ true, LParenLoc,
                      args, argLabels, argLabelLocs, RParenLoc, trailingClosure);
    if (status.hasCodeCompletion())
      return makeParserCodeCompletionResult<Expr>();
    if (status.isError())
      return makeParserError();
  }

  std::pair<StringRef, StringRef> NewNameArgPair =
      llvm::StringSwitch<std::pair<StringRef, StringRef>>(Name.str())
          .Case("Color", {"colorLiteral", "red"})
          .Case("Image", {"imageLiteral", "resourceName"})
          .Case("FileReference", {"fileLiteral", "resourceName"})
          .Default({});

  // If it's not legacy object literal, we don't know how to handle this.
  if (NewNameArgPair.first.empty()) {
    diagnose(PoundLoc, diag::unknown_pound_expr, Name.str());
    return makeParserError();
  }

  // Diagnose legacy object literal.

  // Didn't have arguments.
  if (LParenLoc.isInvalid()) {
    diagnose(Tok.getLoc(), diag::expected_arg_list_in_object_literal);
    return makeParserError();
  }

  // If it's started with '[', try to parse closing '#]'.
  SourceLoc RPoundLoc, RSquareLoc;
  if (LSquareLoc.isValid() && consumeIf(tok::pound, RPoundLoc))
    consumeIf(tok::r_square, RSquareLoc);

  auto diag = diagnose(LSquareLoc.isValid() ? LSquareLoc : PoundLoc,
                       diag::legacy_object_literal, LSquareLoc.isValid(),
                       Name.str(), NewNameArgPair.first);

  // Remove '[' if exist.
  if (LSquareLoc.isValid())
    diag.fixItRemove(LSquareLoc);
  // Replace the literal name.
  diag.fixItReplace(NameLoc, NewNameArgPair.first);
  // Replace the first argument.
  if (!argLabelLocs.empty() && argLabelLocs[0].isValid())
    diag.fixItReplace(argLabelLocs[0], NewNameArgPair.second);
  // Remove '#]' if exist.
  if (RPoundLoc.isValid())
    diag.fixItRemove(
        {RPoundLoc, RSquareLoc.isValid() ? RSquareLoc : RPoundLoc});

  return makeParserError();
}

ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprPoundUnknownSyntax(Optional<ParsedTokenSyntax> &&LSquare,
                                    SourceLoc LSquareLoc) {
  SourceLoc ExprLoc = Tok.getLoc();
  if (LSquareLoc.isValid())
    ExprLoc = LSquareLoc;
  ParserStatus status;
  {
    SyntaxParsingContext ExprParsingContext(SyntaxContext,
                                            SyntaxContextKind::Expr);
    if (LSquare)
      ExprParsingContext.addSyntax(std::move(*LSquare));
    ParserResult<Expr> result = parseExprPoundUnknown(LSquareLoc);
    status = result;
    Generator.addExpr(result.getPtrOrNull(), ExprLoc);
  }
  auto parsed = SyntaxContext->popIf<ParsedExprSyntax>();
  return makeParsedResult(std::move(*parsed), status);
}

/// Handle code completion after pound in expression position.
///
/// In case it's in a stmt condition position, specify \p ParentKind to
/// decide the position accepts #available(...) condtion.
///
/// expr-pound-codecompletion:
///   '#' code-completion-token
ParserResult<Expr>
Parser::parseExprPoundCodeCompletion(Optional<StmtKind> ParentKind) {
  assert(Tok.is(tok::pound) && peekToken().is(tok::code_complete) &&
         Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc());
  consumeToken(); // '#' token.
  auto CodeCompletionPos = consumeToken();
  auto Expr = new (Context) CodeCompletionExpr(CodeCompletionPos);
  if (CodeCompletion)
    CodeCompletion->completeAfterPoundExpr(Expr, ParentKind);
  return makeParserCodeCompletionResult(Expr);
}

/// Parse an expression call suffix.
///
/// expr-call-suffix:
///   expr-paren
///   expr-closure (except in expr-basic)
ParserResult<Expr>
Parser::parseExprCallSuffix(ParserResult<Expr> fn, bool isExprBasic) {
  assert(Tok.isFollowingLParen() && "Not a call suffix?");

  // Parse the first argument.

  // If there is a code completion token right after the '(', do a special case
  // callback.
  if (peekToken().is(tok::code_complete) && CodeCompletion) {
    consumeToken(tok::l_paren);
    auto CCE = new (Context) CodeCompletionExpr(Tok.getLoc());
    auto Result = makeParserResult(fn,
      CallExpr::create(Context, fn.get(), SourceLoc(),
                       { CCE },
                       { Identifier() },
                       { },
                       SourceLoc(),
                       /*trailingClosure=*/nullptr,
                       /*implicit=*/false));
    CodeCompletion->completePostfixExprParen(fn.get(), CCE);
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    Result.setHasCodeCompletion();
    return Result;
  }

  // Parse the argument list.
  SourceLoc lParenLoc, rParenLoc;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  SmallVector<SourceLoc, 2> argLabelLocs;
  Expr *trailingClosure;

  ParserStatus status = parseExprList(tok::l_paren, tok::r_paren,
                                      /*isPostfix=*/true, isExprBasic,
                                      lParenLoc, args, argLabels,
                                      argLabelLocs,
                                      rParenLoc,
                                      trailingClosure);

  // Form the call.
  return makeParserResult(
      status | fn, CallExpr::create(Context, fn.get(), lParenLoc, args,
                                    argLabels, argLabelLocs, rParenLoc,
                                    trailingClosure, /*implicit=*/false));
}

/// parseExprCollection - Parse a collection literal expression.
///
///   expr-collection:
///     expr-array
///     expr-dictionary
///   expr-array:
///     '[' expr (',' expr)* ','? ']'
///     '[' ']'
///   expr-dictionary:
///     '[' expr ':' expr (',' expr ':' expr)* ','? ']'
///     '[' ':' ']'
ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprCollectionSyntax() {
  auto LSquareLoc = Tok.getLoc();
  auto LSquare = consumeTokenSyntax(tok::l_square);

  Parser::StructureMarkerRAII ParsingCollection(
      *this, LSquareLoc, StructureMarkerKind::OpenSquare);

  // [] is always an array.
  if (Tok.is(tok::r_square)) {
    ParsedArrayExprSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquare(std::move(LSquare));
    builder.useRightSquare(consumeTokenSyntax(tok::r_square));
    return makeParsedResult(builder.build());
  }

  // [:] is always an empty dictionary.
  if (Tok.is(tok::colon) && peekToken().is(tok::r_square)) {
    ParsedDictionaryExprSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquare(std::move(LSquare));
    builder.useContent(consumeTokenSyntax(tok::colon));
    builder.useRightSquare(consumeTokenSyntax(tok::r_square));
    return makeParsedResult(builder.build());
  }

  // '[#identifier' is likely to be a legacy object literal.
  if (Tok.is(tok::pound) && peekToken().is(tok::identifier) &&
      !peekToken().isEscapedIdentifier() &&
      LSquareLoc.getAdvancedLoc(1) == Tok.getLoc() &&
      Tok.getLoc().getAdvancedLoc(1) == peekToken().getLoc()) {
     return parseExprPoundUnknownSyntax(std::move(LSquare), LSquareLoc);
  }

  auto firstExpr =
      parseExpressionSyntax(diag::expected_expr_in_collection_literal);

  if (!Tok.is(tok::colon)) {
    // Array.
    return parseExprArraySyntax(std::move(LSquare), LSquareLoc,
                                std::move(firstExpr));
  } else {
    // Dictionary.
    return parseExprDictionarySyntax(std::move(LSquare), LSquareLoc,
                                     std::move(firstExpr));
  }
}

ParsedSyntaxResult<ParsedExprSyntax>
Parser::parseExprArraySyntax(ParsedTokenSyntax &&LSquare, SourceLoc LSquareLoc,
                             ParsedSyntaxResult<ParsedExprSyntax> &&firstExpr) {
  ParserStatus status;

  ParsedArrayExprSyntaxBuilder builder(*SyntaxContext);
  builder.useLeftSquare(std::move(LSquare));

  SmallVector<ParsedArrayElementSyntax, 8> elements;

  // Parse elements.
  // NOTE: 'AllowEmpty=false' because we already have 'firstExpr'.
  bool isFirst = true;
  status |= parseListSyntax(
      elements, /*AllowEmpty=*/false, /*AllowSepAfterLast=*/true,
      [&]() { return Tok.is(tok::r_square); },
      [&](ParsedArrayElementSyntaxBuilder &elemBuilder) {
        auto expr = isFirst ? std::move(firstExpr)
                            : parseExpressionSyntax(
                                  diag::expected_expr_in_collection_literal);
        isFirst = false;
        elemBuilder.useExpression(
            !expr.isNull()
                ? expr.get()
                : ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));
        return expr.getStatus();
      });
  for (auto &elem : elements)
    builder.addElementsMember(std::move(elem));

  // Parse ']'.
  auto RSquare =
      parseMatchingTokenSyntax(tok::r_square, diag::expected_rsquare_array_expr,
                               LSquareLoc, status.isError());
  status |= RSquare.getStatus();
  if (!RSquare.isNull())
    builder.useRightSquare(RSquare.get());

  return makeParsedResult(builder.build(), status);
}

ParsedSyntaxResult<ParsedExprSyntax> Parser::parseExprDictionarySyntax(
    ParsedTokenSyntax &&LSquare, SourceLoc LSquareLoc,
    ParsedSyntaxResult<ParsedExprSyntax> &&firstExpr) {
  ParserStatus status = firstExpr.getStatus();

  ParsedDictionaryExprSyntaxBuilder builder(*SyntaxContext);
  builder.useLeftSquare(std::move(LSquare));

  SmallVector<ParsedDictionaryElementSyntax, 8> elements;

  // Parse other elements.
  bool isFirst = true;
  status |= parseListSyntax(
      elements, /*AllowEmpty=*/false, /*AllowSepAfterLast=*/true,
      [&]() { return Tok.is(tok::r_square); },
      [&](ParsedDictionaryElementSyntaxBuilder &elemBuilder) {
        // Parse the key expression.
        auto key = isFirst ? std::move(firstExpr)
                           : parseExpressionSyntax(
                                 diag::expected_key_in_dictionary_literal);
        isFirst = false;
        elemBuilder.useKeyExpression(
            !key.isNull()
                ? key.get()
                : ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));

        // Parse ':'.
        if (Tok.is(tok::colon)) {
          elemBuilder.useColon(consumeTokenSyntax(tok::colon));
        } else {
          if (key.getStatus().isSuccess())
            diagnose(Tok, diag::expected_colon_in_dictionary_literal);
          elemBuilder.useValueExpression(
               ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));
          return key.getStatus() | makeParserError();
        }

        // Parse the value expression.
        auto value =
            parseExpressionSyntax(diag::expected_value_in_dictionary_literal);
        elemBuilder.useValueExpression(
            !value.isNull()
                ? value.get()
                : ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));

        return key.getStatus() | value.getStatus();
      });
  builder.useContent(ParsedSyntaxRecorder::makeDictionaryElementList(
      elements, *SyntaxContext));

  // Parse ']'.
  auto RSquare =
      parseMatchingTokenSyntax(tok::r_square, diag::expected_rsquare_array_expr,
                               LSquareLoc, status.isError());
  status |= RSquare.getStatus();
  if (!RSquare.isNull())
    builder.useRightSquare(RSquare.get());

  return makeParsedResult(builder.build(), status);
}

void Parser::addPatternVariablesToScope(ArrayRef<Pattern *> Patterns) {
  for (Pattern *Pat : Patterns) {
    Pat->forEachVariable([&](VarDecl *VD) {
      if (VD->hasName()) {
        // Add any variable declarations to the current scope.
        addToScope(VD);
      }
    });
  }
}

void Parser::addParametersToScope(ParameterList *PL) {
  for (auto param : *PL)
    if (param->hasName())
      addToScope(param);
}



/// Parse availability query specification.
///
///  availability-spec:
///     '*'
///     language-version-constraint-spec
///     package-description-constraint-spec
///     platform-version-constraint-spec
ParserResult<AvailabilitySpec> Parser::parseAvailabilitySpec() {
  if (Tok.isBinaryOperator() && Tok.getText() == "*") {
    SourceLoc StarLoc = Tok.getLoc();
    consumeToken();

    return makeParserResult(new (Context) OtherPlatformAvailabilitySpec(StarLoc));
  }
  if (Tok.isIdentifierOrUnderscore() &&
       (Tok.getText() == "swift" || Tok.getText() == "_PackageDescription"))
      return parsePlatformAgnosticVersionConstraintSpec();

  return parsePlatformVersionConstraintSpec();
}

/// Parse platform-agnostic version constraint specification.
///
///  language-version-constraint-spec:
///     "swift" version-tuple
///  package-description-version-constraint-spec:
///     "_PackageDescription" version-tuple
ParserResult<PlatformAgnosticVersionConstraintAvailabilitySpec>
Parser::parsePlatformAgnosticVersionConstraintSpec() {
  SyntaxParsingContext VersionRestrictionContext(
      SyntaxContext, SyntaxKind::AvailabilityVersionRestriction);
  SourceLoc PlatformAgnosticNameLoc;
  llvm::VersionTuple Version;
  Optional<AvailabilitySpecKind> Kind;
  SourceRange VersionRange;

  if (Tok.isIdentifierOrUnderscore()) {
    if (Tok.getText() == "swift")
      Kind = AvailabilitySpecKind::LanguageVersionConstraint;
    else if (Tok.getText() == "_PackageDescription")
      Kind = AvailabilitySpecKind::PackageDescriptionVersionConstraint;
  }

  if (!Kind.hasValue())
    return nullptr;

  PlatformAgnosticNameLoc = Tok.getLoc();
  consumeToken();
  if (parseVersionTuple(Version, VersionRange,
                        diag::avail_query_expected_version_number)) {
    return nullptr;
  }
  return makeParserResult(new (Context)
                          PlatformAgnosticVersionConstraintAvailabilitySpec(
                            Kind.getValue(), PlatformAgnosticNameLoc, Version, VersionRange));
}

/// Parse platform-version constraint specification.
///
///  platform-version-constraint-spec:
///     identifier version-comparison version-tuple
ParserResult<PlatformVersionConstraintAvailabilitySpec>
Parser::parsePlatformVersionConstraintSpec() {
  SyntaxParsingContext VersionRestrictionContext(
      SyntaxContext, SyntaxKind::AvailabilityVersionRestriction);

  Identifier PlatformIdentifier;
  SourceLoc PlatformLoc;
  if (Tok.is(tok::code_complete)) {
    consumeToken();
    if (CodeCompletion) {
      CodeCompletion->completePoundAvailablePlatform();
    }
    return makeParserCodeCompletionStatus();
  }

  if (parseIdentifier(PlatformIdentifier, PlatformLoc,
                      diag::avail_query_expected_platform_name)) {
    return nullptr;
  }

  if (Tok.isBinaryOperator() && Tok.getText() == ">=") {
    diagnose(Tok, diag::avail_query_version_comparison_not_needed)
        .fixItRemove(Tok.getLoc());
    consumeToken();
  }

  llvm::VersionTuple Version;
  SourceRange VersionRange;

  if (parseVersionTuple(Version, VersionRange,
                        diag::avail_query_expected_version_number)) {
    return nullptr;
  }

  Optional<PlatformKind> Platform =
      platformFromString(PlatformIdentifier.str());

  if (!Platform.hasValue() || Platform.getValue() == PlatformKind::none) {
    diagnose(Tok, diag::avail_query_unrecognized_platform_name,
             PlatformIdentifier);
    Platform = PlatformKind::none;
  }

  // Register the platform name as a keyword token.
  TokReceiver->registerTokenKindChange(PlatformLoc, tok::contextual_keyword);

  return makeParserResult(new (Context) PlatformVersionConstraintAvailabilitySpec(
      Platform.getValue(), PlatformLoc, Version, VersionRange));
}
