//===--- ParsePattern.cpp - Swift Language Parser for Patterns ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Pattern Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Initializer.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/SyntaxParsingContext.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace swift::syntax;

/// Parse a pattern with an optional type annotation.
///
///  typed-pattern ::= pattern (':' type)?
///
ParserResult<Pattern> Parser::parseTypedPattern() {
  auto result = parsePattern();
  
  // Now parse an optional type annotation.
  if (Tok.is(tok::colon)) {
    SourceLoc colonLoc = consumeToken(tok::colon);
    
    if (result.isNull())  // Recover by creating AnyPattern.
      result = makeParserErrorResult(new (Context) AnyPattern(colonLoc));
    
    ParserResult<TypeRepr> Ty = parseType();
    if (Ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (!Ty.isNull()) {
      // Attempt to diagnose initializer calls incorrectly written
      // as typed patterns, such as "var x: [Int]()".
      // Disable this tentative parse when in code-completion mode, otherwise
      // code-completion may enter the delayed-decl state twice.
      if (Tok.isFollowingLParen() && !L->isCodeCompletion()) {
        BacktrackingScope backtrack(*this);
        
        // Create a local context if needed so we can parse trailing closures.
        LocalContext dummyContext;
        Optional<ContextChange> contextChange;
        if (!CurLocalContext) {
          contextChange.emplace(*this, CurDeclContext, &dummyContext);
        }
        
        SourceLoc lParenLoc, rParenLoc;
        SmallVector<Expr *, 2> args;
        SmallVector<Identifier, 2> argLabels;
        SmallVector<SourceLoc, 2> argLabelLocs;
        Expr *trailingClosure;
        ParserStatus status = parseExprList(tok::l_paren, tok::r_paren,
                                            /*isPostfix=*/true,
                                            /*isExprBasic=*/false,
                                            lParenLoc, args, argLabels,
                                            argLabelLocs, rParenLoc,
                                            trailingClosure,
                                            SyntaxKind::Unknown);
        if (status.isSuccess()) {
          backtrack.cancelBacktrack();
          
          // Suggest replacing ':' with '='
          diagnose(lParenLoc, diag::initializer_as_typed_pattern)
            .highlight({Ty.get()->getStartLoc(), rParenLoc})
            .fixItReplace(colonLoc, " = ");
          result.setIsParseError();
        }
      }
    } else {
      Ty = makeParserResult(new (Context) ErrorTypeRepr(PreviousLoc));
    }
    
    result = makeParserResult(result,
                            new (Context) TypedPattern(result.get(), Ty.get()));
  }
  
  return result;
}

/// Parse a pattern.
///   pattern ::= identifier
///   pattern ::= '_'
///   pattern ::= pattern-tuple
///   pattern ::= 'var' pattern
///   pattern ::= 'let' pattern
///
ParserResult<Pattern> Parser::parsePattern() {
  switch (Tok.getKind()) {
  case tok::l_paren:
    return parsePatternTuple();
    
  case tok::kw__:
    return makeParserResult(new (Context) AnyPattern(consumeToken(tok::kw__)));
    
  case tok::identifier: {
    Identifier name;
    SourceLoc loc = consumeIdentifier(&name);
    bool isLet = (InVarOrLetPattern != IVOLP_InVar);
    auto specifier = isLet
                   ? VarDecl::Specifier::Let
                   : VarDecl::Specifier::Var;
    if (Tok.isIdentifierOrUnderscore() && !Tok.isContextualDeclKeyword())
      diagnoseConsecutiveIDs(name.str(), loc, isLet ? "constant" : "variable");

    return makeParserResult(createBindingFromPattern(loc, name, specifier));
  }
    
  case tok::code_complete:
    if (!CurDeclContext->getAsNominalTypeOrNominalTypeExtensionContext()) {
      // This cannot be an overridden property, so just eat the token. We cannot
      // code complete anything here -- we expect an identifier.
      consumeToken(tok::code_complete);
    }
    return nullptr;
    
  case tok::kw_var:
  case tok::kw_let: {
    bool isLet = Tok.is(tok::kw_let);
    SourceLoc varLoc = consumeToken();
    
    // 'var' and 'let' patterns shouldn't nest.
    if (InVarOrLetPattern == IVOLP_InLet ||
        InVarOrLetPattern == IVOLP_InVar)
      diagnose(varLoc, diag::var_pattern_in_var, unsigned(isLet));
    
    // 'let' isn't valid inside an implicitly immutable context, but var is.
    if (isLet && InVarOrLetPattern == IVOLP_ImplicitlyImmutable)
      diagnose(varLoc, diag::let_pattern_in_immutable_context);
    
    // In our recursive parse, remember that we're in a var/let pattern.
    llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);
    
    ParserResult<Pattern> subPattern = parsePattern();
    if (subPattern.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (subPattern.isNull())
      return nullptr;
    return makeParserResult(new (Context) VarPattern(varLoc, isLet,
                                                     subPattern.get()));
  }
      
  default:
    if (Tok.isKeyword() &&
        (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
      diagnose(Tok, diag::keyword_cant_be_identifier, Tok.getText());
      diagnose(Tok, diag::backticks_to_escape)
        .fixItReplace(Tok.getLoc(), "`" + Tok.getText().str() + "`");
      SourceLoc Loc = Tok.getLoc();
      consumeToken();
      return makeParserErrorResult(new (Context) AnyPattern(Loc));
    }
    diagnose(Tok, diag::expected_pattern);
    return nullptr;
  }
}

Pattern *Parser::createBindingFromPattern(SourceLoc loc, Identifier name,
                                          VarDecl::Specifier specifier) {
  auto var = new (Context) VarDecl(/*IsStatic*/false, specifier,
                                   /*IsCaptureList*/false, loc, name, Type(),
                                   CurDeclContext);
  return new (Context) NamedPattern(var);
}

/// Parse an element of a tuple pattern.
///
///   pattern-tuple-element:
///     (identifier ':')? pattern
std::pair<ParserStatus, Optional<TuplePatternElt>>
Parser::parsePatternTupleElement() {
  // If this element has a label, parse it.
  Identifier Label;
  SourceLoc LabelLoc;

  // If the tuple element has a label, parse it.
  if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
    LabelLoc = consumeIdentifier(&Label);
    consumeToken(tok::colon);
  }

  // Parse the pattern.
  ParserResult<Pattern>  pattern = parsePattern();
  if (pattern.hasCodeCompletion())
    return std::make_pair(makeParserCodeCompletionStatus(), None);
  if (pattern.isNull())
    return std::make_pair(makeParserError(), None);

  auto Elt = TuplePatternElt(Label, LabelLoc, pattern.get());
  return std::make_pair(makeParserSuccess(), Elt);
}

/// Parse a tuple pattern.
///
///   pattern-tuple:
///     '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*
ParserResult<Pattern> Parser::parsePatternTuple() {
  StructureMarkerRAII ParsingPatternTuple(*this, Tok);
  SourceLoc LPLoc = consumeToken(tok::l_paren);
  SourceLoc RPLoc;

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  ParserStatus ListStatus =
    parseList(tok::r_paren, LPLoc, RPLoc,
              /*AllowSepAfterLast=*/false,
              diag::expected_rparen_tuple_pattern_list,
              SyntaxKind::Unknown,
              [&] () -> ParserStatus {
    // Parse the pattern tuple element.
    ParserStatus EltStatus;
    Optional<TuplePatternElt> elt;
    std::tie(EltStatus, elt) = parsePatternTupleElement();
    if (EltStatus.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (!elt)
      return makeParserError();

    // Add this element to the list.
    elts.push_back(*elt);
    return makeParserSuccess();
  });

  return makeParserResult(
           ListStatus,
           TuplePattern::createSimple(Context, LPLoc, elts, RPLoc));
}

/// Parse an optional type annotation on a pattern.
///
///  pattern-type-annotation ::= (':' type)?
///
ParserResult<Pattern> Parser::
parseOptionalPatternTypeAnnotation(ParserResult<Pattern> result,
                                   bool isOptional) {

  // Parse an optional type annotation.
  if (!consumeIf(tok::colon))
    return result;

  Pattern *P;
  if (result.isNull())
    return nullptr;
  else
    P = result.get();

  ParserResult<TypeRepr> Ty = parseType();
  if (Ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<Pattern>();

  TypeRepr *repr = Ty.getPtrOrNull();
  if (!repr)
    repr = new (Context) ErrorTypeRepr(PreviousLoc);

  // In an if-let, the actual type of the expression is Optional of whatever
  // was written.
  if (isOptional)
    repr = new (Context) OptionalTypeRepr(repr, SourceLoc());

  return makeParserResult(new (Context) TypedPattern(P, repr));
}



/// matching-pattern ::= 'is' type
/// matching-pattern ::= matching-pattern-var
/// matching-pattern ::= expr
///
ParserResult<Pattern> Parser::parseMatchingPattern(bool isExprBasic) {
  // TODO: Since we expect a pattern in this position, we should optimistically
  // parse pattern nodes for productions shared by pattern and expression
  // grammar. For short-term ease of initial implementation, we always go
  // through the expr parser for ambiguous productions.

  // Parse productions that can only be patterns.
  if (Tok.isAny(tok::kw_var, tok::kw_let)) {
    assert(Tok.isAny(tok::kw_let, tok::kw_var) && "expects var or let");
    bool isLet = Tok.is(tok::kw_let);
    SourceLoc varLoc = consumeToken();
    
    return parseMatchingPatternAsLetOrVar(isLet, varLoc, isExprBasic);
  }
  
  // matching-pattern ::= 'is' type
  if (Tok.is(tok::kw_is)) {
    SourceLoc isLoc = consumeToken(tok::kw_is);
    ParserResult<TypeRepr> castType = parseType();
    if (castType.isNull() || castType.hasCodeCompletion())
      return nullptr;
    return makeParserResult(new (Context) IsPattern(isLoc, castType.get(),
                                                    nullptr,
                                                    CheckedCastKind::Unresolved));
  }

  // matching-pattern ::= expr
  // Fall back to expression parsing for ambiguous forms. Name lookup will
  // disambiguate.
  ParserResult<Expr> subExpr =
    parseExprImpl(diag::expected_pattern, isExprBasic);
  if (subExpr.hasCodeCompletion())
    return makeParserCodeCompletionStatus();
  if (subExpr.isNull())
    return nullptr;
  
  // The most common case here is to parse something that was a lexically
  // obvious pattern, which will come back wrapped in an immediate
  // UnresolvedPatternExpr.  Transform this now to simplify later code.
  if (auto *UPE = dyn_cast<UnresolvedPatternExpr>(subExpr.get()))
    return makeParserResult(UPE->getSubPattern());
  
  return makeParserResult(new (Context) ExprPattern(subExpr.get()));
}

ParserResult<Pattern> Parser::parseMatchingPatternAsLetOrVar(bool isLet,
                                                             SourceLoc varLoc,
                                                             bool isExprBasic) {
  // 'var' and 'let' patterns shouldn't nest.
  if (InVarOrLetPattern == IVOLP_InLet ||
      InVarOrLetPattern == IVOLP_InVar)
    diagnose(varLoc, diag::var_pattern_in_var, unsigned(isLet));
  
  // 'let' isn't valid inside an implicitly immutable context, but var is.
  if (isLet && InVarOrLetPattern == IVOLP_ImplicitlyImmutable)
    diagnose(varLoc, diag::let_pattern_in_immutable_context);

  // In our recursive parse, remember that we're in a var/let pattern.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);

  ParserResult<Pattern> subPattern = parseMatchingPattern(isExprBasic);
  if (subPattern.isNull())
    return nullptr;
  auto *varP = new (Context) VarPattern(varLoc, isLet, subPattern.get());
  return makeParserResult(varP);
}


bool Parser::isOnlyStartOfMatchingPattern() {
  return Tok.isAny(tok::kw_var, tok::kw_let, tok::kw_is);
}


static bool canParsePatternTuple(Parser &P);

///   pattern ::= identifier
///   pattern ::= '_'
///   pattern ::= pattern-tuple
///   pattern ::= 'var' pattern
///   pattern ::= 'let' pattern
static bool canParsePattern(Parser &P) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
  case tok::kw__:
    P.consumeToken();
    return true;
  case tok::kw_let:
  case tok::kw_var:
    P.consumeToken();
    return canParsePattern(P);
  case tok::l_paren:
    return canParsePatternTuple(P);

  default:
    return false;
  }
}


static bool canParsePatternTuple(Parser &P) {
  if (!P.consumeIf(tok::l_paren)) return false;

  if (P.Tok.isNot(tok::r_paren)) {
    do {
      if (!canParsePattern(P)) return false;
    } while (P.consumeIf(tok::comma));
  }

  return P.consumeIf(tok::r_paren);
}

///  typed-pattern ::= pattern (':' type)?
///
bool Parser::canParseTypedPattern() {
  if (!canParsePattern(*this)) return false;
  
  if (consumeIf(tok::colon))
    return canParseType();
  return true;
}

