//===--- CSRetypechecking.cpp - Constraint Generator ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace constraints;

namespace {
// Check if \p E is a call expression to curried thunk of "KeyPath as function".
// i.e. '{ `$kp$` in { $0[keyPath: $kp$] } }(keypath)'
static bool isKeyPathCurriedThunkCallExpr(Expr *E) {
  auto CE = dyn_cast<CallExpr>(E);
  if (!CE)
    return false;
  auto thunk = dyn_cast<AutoClosureExpr>(CE->getFn());
  if (!thunk)
    return false;
  if (thunk->getParameters()->size() != 1 ||
      thunk->getParameters()->get(0)->getParameterName().str() != "$kp$")
    return false;

  auto PE = dyn_cast<ParenExpr>(CE->getArg());
  if (!PE)
    return false;
  return isa<KeyPathExpr>(PE->getSubExpr());
}

// Extract the keypath expression from the curried thunk expression.
static Expr *extractKeyPathFromCurryThunkCall(Expr *E) {
  assert(isKeyPathCurriedThunkCallExpr(E));
  auto call = cast<CallExpr>(E);
  auto arg = cast<ParenExpr>(call->getArg());
  return arg->getSubExpr();
}

/// Find the declaration directly referenced by this expression.
static std::pair<ValueDecl *, FunctionRefKind>
findReferencedDecl(Expr *expr, DeclNameLoc &loc) {
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
      expr = ice->getSubExpr();
      continue;
    }

    if (auto dre = dyn_cast<DeclRefExpr>(expr)) {
      loc = dre->getNameLoc();
      return {dre->getDecl(), dre->getFunctionRefKind()};
    }

    return {nullptr, FunctionRefKind::Unapplied};
  } while (true);
}

/// AST walker that "sanitizes" an expression for the
/// constraint-based type checker.
///
/// This is necessary because Sema fills in too much type information before
/// the type-checker runs, causing redundant work, and for expression that
/// have already been typechecked and may contain unhandled AST nodes.
///
/// FIXME: Remove this one we no longer re-type check expressions during
/// diagnostics and code completion.
class SanitizeExpr : public ASTWalker {
  ConstraintSystem &CS;
  llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpenExistentials;

public:
  SanitizeExpr(ConstraintSystem &cs) : CS(cs) {}

  ASTContext &getASTContext() const { return CS.getASTContext(); }

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    while (true) {

      // If we should reuse pre-checked types, don't sanitize the expression
      // if it's already type-checked.
      if (CS.shouldReusePrecheckedType() && expr->getType())
        return {false, expr};

      // OpenExistentialExpr contains OpaqueValueExpr in its sub expression.
      if (auto OOE = dyn_cast<OpenExistentialExpr>(expr)) {
        auto archetypeVal = OOE->getOpaqueValue();
        auto base = OOE->getExistentialValue();

        bool inserted = OpenExistentials.insert({archetypeVal, base}).second;
        assert(inserted && "OpaqueValue appears multiple times?");
        (void)inserted;
        SWIFT_DEFER { OpenExistentials.erase(archetypeVal); };

        // Walk to and return the base expression to erase any existentials
        // within it.
        return {false, OOE->getSubExpr()->walk(*this)};
      }

      // Hacky, this behaves just like an OpenedExistential in that it changes
      // the expr tree.
      if (auto ISLE = dyn_cast<InterpolatedStringLiteralExpr>(expr)) {
        if (auto subExpr = ISLE->getAppendingExpr()->getSubExpr()) {
          if (auto opaqueValue = dyn_cast<OpaqueValueExpr>(subExpr)) {
            ISLE->getAppendingExpr()->setSubExpr(nullptr);
          }
        }
      }

      // Substitute OpaqueValue with its representing existental.
      if (auto OVE = dyn_cast<OpaqueValueExpr>(expr)) {
        auto value = OpenExistentials.find(OVE);

        if (value != OpenExistentials.end()) {
          expr = value->second;
          continue;
        } else {
          assert(OVE->isPlaceholder() &&
                 "Didn't see this OVE in a containing OpenExistentialExpr?");
        }
      }

      // Skip any implicit conversions applied to this expression.
      if (auto ICE = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = ICE->getSubExpr();
        continue;
      }

      // MakeTemporarilyEscapableExpr is typechecked expression.
      if (auto MTEE = dyn_cast<MakeTemporarilyEscapableExpr>(expr)) {
        expr = MTEE->getOriginalExpr();
        continue;
      }

      // Extract keypath from '{ `$kp$` in { $0[keyPath: $kp$] } }(keypath)'
      if (isKeyPathCurriedThunkCallExpr(expr)) {
        expr = extractKeyPathFromCurryThunkCall(expr);
        continue;
      }

      // Restore '@autoclosure'd value.
      if (auto ACE = dyn_cast<AutoClosureExpr>(expr)) {
        // This is only valid if the closure doesn't have parameters.
        if (ACE->getParameters()->size() == 0) {
          expr = ACE->getSingleExpressionBody();
          continue;
        }
        llvm_unreachable("other AutoClosureExpr must be handled specially");
      }

      // Remove any semantic expression injected by typechecking.
      if (auto EPE = dyn_cast<EditorPlaceholderExpr>(expr)) {
        EPE->setSemanticExpr(nullptr);
      }

      // Strip default arguments and varargs from type-checked call
      // argument lists.
      if (isa<ParenExpr>(expr) || isa<TupleExpr>(expr)) {
        if (shouldSanitizeArgumentList(expr))
          expr = sanitizeArgumentList(expr);
      }

      // If this expression represents keypath based dynamic member
      // lookup, let's convert it back to the original form of
      // member or subscript reference.
      if (auto *SE = dyn_cast<SubscriptExpr>(expr)) {
        if (auto *TE = dyn_cast<TupleExpr>(SE->getIndex())) {
          auto isImplicitKeyPathExpr = [](Expr *argExpr) -> bool {
            if (auto *KP = dyn_cast<KeyPathExpr>(argExpr))
              return KP->isImplicit();
            return false;
          };

          if (TE->isImplicit() && TE->getNumElements() == 1 &&
              TE->getElementName(0) == getASTContext().Id_dynamicMember &&
              isImplicitKeyPathExpr(TE->getElement(0))) {
            auto *keyPathExpr = cast<KeyPathExpr>(TE->getElement(0));
            auto *componentExpr = keyPathExpr->getParsedPath();

            if (auto *UDE = dyn_cast<UnresolvedDotExpr>(componentExpr)) {
              UDE->setBase(SE->getBase());
              return {true, UDE};
            }

            if (auto *subscript = dyn_cast<SubscriptExpr>(componentExpr)) {
              subscript->setBase(SE->getBase());
              return {true, subscript};
            }

            llvm_unreachable("unknown keypath component type");
          }
        }
      }

      // Now, we're ready to walk into sub expressions.
      return {true, expr};
    }
  }

  bool isSyntheticArgumentExpr(const Expr *expr) {
    if (isa<DefaultArgumentExpr>(expr))
      return true;

    if (auto *varargExpr = dyn_cast<VarargExpansionExpr>(expr))
      if (isa<ArrayExpr>(varargExpr->getSubExpr()))
        return true;

    return false;
  }

  bool shouldSanitizeArgumentList(const Expr *expr) {
    if (auto *parenExpr = dyn_cast<ParenExpr>(expr)) {
      return isSyntheticArgumentExpr(parenExpr->getSubExpr());
    } else if (auto *tupleExpr = dyn_cast<TupleExpr>(expr)) {
      for (auto *arg : tupleExpr->getElements()) {
        if (isSyntheticArgumentExpr(arg))
          return true;
      }

      return false;
    } else {
      return isSyntheticArgumentExpr(expr);
    }
  }

  Expr *sanitizeArgumentList(Expr *original) {
    auto argList = getOriginalArgumentList(original);

    if (argList.args.size() == 1 && argList.labels[0].empty() &&
        !isa<VarargExpansionExpr>(argList.args[0])) {
      auto *result = new (getASTContext())
          ParenExpr(argList.lParenLoc, argList.args[0], argList.rParenLoc,
                    argList.hasTrailingClosure);
      result->setImplicit();
      return result;
    }

    return TupleExpr::create(getASTContext(), argList.lParenLoc, argList.args,
                             argList.labels, argList.labelLocs,
                             argList.rParenLoc, argList.hasTrailingClosure,
                             /*implicit=*/true);
  }

  Expr *walkToExprPost(Expr *expr) override {
    if (CS.hasType(expr)) {
      Type type = CS.getType(expr);
      if (type->hasOpenedExistential()) {
        type = type.transform([&](Type type) -> Type {
          if (auto archetype = type->getAs<OpenedArchetypeType>())
            return archetype->getOpenedExistentialType();
          return type;
        });
        CS.setType(expr, type);
        // Set new type to the expression directly.
        expr->setType(type);
      }
    }

    assert(!isa<ImplicitConversionExpr>(expr) &&
           "ImplicitConversionExpr should be eliminated in walkToExprPre");

    auto buildMemberRef = [&](Type memberType, Expr *base, SourceLoc dotLoc,
                              ConcreteDeclRef member, DeclNameLoc memberLoc,
                              bool implicit) -> Expr * {
      auto *memberRef = new (getASTContext())
          MemberRefExpr(base, dotLoc, member, memberLoc, implicit);

      if (memberType) {
        memberRef->setType(memberType);
        return CS.cacheType(memberRef);
      }

      return memberRef;
    };

    // A DotSyntaxCallExpr is a member reference that has already been
    // type-checked down to a call; turn it back into an overloaded
    // member reference expression.
    if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr)) {
      DeclNameLoc memberLoc;
      auto memberAndFunctionRef =
          findReferencedDecl(dotCall->getFn(), memberLoc);
      if (memberAndFunctionRef.first) {
        assert(!isa<ImplicitConversionExpr>(dotCall->getBase()));
        return buildMemberRef(dotCall->getType(), dotCall->getBase(),
                              dotCall->getDotLoc(), memberAndFunctionRef.first,
                              memberLoc, expr->isImplicit());
      }
    }

    if (auto *dynamicMember = dyn_cast<DynamicMemberRefExpr>(expr)) {
      if (auto memberRef = dynamicMember->getMember()) {
        assert(!isa<ImplicitConversionExpr>(dynamicMember->getBase()));
        return buildMemberRef(dynamicMember->getType(),
                              dynamicMember->getBase(),
                              dynamicMember->getDotLoc(), memberRef,
                              dynamicMember->getNameLoc(), expr->isImplicit());
      }
    }

    // A DotSyntaxBaseIgnoredExpr is a static member reference that has
    // already been type-checked down to a call where the argument doesn't
    // actually matter; turn it back into an overloaded member reference
    // expression.
    if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
      DeclNameLoc memberLoc;
      auto memberAndFunctionRef =
          findReferencedDecl(dotIgnored->getRHS(), memberLoc);
      if (memberAndFunctionRef.first) {
        assert(!isa<ImplicitConversionExpr>(dotIgnored->getLHS()));
        return buildMemberRef(dotIgnored->getType(), dotIgnored->getLHS(),
                              dotIgnored->getDotLoc(),
                              memberAndFunctionRef.first, memberLoc,
                              expr->isImplicit());
      }
    }
    return expr;
  }

  /// Ignore declarations.
  bool walkToDeclPre(Decl *decl) override { return false; }

  // Don't walk into statements.  This handles the BraceStmt in
  // non-single-expr closures, so we don't walk into their body.
  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override { return {false, S}; }
};
} // namespace

Type swift::getTypeOfExpressionWithoutApplying(
    Expr *&expr, DeclContext *dc, ConcreteDeclRef &referencedDecl) {
  auto &Context = dc->getASTContext();
  FrontendStatsTracer StatsTracer(Context.Stats, "typecheck-expr-no-apply",
                                  expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);
  referencedDecl = nullptr;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(dc, ConstraintSystemFlags::SuppressDiagnostics);

  // Attempt to solve the constraint system.
  const Type originalType = expr->getType();
  const bool needClearType = originalType && originalType->hasError();
  const auto recoverOriginalType = [&]() {
    if (needClearType)
      expr->setType(originalType);
  };

  // If the previous checking gives the expr error type, clear the result and
  // re-check.
  if (needClearType)
    expr->setType(Type());

  // Remove implicit conversions from the expression.
  expr = expr->walk(SanitizeExpr(cs));

  SolutionApplicationTarget target(expr, dc, CTP_Unused, Type(),
                                   /*isDiscarded=*/false);
  auto viable = cs.solve(target, FreeTypeVariableBinding::UnresolvedType);
  if (!viable) {
    recoverOriginalType();
    return Type();
  }

  // Get the expression's simplified type.
  expr = target.getAsExpr();
  auto &solution = (*viable)[0];
  auto &solutionCS = solution.getConstraintSystem();
  Type exprType = solution.simplifyType(solutionCS.getType(expr));

  assert(exprType && !exprType->hasTypeVariable() &&
         "free type variable with FreeTypeVariableBinding::GenericParameters?");

  if (exprType->hasError()) {
    recoverOriginalType();
    return Type();
  }

  // Dig the declaration out of the solution.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  auto topLocator = cs.getConstraintLocator(semanticExpr);
  referencedDecl = solution.resolveLocatorToDecl(topLocator);

  if (!referencedDecl.getDecl()) {
    // Do another check in case we have a curried call from binding a function
    // reference to a variable, for example:
    //
    //   class C {
    //     func instanceFunc(p1: Int, p2: Int) {}
    //   }
    //   func t(c: C) {
    //     C.instanceFunc(c)#^COMPLETE^#
    //   }
    //
    // We need to get the referenced function so we can complete the argument
    // labels. (Note that the requirement to have labels in the curried call
    // seems inconsistent with the removal of labels from function types.
    // If this changes the following code could be removed).
    if (auto *CE = dyn_cast<CallExpr>(semanticExpr)) {
      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(CE->getFn())) {
        if (isa<TypeExpr>(UDE->getBase())) {
          auto udeLocator = cs.getConstraintLocator(UDE);
          auto udeRefDecl = solution.resolveLocatorToDecl(udeLocator);
          if (auto *FD = dyn_cast_or_null<FuncDecl>(udeRefDecl.getDecl())) {
            if (FD->isInstanceMember())
              referencedDecl = udeRefDecl;
          }
        }
      }
    }
  }

  // Recover the original type if needed.
  recoverOriginalType();
  return exprType;
}
