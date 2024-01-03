
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/IfConfigClause.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/APFloat.h"

using namespace swift;

namespace {
#define CHECK_PTR_PROP(Prop)                                                   \
  if (!doIt(lhs->Prop, rhs->Prop, "->" #Prop))                                 \
    return false;

#define CHECK_PROP(Prop)                                                       \
  if (!doIt(lhs.Prop, rhs.Prop, "." #Prop))                                    \
    return false;

#define CHECK_PTR_AS_BOOL()                                                    \
  if (bool(lhs) != bool(rhs))                                                  \
    return false;                                                              \
  if (!bool(lhs) && !bool())                                                   \
    return true;

#define CHECK_PTR_EQUIV()                                                      \
  if (auto result = checkPtr(lhs, rhs))                                        \
    return result.value();

#define CHECK_IMPL_AS(Ty)                                                      \
  if (!check##Ty(cast<Ty>(lhs), cast<Ty>(rhs)))                                \
    return false;

class ASTComparison {
  llvm::DenseSet<std::pair<const void *, const void *>> equivalentPtrPairs;
  llvm::DenseSet<const void *> anyEquivalentPtrs;

  SmallVector<StringRef> pathStack;
  std::string err;

public:
  ASTComparison() {}

  llvm::Error getError() const {
    return llvm::createStringError(llvm::inconvertibleErrorCode(), err);
  }

  /// Registers the pair as a pair of equivalent pointer.
  /// If the pair is already registered, returns 'true'. If only either of it is
  /// registered, returns 'false', otherwise returns nullopt.
  /// After this function returns, the pair is always considered equivalent.
  /// So recursive checking is safe.
  /// E.g. VarDecl.Accessors -> AccessorDecl.Storage -> VarDecl.Accessors.
  std::optional<bool> checkPtr(const void *lhs, const void *rhs) {
    // FIXME: Should we really always support the same pointer check?
    // Since we're comparing different AST instance, it _should_ not contain
    // the same instance, but it may have links to things outside the tree.
    if (lhs == rhs)
      return true;

    if (!lhs || !rhs)
      return false;

    if (!equivalentPtrPairs.insert({lhs, rhs}).second)
      return true;

    // If either of it was considered equivalent with another pointer, that
    // should not be equivalent with others.
    if (!anyEquivalentPtrs.insert(lhs).second ||
        !anyEquivalentPtrs.insert(rhs).second)
      return false;

    return std::nullopt;
  }

  bool withPathStack(StringRef path, llvm::function_ref<bool()> body) {
    pathStack.push_back(path);
    SWIFT_DEFER { pathStack.pop_back(); };

    if (body())
      return true;
    err = "detected difference at " + llvm::join(pathStack, "");
    return false;
  }

  template <typename T>
  bool doIt(T lhs, T rhs, StringRef path) {
    return withPathStack(path, [&]() { return check(lhs, rhs); });
  }

  // This specialization is for disambiguation of 'check(lhs, rhs)'.
  // Some types e.g. 'FuncDecl' inherits 'DeclContext' and 'Decl' which makes
  // 'check(lhs, rhs)' ambiguous.
  template <>
  bool doIt<DeclContext *>(DeclContext *lhs, DeclContext *rhs, StringRef path) {
    return withPathStack(path, [&]() { return checkDeclContext(lhs, rhs); });
  }

#define DECLARE_CHECK_FUNC(TYPE) bool check(TYPE lhs, TYPE rhs)
#define DECLARE_CHECK_FUNC_EQ(TYPE)                                            \
  DECLARE_CHECK_FUNC(TYPE) { return lhs == rhs; }

  template <typename T>
  DECLARE_CHECK_FUNC(std::optional<T>) {
    if (lhs.has_value() != rhs.has_value())
      return false;
    if (!lhs.has_value())
      return true;
    return doIt(lhs.value(), rhs.value(), ".value()");
  }

  template <typename T>
  DECLARE_CHECK_FUNC(ArrayRef<T>) {
    if (lhs.size() != rhs.size())
      return false;

    for (auto i : indices(lhs))
      if (!doIt(lhs[i], rhs[i], "[" + std::to_string(i) + "]"))
        return false;

    return true;
  }

  template <typename T>
  DECLARE_CHECK_FUNC(Located<T>) {
    CHECK_PROP(Loc);
    CHECK_PROP(Item);
    return true;
  }

  template <typename T>
  DECLARE_CHECK_FUNC(OptionSet<T>) {
    CHECK_PROP(toRaw());
    return true;
  }

  DECLARE_CHECK_FUNC_EQ(SourceLoc)
  DECLARE_CHECK_FUNC_EQ(SourceRange)
  DECLARE_CHECK_FUNC_EQ(StringRef)
  DECLARE_CHECK_FUNC_EQ(ExprKind)
  DECLARE_CHECK_FUNC_EQ(StmtKind)
  DECLARE_CHECK_FUNC_EQ(DeclKind)
  DECLARE_CHECK_FUNC_EQ(TypeReprKind)
  DECLARE_CHECK_FUNC_EQ(PatternKind)
  DECLARE_CHECK_FUNC_EQ(DeclContextKind)
  DECLARE_CHECK_FUNC_EQ(uint8_t)
  DECLARE_CHECK_FUNC_EQ(uint16_t)
  DECLARE_CHECK_FUNC_EQ(uint32_t)
  DECLARE_CHECK_FUNC_EQ(uint64_t)
  DECLARE_CHECK_FUNC_EQ(size_t)
  DECLARE_CHECK_FUNC_EQ(bool)
  DECLARE_CHECK_FUNC_EQ(llvm::APInt)
  DECLARE_CHECK_FUNC_EQ(llvm::APFloat)
  DECLARE_CHECK_FUNC_EQ(DeclRefKind)
  DECLARE_CHECK_FUNC_EQ(FunctionRefKind)
  DECLARE_CHECK_FUNC_EQ(VarDecl::Introducer)
  DECLARE_CHECK_FUNC_EQ(StaticSpellingKind)
  DECLARE_CHECK_FUNC_EQ(AccessorKind)
  DECLARE_CHECK_FUNC_EQ(ImportKind)
  DECLARE_CHECK_FUNC_EQ(Associativity)
  DECLARE_CHECK_FUNC_EQ(OperatorFixity);
  DECLARE_CHECK_FUNC_EQ(CaseParentKind);
  DECLARE_CHECK_FUNC_EQ(ClosureExpr::BodyState);
  DECLARE_CHECK_FUNC_EQ(CheckedCastKind);
  DECLARE_CHECK_FUNC_EQ(ObjCSelectorExpr::ObjCSelectorKind);
  DECLARE_CHECK_FUNC_EQ(DeclName);
  DECLARE_CHECK_FUNC_EQ(DeclNameRef);
  DECLARE_CHECK_FUNC_EQ(KeyPathExpr::Component::Kind);
  DECLARE_CHECK_FUNC_EQ(StmtConditionElement::ConditionKind);
  DECLARE_CHECK_FUNC_EQ(RequirementReprKind);
  DECLARE_CHECK_FUNC_EQ(LayoutConstraint);
  DECLARE_CHECK_FUNC_EQ(ProtocolConformanceRef);
  DECLARE_CHECK_FUNC_EQ(DiagID);
  DECLARE_CHECK_FUNC_EQ(ParamSpecifier);

  DECLARE_CHECK_FUNC(SubstitutionMap);
  DECLARE_CHECK_FUNC(ConcreteDeclRef);
  DECLARE_CHECK_FUNC(DeclNameLoc);
  DECLARE_CHECK_FUNC(ArgumentList *);
  DECLARE_CHECK_FUNC(const DeclAttributes &);
  DECLARE_CHECK_FUNC(InheritedTypes);
  DECLARE_CHECK_FUNC(GenericParamList *);
  DECLARE_CHECK_FUNC(PrimaryAssociatedTypeName);
  DECLARE_CHECK_FUNC(TrailingWhereClause *);
  DECLARE_CHECK_FUNC(CaptureInfo);
  DECLARE_CHECK_FUNC(ImportPath);
  DECLARE_CHECK_FUNC(IfConfigClause);
  DECLARE_CHECK_FUNC(PrecedenceGroupDecl::Relation);
  DECLARE_CHECK_FUNC(PatternBindingEntry);
  DECLARE_CHECK_FUNC(ASTNode);
  DECLARE_CHECK_FUNC(StmtConditionElement);
  DECLARE_CHECK_FUNC(CaseLabelItem);
  DECLARE_CHECK_FUNC(CaptureListEntry);
  DECLARE_CHECK_FUNC(ApplyIsolationCrossing);
  DECLARE_CHECK_FUNC(KeyPathExpr::Component);
  DECLARE_CHECK_FUNC(InheritedEntry);
  DECLARE_CHECK_FUNC(RequirementRepr);
  DECLARE_CHECK_FUNC(Type);
  DECLARE_CHECK_FUNC(CapturedValue);
  DECLARE_CHECK_FUNC(LayoutConstraintLoc);
  DECLARE_CHECK_FUNC(TupleTypeReprElement);
  DECLARE_CHECK_FUNC(TypeAttributes);
  DECLARE_CHECK_FUNC(TuplePatternElt);

#define DECLARE_CHECK_IMPL_FUNC(TYPE) bool check##TYPE(TYPE *lhs, TYPE *rhs)

  DECLARE_CHECK_IMPL_FUNC(DeclContext) {
    CHECK_PTR_EQUIV();

    // If the parent and the kind are equivalent, 'lhs' and 'rhs' are at least
    // at the same hierarchy.
    CHECK_PTR_PROP(getParent());
    CHECK_PTR_PROP(getContextKind());

    // Still here, it doesn't really mean these decl contexts are equivalent,
    // but there's no indicator anymore. So let's consider they are equivalent.
    // Even if not, there must be some other difference in the tree.
    return true;
  }

  DECLARE_CHECK_IMPL_FUNC(FreestandingMacroExpansion);

  // MARK: - Exprs.

  DECLARE_CHECK_FUNC(Expr *) {
    CHECK_PTR_EQUIV();
    CHECK_PTR_PROP(getKind());
    CHECK_PTR_PROP(isImplicit());
    CHECK_PTR_PROP(getLoc());
    CHECK_PTR_PROP(getSourceRange());

    switch (lhs->getKind()) {
#define EXPR(Id, Parent)                                                       \
  case ExprKind::Id:                                                           \
    return check##Id##Expr(cast<Id##Expr>(lhs), cast<Id##Expr>(rhs));
#include "swift/AST/ExprNodes.def"
    }
  }

#define ABSTRACT_EXPR(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Expr);
#define EXPR(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Expr);
#include "swift/AST/ExprNodes.def"

  // MARK: - Decls.

  DECLARE_CHECK_FUNC(Decl *) {
    CHECK_PTR_EQUIV();
    CHECK_PTR_PROP(getDeclContext());
    CHECK_PTR_PROP(getKind());
    CHECK_PTR_PROP(getAttrs());
    CHECK_PTR_PROP(isImplicit());
    CHECK_PTR_PROP(isHoisted());
    CHECK_PTR_PROP(isInvalid());

    // Dispatch to checkXXXDecl(lhs, rhs);
    switch (lhs->getKind()) {
#define DECL(Id, Parent)                                                       \
  case DeclKind::Id:                                                           \
    return check##Id##Decl(cast<Id##Decl>(lhs), cast<Id##Decl>(rhs));
#include "swift/AST/DeclNodes.def"
    }
  }

#define ABSTRACT_DECL(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Decl);
#define DECL(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Decl);
#include "swift/AST/DeclNodes.def"

  // MARK: - Stmts.

  DECLARE_CHECK_FUNC(Stmt *) {
    CHECK_PTR_PROP(getKind());
    CHECK_PTR_PROP(getSourceRange());
    CHECK_PTR_PROP(isImplicit());

    switch (lhs->getKind()) {
#define STMT(Id, Parent)                                                       \
  case StmtKind::Id:                                                           \
    return check##Id##Stmt(cast<Id##Stmt>(lhs), cast<Id##Stmt>(rhs));
#include "swift/AST/StmtNodes.def"
    }
  }

#define ABSTRACT_STMT(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Stmt);
#define STMT(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Stmt);
#include "swift/AST/StmtNodes.def"

  DECLARE_CHECK_FUNC(TypeRepr *) {
    CHECK_PTR_EQUIV();
    CHECK_PTR_PROP(getLoc());
    CHECK_PTR_PROP(getSourceRange());
    CHECK_PTR_PROP(isInvalid());
    CHECK_PTR_PROP(isWarnedAbout());

    switch (lhs->getKind()) {
#define TYPEREPR(Id, Parent)                                                   \
  case TypeReprKind::Id:                                                       \
    return check##Id##TypeRepr(cast<Id##TypeRepr>(lhs),                        \
                               cast<Id##TypeRepr>(rhs));
#include "swift/AST/TypeReprNodes.def"
    }
  }

#define ABSTRACT_TYPEREPR(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##TypeRepr);
#define TYPEREPR(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##TypeRepr);
#include "swift/AST/TypeReprNodes.def"

  DECLARE_CHECK_FUNC(Pattern *) {
    CHECK_PTR_PROP(getKind());
    CHECK_PTR_PROP(isImplicit());
    CHECK_PTR_PROP(hasType());

    switch (lhs->getKind()) {
#define PATTERN(Id, Parent)                                                    \
  case PatternKind::Id:                                                        \
    return check##Id##Pattern(cast<Id##Pattern>(lhs), cast<Id##Pattern>(rhs));
#include "swift/AST/PatternNodes.def"
    }
  }

#define PATTERN(Id, Parent) DECLARE_CHECK_IMPL_FUNC(Id##Pattern);
#include "swift/AST/PatternNodes.def"

  DECLARE_CHECK_FUNC(Type *) {
    CHECK_PTR_PROP(getString());
    return true;
  }

  DECLARE_CHECK_FUNC(GenericSignature) {
    CHECK_PROP(getAsString());
    return true;
  }
};

#define DEFINE_CHECK_FUNC(TYPE) bool ASTComparison::check(TYPE lhs, TYPE rhs)

DEFINE_CHECK_FUNC(SubstitutionMap) {
  CHECK_PROP(getReplacementTypes());
  CHECK_PROP(getConformances());
  CHECK_PROP(getGenericSignature());
  return true;
}

DEFINE_CHECK_FUNC(ConcreteDeclRef) {
  CHECK_PROP(getDecl());
  CHECK_PROP(getSubstitutions());
  return true;
}

DEFINE_CHECK_FUNC(DeclNameLoc) {
  CHECK_PROP(getBaseNameLoc());
  CHECK_PROP(getLParenLoc());
  CHECK_PROP(getRParenLoc());
  CHECK_PROP(getArgumentLabelLocs());
  return true;
}

DEFINE_CHECK_FUNC(ArgumentList *) {
  CHECK_PTR_PROP(getLParenLoc());
  CHECK_PTR_PROP(getRParenLoc());
  CHECK_PTR_PROP(isImplicit());
  CHECK_PTR_PROP(hasAnyArgumentLabels());
  CHECK_PTR_PROP(getLoc());
  CHECK_PTR_PROP(getSourceRange());

  return true;
}
DEFINE_CHECK_FUNC(const DeclAttributes &) {
  int i = 0;
  auto l = lhs.begin(), r = rhs.begin();
  for (; l != lhs.end() && r != rhs.end(); ++i, ++l, ++r)
    if (!doIt(*l, *r, "[" + std::to_string(i) + "]"))
      return false;
  if (!doIt(l != lhs.end(), r != rhs.end(), "[" + std::to_string(i) + "]"))
    return false;

  return true;
}

DEFINE_CHECK_FUNC(InheritedTypes) {
  CHECK_PROP(getEntries());
  return true;
}
DEFINE_CHECK_FUNC(GenericParamList *) {
  CHECK_PTR_PROP(getLAngleLoc());
  CHECK_PTR_PROP(getRAngleLoc());
  CHECK_PTR_PROP(getParams());
  CHECK_PTR_PROP(getRequirements());
  CHECK_PTR_PROP(getWhereLoc());
  CHECK_PTR_PROP(getOuterParameters());
  CHECK_PTR_PROP(getSourceRange());

  return true;
}

DEFINE_CHECK_FUNC(PrimaryAssociatedTypeName) {
  CHECK_PROP(first);
  CHECK_PROP(second);
  return true;
}

DEFINE_CHECK_FUNC(TrailingWhereClause *) {
  CHECK_PTR_PROP(getWhereLoc());
  CHECK_PTR_PROP(getRequirements());
  CHECK_PTR_PROP(getSourceRange());
  return true;
}

DEFINE_CHECK_FUNC(CaptureInfo) {
  CHECK_PROP(getCaptures());
  CHECK_PROP(getDynamicSelfType());
  CHECK_PROP(getOpaqueValue());
  CHECK_PROP(hasBeenComputed());
  CHECK_PROP(hasGenericParamCaptures());
  return true;
}

DEFINE_CHECK_FUNC(ImportPath) {
  CHECK_PROP(getRaw())
  return true;
}

DEFINE_CHECK_FUNC(IfConfigClause) {
  CHECK_PROP(isActive);
  CHECK_PROP(Cond);
  CHECK_PROP(Elements);
  CHECK_PROP(Loc);
  return true;
}

DEFINE_CHECK_FUNC(PrecedenceGroupDecl::Relation) {
  CHECK_PROP(Group);
  CHECK_PROP(Name);
  CHECK_PROP(NameLoc);
  return true;
}

DEFINE_CHECK_FUNC(PatternBindingEntry) {
  CHECK_PROP(getPattern());
  CHECK_PROP(getInit());
  return true;
}

DEFINE_CHECK_FUNC(StmtConditionElement) {
  CHECK_PROP(getKind());
  switch (lhs.getKind()) {
  case StmtConditionElement::CK_Boolean:
    CHECK_PROP(getBooleanOrNull());
    break;
  case StmtConditionElement::CK_PatternBinding:
    CHECK_PROP(getPatternBinding());
    break;
  case StmtConditionElement::CK_Availability:
    CHECK_PROP(getAvailability());
    break;
  case StmtConditionElement::CK_HasSymbol:
    CHECK_PROP(getHasSymbolInfo());
    break;
  }
  return true;
}

DEFINE_CHECK_FUNC(CaseLabelItem) {
  CHECK_PROP(getPattern());
  CHECK_PROP(getWhereLoc());
  CHECK_PROP(getGuardExpr());
  CHECK_PROP(isDefault());
  CHECK_PROP(isPatternResolved());
  return true;
}
DEFINE_CHECK_FUNC(CaptureListEntry) {
  CHECK_PROP(PBD);
  return true;
}
DEFINE_CHECK_FUNC(ApplyIsolationCrossing) {
  CHECK_PROP(getCallerIsolation());
  CHECK_PROP(getCalleeIsolation());
  return true;
}
DEFINE_CHECK_FUNC(KeyPathExpr::Component) {
  CHECK_PROP(getKind());
  CHECK_PROP(getLoc());
  CHECK_PROP(getComponentType());

  switch (lhs.getKind()) {
  case KeyPathExpr::Component::Kind::Invalid:
    break;
  case KeyPathExpr::Component::Kind::UnresolvedProperty:
    CHECK_PROP(getUnresolvedDeclName());
    break;
  case KeyPathExpr::Component::Kind::UnresolvedSubscript:
    CHECK_PROP(getSubscriptArgs());
    break;
  case KeyPathExpr::Component::Kind::Property:
    CHECK_PROP(getDeclRef());
    break;
  case KeyPathExpr::Component::Kind::Subscript:
    CHECK_PROP(getDeclRef());
    CHECK_PROP(getSubscriptIndexHashableConformances());
    break;
  case KeyPathExpr::Component::Kind::OptionalForce:
    break;
  case KeyPathExpr::Component::Kind::OptionalChain:
    break;
  case KeyPathExpr::Component::Kind::OptionalWrap:
    break;
  case KeyPathExpr::Component::Kind::Identity:
    break;
  case KeyPathExpr::Component::Kind::TupleElement:
    CHECK_PROP(getTupleIndex());
    break;
  case KeyPathExpr::Component::Kind::DictionaryKey:
    CHECK_PROP(getUnresolvedDeclName());
    break;
  case KeyPathExpr::Component::Kind::CodeCompletion:
    break;
  }
  return true;
}

DEFINE_CHECK_FUNC(Type) {
  CHECK_PROP(getPointer());
  return true;
}

DEFINE_CHECK_FUNC(CapturedValue) {
  CHECK_PROP(getLoc());
  CHECK_PROP(isDirect());
  CHECK_PROP(isNoEscape());
  CHECK_PROP(isOpaqueValue());
  CHECK_PROP(isDynamicSelfMetadata());

  if (lhs.isDynamicSelfMetadata()) {
  } else if (lhs.isOpaqueValue()) {
    CHECK_PROP(getOpaqueValue());
  } else {
    CHECK_PROP(getDecl());
  }
  return true;
}

DEFINE_CHECK_FUNC(RequirementRepr) {
  CHECK_PROP(getKind());
  CHECK_PROP(getSeparatorLoc());
  CHECK_PROP(isInvalid());
  CHECK_PROP(isExpansionPattern());
  switch (lhs.getKind()) {
  case swift::RequirementReprKind::TypeConstraint:
    CHECK_PROP(getSubjectRepr());
    CHECK_PROP(getConstraintRepr());
    break;
  case swift::RequirementReprKind::SameType:
    CHECK_PROP(getFirstTypeRepr());
    CHECK_PROP(getSecondTypeRepr());
    break;
  case swift::RequirementReprKind::LayoutConstraint:
    CHECK_PROP(getSubjectRepr());
    CHECK_PROP(getLayoutConstraintLoc());
    break;
  }
  return true;
}

DEFINE_CHECK_FUNC(LayoutConstraintLoc) {
  CHECK_PROP(getLoc());
  CHECK_PROP(getLayoutConstraint());
  return true;
}

DEFINE_CHECK_FUNC(InheritedEntry) {
  CHECK_PROP(getTypeRepr());
  CHECK_PROP(isRetroactive);
  CHECK_PROP(isUnchecked);
  return true;
}

DEFINE_CHECK_FUNC(ASTNode) {
  CHECK_PROP(isNull());
  CHECK_PROP(dyn_cast<Expr *>());
  CHECK_PROP(dyn_cast<Stmt *>());
  CHECK_PROP(dyn_cast<Decl *>());
  CHECK_PROP(dyn_cast<StmtConditionElement *>());
  CHECK_PROP(dyn_cast<CaseLabelItem *>());
  return true;
}

DEFINE_CHECK_FUNC(TypeAttributes) { return true; }

DEFINE_CHECK_FUNC(TuplePatternElt) {
  CHECK_PROP(getLabel());
  CHECK_PROP(getLabelLoc());
  CHECK_PROP(getPattern());
  return true;
}

#define DEFINE_CHECK_IMPL_FUNC(TYPE)                                           \
  bool ASTComparison::check##TYPE(TYPE *lhs, TYPE *rhs)

DEFINE_CHECK_IMPL_FUNC(FreestandingMacroExpansion) {
  CHECK_PTR_PROP(getPoundLoc());
  CHECK_PTR_PROP(getModuleName());
  CHECK_PTR_PROP(getModuleNameLoc());
  CHECK_PTR_PROP(getMacroName());
  CHECK_PTR_PROP(getMacroNameLoc());
  CHECK_PTR_PROP(getGenericArgs());
  CHECK_PTR_PROP(getGenericArgsRange());
  CHECK_PTR_PROP(getArgs());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ValueDecl) {
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getLocalDiscriminator());
  CHECK_PTR_PROP(getResultTypeRepr())
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TypeDecl) {
  CHECK_IMPL_AS(ValueDecl);
  CHECK_PTR_PROP(getInherited());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(GenericTypeDecl) {
  CHECK_IMPL_AS(TypeDecl);
  CHECK_PTR_PROP(getParsedGenericParams());
  CHECK_PTR_PROP(getTrailingWhereClause());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(NominalTypeDecl) {
  CHECK_IMPL_AS(GenericTypeDecl);
  CHECK_PTR_PROP(getBraces());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(EnumDecl) {
  CHECK_IMPL_AS(NominalTypeDecl);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(StructDecl) {
  CHECK_IMPL_AS(NominalTypeDecl);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ClassDecl) {
  CHECK_IMPL_AS(NominalTypeDecl);
  CHECK_PTR_PROP(isExplicitActor());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ProtocolDecl) {
  CHECK_IMPL_AS(NominalTypeDecl);
  CHECK_PTR_PROP(getPrimaryAssociatedTypeNames());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BuiltinTupleDecl) {
  llvm_unreachable("BuiltinTupleDecl should not be in parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(OpaqueTypeDecl) {
  llvm_unreachable("OpaqueTypeDecl should not be in parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(TypeAliasDecl) {
  CHECK_IMPL_AS(GenericTypeDecl);
  CHECK_PTR_PROP(getEqualLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(GenericTypeParamDecl) {
  CHECK_IMPL_AS(TypeDecl);
  CHECK_PTR_PROP(getDepth());
  CHECK_PTR_PROP(getIndex());
  CHECK_PTR_PROP(isOpaqueType());
  CHECK_PTR_PROP(getOpaqueTypeRepr());
  CHECK_PTR_PROP(isParameterPack());
  CHECK_PTR_PROP(getEachLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AssociatedTypeDecl) {
  CHECK_IMPL_AS(TypeDecl);
  CHECK_PTR_PROP(getDefaultDefinitionTypeRepr());
  CHECK_PTR_PROP(getTrailingWhereClause());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ModuleDecl) {
  llvm_unreachable("ModuleDecl should not be in parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(AbstractStorageDecl) {
  CHECK_IMPL_AS(ValueDecl);
  CHECK_PTR_PROP(isStatic());
  CHECK_PTR_PROP(getAllAccessors())
  CHECK_PTR_PROP(getBracesRange())
  return true;
}

DEFINE_CHECK_IMPL_FUNC(VarDecl) {
  CHECK_IMPL_AS(AbstractStorageDecl);
  CHECK_PTR_PROP(getIntroducer());
  CHECK_PTR_PROP(getParentPatternBinding());
  CHECK_PTR_PROP(getParentVarDecl());
  CHECK_PTR_PROP(getParentPatternStmt());
  CHECK_PTR_PROP(getParentCaptureList());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ParamDecl) {
  CHECK_IMPL_AS(VarDecl);
  CHECK_PTR_PROP(getArgumentName());
  CHECK_PTR_PROP(getArgumentNameLoc());
  CHECK_PTR_PROP(getParameterName());
  CHECK_PTR_PROP(getParameterNameLoc());
  CHECK_PTR_PROP(isDestructured());
  CHECK_PTR_PROP(isIsolated());
  CHECK_PTR_PROP(isCompileTimeConst());
  CHECK_PTR_PROP(hasResultDependsOn());
  CHECK_PTR_PROP(getTypeRepr());
  CHECK_PTR_PROP(isAutoClosure());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SubscriptDecl) {
  CHECK_IMPL_AS(AbstractStorageDecl);
  CHECK_PTR_PROP(getParsedGenericParams());
  CHECK_PTR_PROP(getTrailingWhereClause());
  CHECK_PTR_PROP(getStaticLoc());
  CHECK_PTR_PROP(getIndices());
  CHECK_PTR_PROP(getSubscriptLoc());
  CHECK_PTR_PROP(getElementTypeRepr());
  CHECK_PTR_PROP(getStaticSpelling());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AbstractFunctionDecl) {
  CHECK_IMPL_AS(ValueDecl);
  CHECK_PTR_PROP(getParameters());
  CHECK_PTR_PROP(hasSingleExpressionBody());
  CHECK_PTR_PROP(hasAsync());
  CHECK_PTR_PROP(hasThrows());
  CHECK_PTR_PROP(getAsyncLoc());
  CHECK_PTR_PROP(getThrowsLoc());
  CHECK_PTR_PROP(getCaptureInfo());
  CHECK_PTR_PROP(getThrownTypeRepr());
  CHECK_PTR_PROP(getBodySourceRange())
  CHECK_PTR_PROP(getBody(/*canSynthesize=false*/));
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ConstructorDecl) {
  CHECK_IMPL_AS(AbstractFunctionDecl);
  CHECK_PTR_PROP(getFailabilityLoc());
  CHECK_PTR_PROP(isFailable());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DestructorDecl) {
  CHECK_IMPL_AS(AbstractFunctionDecl);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(FuncDecl) {
  CHECK_IMPL_AS(AbstractFunctionDecl);
  CHECK_PTR_PROP(getStaticLoc());
  CHECK_PTR_PROP(getStaticSpelling());
  CHECK_PTR_PROP(getFuncLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AccessorDecl) {
  CHECK_IMPL_AS(FuncDecl);
  CHECK_PTR_PROP(getAccessorKeywordLoc());
  CHECK_PTR_PROP(getAccessorKind());
  CHECK_PTR_PROP(getStorage());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MacroDecl) {
  CHECK_IMPL_AS(ValueDecl);
  CHECK_PTR_PROP(getParameterList());
  //(private)  CHECK_PTR_PROP(definition);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(EnumElementDecl) {
  CHECK_IMPL_AS(ValueDecl);
  CHECK_PTR_PROP(getParameterList());
  CHECK_PTR_PROP(getRawValueUnchecked());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ExtensionDecl) {
  CHECK_PTR_PROP(getParsedGenericParams());
  CHECK_PTR_PROP(getTrailingWhereClause());
  CHECK_PTR_PROP(getBraces());
  CHECK_PTR_PROP(getExtendedTypeRepr());
  CHECK_PTR_PROP(getInherited());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TopLevelCodeDecl) {
  CHECK_PTR_PROP(getBody());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ImportDecl) {
  CHECK_PTR_PROP(getKindLoc());
  CHECK_PTR_PROP(getImportKind());
  CHECK_PTR_PROP(getImportPath());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(IfConfigDecl) {
  CHECK_PTR_PROP(getClauses());
  CHECK_PTR_PROP(hadMissingEnd());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PoundDiagnosticDecl) {
  CHECK_PTR_PROP(isError());
  CHECK_PTR_PROP(getMessage());
  CHECK_PTR_PROP(hasBeenEmitted());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(PrecedenceGroupDecl) {
  CHECK_PTR_PROP(getPrecedenceGroupLoc());
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getLBraceLoc());
  CHECK_PTR_PROP(getRBraceLoc());
  CHECK_PTR_PROP(getAssociativityKeywordLoc());
  CHECK_PTR_PROP(getAssociativityValueLoc());
  CHECK_PTR_PROP(getAssociativity());
  CHECK_PTR_PROP(getAssignmentKeywordLoc());
  CHECK_PTR_PROP(getAssignmentValueLoc());
  CHECK_PTR_PROP(isAssignment());
  CHECK_PTR_PROP(getHigherThanLoc());
  CHECK_PTR_PROP(getHigherThan());
  CHECK_PTR_PROP(getLowerThanLoc());
  CHECK_PTR_PROP(getLowerThan());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(MissingDecl) { return true; }

DEFINE_CHECK_IMPL_FUNC(MissingMemberDecl) {
  llvm_unreachable("MissingMemberDecl should not appear in parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(PatternBindingDecl) {
  CHECK_PTR_PROP(getStaticLoc());
  CHECK_PTR_PROP(isStatic());
  CHECK_PTR_PROP(getStaticSpelling());
  CHECK_PTR_PROP(getPatternList());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(EnumCaseDecl) {
  CHECK_PTR_PROP(getElements());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(OperatorDecl) {
  CHECK_PTR_PROP(getOperatorLoc());
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getFixity());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(InfixOperatorDecl) {
  CHECK_IMPL_AS(OperatorDecl);
  CHECK_PTR_PROP(getColonLoc());
  CHECK_PTR_PROP(getPrecedenceGroupName());
  CHECK_PTR_PROP(getPrecedenceGroupLoc());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(PrefixOperatorDecl) {
  CHECK_IMPL_AS(OperatorDecl);

  return true;
}

DEFINE_CHECK_IMPL_FUNC(PostfixOperatorDecl) {
  CHECK_IMPL_AS(OperatorDecl);

  return true;
}

DEFINE_CHECK_IMPL_FUNC(MacroExpansionDecl) {
  CHECK_IMPL_AS(FreestandingMacroExpansion);
  CHECK_PTR_PROP(getRawDiscriminator());

  return true;
}

// MARK: - Statements

DEFINE_CHECK_IMPL_FUNC(BraceStmt) {
  CHECK_PTR_PROP(getElements());
  CHECK_PTR_PROP(getLBraceLoc());
  CHECK_PTR_PROP(getRBraceLoc());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ReturnStmt) {
  CHECK_PTR_PROP(getReturnLoc());
  CHECK_PTR_PROP(getResult());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ThenStmt) {
  CHECK_PTR_PROP(getThenLoc());
  CHECK_PTR_PROP(getResult());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(YieldStmt) {
  CHECK_PTR_PROP(getYieldLoc());
  CHECK_PTR_PROP(getYields());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DeferStmt) {
  CHECK_PTR_PROP(getDeferLoc());
  CHECK_PTR_PROP(getTempDecl());
  CHECK_PTR_PROP(getCallExpr());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(LabeledStmt) {
  CHECK_PTR_PROP(getLabelInfo());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LabeledConditionalStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getCond());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(IfStmt) {
  CHECK_IMPL_AS(LabeledConditionalStmt);
  CHECK_PTR_PROP(getIfLoc());
  CHECK_PTR_PROP(getElseLoc());
  CHECK_PTR_PROP(getThenStmt());
  CHECK_PTR_PROP(getElseStmt());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(GuardStmt) {
  CHECK_IMPL_AS(LabeledConditionalStmt);
  CHECK_PTR_PROP(getGuardLoc());
  CHECK_PTR_PROP(getBody());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(WhileStmt) {
  CHECK_IMPL_AS(LabeledConditionalStmt);
  CHECK_PTR_PROP(getWhileLoc());
  CHECK_PTR_PROP(getBody());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DoStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getDoLoc());
  CHECK_PTR_PROP(getBody());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DoCatchStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getDoLoc());
  CHECK_PTR_PROP(getBody());
  CHECK_PTR_PROP(getCatches());
  CHECK_PTR_PROP(getThrowsLoc());
  CHECK_PTR_PROP(getCaughtTypeRepr());
  CHECK_PTR_PROP(getDeclContext());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(RepeatWhileStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getRepeatLoc());
  CHECK_PTR_PROP(getBody());
  CHECK_PTR_PROP(getCond());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ForEachStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getForLoc());
  CHECK_PTR_PROP(getPattern());
  CHECK_PTR_PROP(getTryLoc());
  CHECK_PTR_PROP(getAwaitLoc());
  CHECK_PTR_PROP(getInLoc());
  CHECK_PTR_PROP(getParsedSequence());
  CHECK_PTR_PROP(getWhereLoc());
  CHECK_PTR_PROP(getWhere());
  CHECK_PTR_PROP(getBody());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(SwitchStmt) {
  CHECK_IMPL_AS(LabeledStmt);
  CHECK_PTR_PROP(getSwitchLoc());
  CHECK_PTR_PROP(getSubjectExpr());
  CHECK_PTR_PROP(getLBraceLoc());
  CHECK_PTR_PROP(getRBraceLoc());
  CHECK_PTR_PROP(getRawCases());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(CaseStmt) {
  CHECK_PTR_PROP(getLoc());
  CHECK_PTR_PROP(getParentKind());
  CHECK_PTR_PROP(getCaseLabelItems());
  CHECK_PTR_PROP(getItemTerminatorLoc());
  CHECK_PTR_PROP(getBody());
  CHECK_PTR_PROP(getCaseBodyVariablesOrEmptyArray());
  CHECK_PTR_PROP(getFallthroughStmt());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(BreakStmt) {
  CHECK_PTR_PROP(getLoc());
  CHECK_PTR_PROP(getTargetName());
  CHECK_PTR_PROP(getTargetLoc());
  CHECK_PTR_PROP(getDeclContext());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ContinueStmt) {
  CHECK_PTR_PROP(getLoc());
  CHECK_PTR_PROP(getTargetName());
  CHECK_PTR_PROP(getTargetLoc());
  CHECK_PTR_PROP(getDeclContext());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(FallthroughStmt) {
  CHECK_PTR_PROP(getLoc());
  CHECK_PTR_PROP(getFallthroughSource());
  CHECK_PTR_PROP(getFallthroughDest());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(FailStmt) {
  CHECK_PTR_PROP(getLoc());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ThrowStmt) {
  CHECK_PTR_PROP(getThrowLoc());
  CHECK_PTR_PROP(getSubExpr());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DiscardStmt) {
  CHECK_PTR_PROP(getDiscardLoc());
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getInnermostMethodContext());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(PoundAssertStmt) {
  CHECK_PTR_PROP(getCondition());
  CHECK_PTR_PROP(getMessage());

  return true;
}

// MARK: - Expressions

DEFINE_CHECK_IMPL_FUNC(ErrorExpr) {
  CHECK_PTR_PROP(getOriginalExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LiteralExpr) {
  CHECK_PTR_PROP(getInitializer());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(NilLiteralExpr) {
  CHECK_IMPL_AS(LiteralExpr);

  return true;
}

DEFINE_CHECK_IMPL_FUNC(BuiltinLiteralExpr) {
  CHECK_IMPL_AS(LiteralExpr);
  CHECK_PTR_PROP(getBuiltinInitializer());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(BooleanLiteralExpr) {
  CHECK_IMPL_AS(BuiltinLiteralExpr);
  CHECK_PTR_PROP(getValue());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(NumberLiteralExpr) {
  CHECK_IMPL_AS(BuiltinLiteralExpr);
  CHECK_PTR_PROP(getDigitsText());
  CHECK_PTR_PROP(getMinusLoc());
  CHECK_PTR_PROP(getDigitsLoc());
  CHECK_PTR_PROP(isNegative());
  CHECK_PTR_PROP(isExplicitConversion());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(IntegerLiteralExpr) {
  CHECK_IMPL_AS(NumberLiteralExpr);

  return true;
}

DEFINE_CHECK_IMPL_FUNC(FloatLiteralExpr) {
  CHECK_IMPL_AS(NumberLiteralExpr);

  return true;
}

DEFINE_CHECK_IMPL_FUNC(StringLiteralExpr) {
  CHECK_IMPL_AS(BuiltinLiteralExpr);
  CHECK_PTR_PROP(getValue());
  CHECK_PTR_PROP(getEncoding());
  CHECK_PTR_PROP(isSingleUnicodeScalar());
  CHECK_PTR_PROP(isSingleExtendedGraphemeCluster());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(MagicIdentifierLiteralExpr) {
  CHECK_IMPL_AS(BuiltinLiteralExpr);
  CHECK_PTR_PROP(getKind());
  if (lhs->isString())
    CHECK_PTR_PROP(getStringEncoding());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(InterpolatedStringLiteralExpr) {
  CHECK_IMPL_AS(LiteralExpr);
  CHECK_PTR_PROP(getTrailingQuoteLoc());
  CHECK_PTR_PROP(getInterpolationCount());
  CHECK_PTR_PROP(getLiteralCapacity());
  CHECK_PTR_PROP(getAppendingExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(RegexLiteralExpr) {
  CHECK_IMPL_AS(LiteralExpr);
  CHECK_PTR_PROP(getRegexText());
  CHECK_PTR_PROP(getVersion());
  CHECK_PTR_PROP(getSerializedCaptureStructure());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ObjectLiteralExpr) {
  CHECK_IMPL_AS(LiteralExpr);
  CHECK_PTR_PROP(getArgs());
  CHECK_PTR_PROP(getLiteralKind());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DiscardAssignmentExpr) { return true; }

DEFINE_CHECK_IMPL_FUNC(DeclRefExpr) {
  CHECK_PTR_PROP(getDeclRef());
  CHECK_PTR_PROP(getNameLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SuperRefExpr) {
  CHECK_PTR_PROP(getSelf());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TypeExpr) {
  CHECK_PTR_PROP(getTypeRepr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OtherConstructorDeclRefExpr) {
  CHECK_PTR_PROP(getDeclRef());
  CHECK_PTR_PROP(getConstructorLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DotSyntaxBaseIgnoredExpr) {
  CHECK_PTR_PROP(getLHS());
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getRHS());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(OverloadedDeclRefExpr) {
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getDecls());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedDeclRefExpr) {
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getRefKind());
  CHECK_PTR_PROP(getFunctionRefKind());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LookupExpr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getMember());
  CHECK_PTR_PROP(isImplicitlyAsync());
  CHECK_PTR_PROP(isImplicitlyThrows());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(MemberRefExpr) {
  CHECK_IMPL_AS(LookupExpr)
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getNameLoc());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(SubscriptExpr) {
  CHECK_IMPL_AS(LookupExpr);
  CHECK_PTR_PROP(getArgs());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DynamicLookupExpr) {
  CHECK_IMPL_AS(LookupExpr)
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DynamicMemberRefExpr) {
  CHECK_IMPL_AS(DynamicLookupExpr);
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getNameLoc());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(DynamicSubscriptExpr) {
  CHECK_IMPL_AS(DynamicLookupExpr);
  CHECK_PTR_PROP(getArgs());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedSpecializeExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getLAngleLoc());
  CHECK_PTR_PROP(getRAngleLoc());
  CHECK_PTR_PROP(getUnresolvedParams());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedMemberExpr) {
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedDotExpr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SequenceExpr) {
  CHECK_PTR_PROP(getElements());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(IdentityExpr) {
  CHECK_PTR_PROP(getSubExpr());

  return true;
}

DEFINE_CHECK_IMPL_FUNC(ParenExpr) {
  CHECK_IMPL_AS(IdentityExpr);
  CHECK_PTR_PROP(getLParenLoc());
  CHECK_PTR_PROP(getRParenLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DotSelfExpr) {
  CHECK_IMPL_AS(IdentityExpr);
  CHECK_PTR_PROP(getDotLoc());
  CHECK_PTR_PROP(getSelfLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AwaitExpr) {
  CHECK_IMPL_AS(IdentityExpr);
  CHECK_PTR_PROP(getAwaitLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BorrowExpr) {
  CHECK_IMPL_AS(IdentityExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedMemberChainResultExpr) {
  CHECK_IMPL_AS(IdentityExpr);
  CHECK_PTR_PROP(getChainBase());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CopyExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ConsumeExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AnyTryExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getTryLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TryExpr) {
  CHECK_IMPL_AS(AnyTryExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ForceTryExpr) {
  CHECK_IMPL_AS(AnyTryExpr);
  CHECK_PTR_PROP(getExclaimLoc());
  // CHECK_PTR_PROP(getThrownError());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OptionalTryExpr) {
  CHECK_IMPL_AS(AnyTryExpr);
  CHECK_PTR_PROP(getQuestionLoc());
  // CHECK_PTR_PROP(getThrownError());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TupleExpr) {
  CHECK_PTR_PROP(getLParenLoc());
  CHECK_PTR_PROP(getRParenLoc());
  CHECK_PTR_PROP(getElements());
  CHECK_PTR_PROP(hasElementNames());
  CHECK_PTR_PROP(getElementNames());
  CHECK_PTR_PROP(hasElementNameLocs());
  CHECK_PTR_PROP(getElementNameLocs());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CollectionExpr) {
  CHECK_PTR_PROP(getLBracketLoc());
  CHECK_PTR_PROP(getRBracketLoc());
  CHECK_PTR_PROP(getNumElements());
  CHECK_PTR_PROP(getElements());
  CHECK_PTR_PROP(getNumCommas());
  CHECK_PTR_PROP(getCommaLocs());
  CHECK_PTR_PROP(isTypeDefaulted());
  CHECK_PTR_PROP(getInitializer());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ArrayExpr) {
  CHECK_IMPL_AS(CollectionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DictionaryExpr) {
  CHECK_IMPL_AS(CollectionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(KeyPathApplicationExpr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getKeyPath());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TupleElementExpr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getFieldNumber());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getDotLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CaptureListExpr) {
  CHECK_PTR_PROP(getCaptureList());
  CHECK_PTR_PROP(getClosureBody());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AbstractClosureExpr) {
  CHECK_PTR_PROP(getCaptureInfo());
  CHECK_PTR_PROP(getParameters());
  CHECK_PTR_PROP(getRawDiscriminator());
  CHECK_PTR_PROP(getActorIsolation());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ClosureExpr) {
  CHECK_PTR_PROP(getAttrs());
  CHECK_PTR_PROP(getBody());
  CHECK_PTR_PROP(hasAnonymousClosureVars());
  CHECK_PTR_PROP(allowsImplicitSelfCapture());
  CHECK_PTR_PROP(inheritsActorContext());
  CHECK_PTR_PROP(isIsolatedByPreconcurrency());
  CHECK_PTR_PROP(hasExplicitResultType());
  CHECK_PTR_PROP(getBracketRange());
  CHECK_PTR_PROP(getArrowLoc());
  CHECK_PTR_PROP(getInLoc());
  CHECK_PTR_PROP(getAsyncLoc());
  CHECK_PTR_PROP(getThrowsLoc());
  CHECK_PTR_PROP(getExplicitThrownTypeRepr());
  CHECK_PTR_PROP(getExplicitResultTypeRepr());
  CHECK_PTR_PROP(hasSingleExpressionBody());
  CHECK_PTR_PROP(getCapturedSelfDecl());
  CHECK_PTR_PROP(getBodyState());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AutoClosureExpr) {
  llvm_unreachable("not a parsed AST");
  return false;
}

DEFINE_CHECK_IMPL_FUNC(InOutExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(VarargExpansionExpr) {
  llvm_unreachable("VarargExpansionExpr should not be in parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(PackExpansionExpr) {
  CHECK_PTR_PROP(getPatternExpr());
  CHECK_PTR_PROP(getGenericEnvironment());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PackElementExpr) {
  CHECK_PTR_PROP(getPackRefExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MaterializePackExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DynamicTypeExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(RebindSelfInConstructorExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OpaqueValueExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PropertyWrapperValuePlaceholderExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AppliedPropertyWrapperExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DefaultArgumentExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BindOptionalExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getQuestionLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OptionalEvaluationExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ForceValueExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getExclaimLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OpenExistentialExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MakeTemporarilyEscapableExpr) {
  llvm_unreachable("not parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ApplyExpr) {
  CHECK_PTR_PROP(getFn());
  CHECK_PTR_PROP(getArgs());
  CHECK_PTR_PROP(isThrowsSet());
  CHECK_PTR_PROP(isNoAsync());
  CHECK_PTR_PROP(getIsolationCrossing());
  CHECK_PTR_PROP(isImplicitlyAsync());
  CHECK_PTR_PROP(implicitlyThrows());
  CHECK_PTR_PROP(shouldApplyDistributedThunk());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CallExpr) {
  CHECK_IMPL_AS(ApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PrefixUnaryExpr) {
  CHECK_IMPL_AS(ApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PostfixUnaryExpr) {
  CHECK_IMPL_AS(ApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BinaryExpr) {
  CHECK_IMPL_AS(ApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SelfApplyExpr) {
  CHECK_IMPL_AS(ApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DotSyntaxCallExpr) {
  CHECK_IMPL_AS(SelfApplyExpr);
  CHECK_PTR_PROP(getDotLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ConstructorRefCallExpr) {
  CHECK_IMPL_AS(SelfApplyExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ImplicitConversionExpr) {
  llvm_unreachable("not a parsed AST");
}

DEFINE_CHECK_IMPL_FUNC(LoadExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ABISafeConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DestructureTupleExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedTypeConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(FunctionConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CovariantFunctionConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CovariantReturnConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MetatypeConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CollectionUpcastConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ErasureExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AnyHashableErasureExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BridgeToObjCExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BridgeFromObjCExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ConditionalBridgeFromObjCExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DerivedToBaseExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ArchetypeToSuperExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(InjectIntoOptionalExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ClassMetatypeToObjectExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ExistentialMetatypeToObjectExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ProtocolMetatypeToObjectExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(InOutToPointerExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ArrayToPointerExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(StringToPointerExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PointerToPointerExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ForeignObjectConversionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnevaluatedInstanceExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnderlyingToOpaqueExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DifferentiableFunctionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LinearFunctionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DifferentiableFunctionExtractOriginalExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LinearFunctionExtractOriginalExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LinearToDifferentiableFunctionExpr) {
  CHECK_IMPL_AS(ImplicitConversionExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ExplicitCastExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getCastTypeRepr());
  CHECK_PTR_PROP(getAsLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CheckedCastExpr) {
  CHECK_IMPL_AS(ExplicitCastExpr);
  CHECK_PTR_PROP(getCastKind());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ForcedCheckedCastExpr) {
  CHECK_IMPL_AS(CheckedCastExpr);
  CHECK_PTR_PROP(getExclaimLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ConditionalCheckedCastExpr) {
  CHECK_IMPL_AS(CheckedCastExpr);
  CHECK_PTR_PROP(getQuestionLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(IsExpr) {
  CHECK_IMPL_AS(CheckedCastExpr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CoerceExpr) {
  CHECK_IMPL_AS(ExplicitCastExpr);
  CHECK_PTR_PROP(isLiteralInit());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ArrowExpr) {
  CHECK_PTR_PROP(getAsyncLoc());
  CHECK_PTR_PROP(getThrowsLoc());
  CHECK_PTR_PROP(getArrowLoc());
  CHECK_PTR_PROP(getArgs());
  CHECK_PTR_PROP(getResultExpr());
  CHECK_PTR_PROP(getThrownTypeExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TernaryExpr) {
  CHECK_PTR_PROP(getCondExpr());
  CHECK_PTR_PROP(getThenExpr());
  CHECK_PTR_PROP(getElseExpr());
  CHECK_PTR_PROP(getQuestionLoc());
  CHECK_PTR_PROP(getColonLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(EnumIsCaseExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getCaseTypeRepr());
  CHECK_PTR_PROP(getEnumElement());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AssignExpr) {
  CHECK_PTR_PROP(getDest());
  CHECK_PTR_PROP(getSrc());
  CHECK_PTR_PROP(getEqualLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CodeCompletionExpr) {
  CHECK_PTR_PROP(getBase());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(UnresolvedPatternExpr) {
  CHECK_PTR_PROP(getSubPattern());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(LazyInitializerExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(EditorPlaceholderExpr) {
  CHECK_PTR_PROP(getPlaceholder());
  CHECK_PTR_PROP(getPlaceholderTypeRepr());
  CHECK_PTR_PROP(getTypeForExpansion());
  CHECK_PTR_PROP(getSemanticExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ObjCSelectorExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getSelectorKind());
  CHECK_PTR_PROP(getMethod());
  CHECK_PTR_PROP(getModifierLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(KeyPathExpr) {
  CHECK_PTR_PROP(getComponents());
  CHECK_PTR_PROP(getParsedRoot());
  CHECK_PTR_PROP(getParsedPath());
  CHECK_PTR_PROP(getObjCStringLiteralExpr());
  CHECK_PTR_PROP(getExplicitRootType());
  CHECK_PTR_PROP(isObjC());
  CHECK_PTR_PROP(expectsContextualRoot());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SingleValueStmtExpr) {
  CHECK_PTR_PROP(getStmt());
  CHECK_PTR_PROP(getDeclContext());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(KeyPathDotExpr) { return true; }

DEFINE_CHECK_IMPL_FUNC(OneWayExpr) {
  CHECK_PTR_PROP(getSubExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TapExpr) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getBody());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TypeJoinExpr) {
  CHECK_PTR_PROP(getVar());
  CHECK_PTR_PROP(getElements());
  CHECK_PTR_PROP(getSingleValueStmtExpr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MacroExpansionExpr) {
  CHECK_IMPL_AS(FreestandingMacroExpansion);
  CHECK_PTR_PROP(getRewritten());
  CHECK_PTR_PROP(getMacroRoles());
  CHECK_PTR_PROP(getDeclContext());
  CHECK_PTR_PROP(getRawDiscriminator());
  return true;
}

// MARK: - TypeRepr.

DEFINE_CHECK_IMPL_FUNC(ErrorTypeRepr) {
  CHECK_PTR_PROP(getDelayedDiagnosticID());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AttributedTypeRepr) {
  CHECK_PTR_PROP(getAttrs());
  CHECK_PTR_PROP(getTypeRepr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DeclRefTypeRepr) { return true; }

DEFINE_CHECK_IMPL_FUNC(IdentTypeRepr) {
  CHECK_IMPL_AS(DeclRefTypeRepr);
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getNameRef());
  CHECK_PTR_PROP(getBoundDecl());
  CHECK_PTR_PROP(getDeclContext());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SimpleIdentTypeRepr) {
  CHECK_IMPL_AS(IdentTypeRepr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(GenericIdentTypeRepr) {
  CHECK_IMPL_AS(IdentTypeRepr);
  CHECK_PTR_PROP(getGenericArgs());
  CHECK_PTR_PROP(getAngleBrackets());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MemberTypeRepr) {
  CHECK_IMPL_AS(DeclRefTypeRepr);
  CHECK_PTR_PROP(getBaseComponent());
  CHECK_PTR_PROP(getMemberComponents());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(FunctionTypeRepr) {
  CHECK_PTR_PROP(getGenericParams());
  CHECK_PTR_PROP(getArgsTypeRepr());
  CHECK_PTR_PROP(getAsyncLoc());
  CHECK_PTR_PROP(getThrowsLoc());
  CHECK_PTR_PROP(getThrownTypeRepr());
  CHECK_PTR_PROP(getArrowLoc());
  CHECK_PTR_PROP(getResultTypeRepr());
  CHECK_PTR_PROP(getPatternGenericParams());
  CHECK_PTR_PROP(getPatternSubstitutions());
  CHECK_PTR_PROP(getInvocationSubstitutions());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ArrayTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getBrackets());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(DictionaryTypeRepr) {
  CHECK_PTR_PROP(getKey());
  CHECK_PTR_PROP(getValue());
  CHECK_PTR_PROP(getBrackets());
  CHECK_PTR_PROP(getColonLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OptionalTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getQuestionLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ImplicitlyUnwrappedOptionalTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getExclamationLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TupleTypeRepr) {
  CHECK_PTR_PROP(getParens());
  CHECK_PTR_PROP(getElements());
  return true;
}

DEFINE_CHECK_FUNC(TupleTypeReprElement) {
  CHECK_PROP(Name);
  CHECK_PROP(NameLoc);
  CHECK_PROP(SecondName);
  CHECK_PROP(SecondNameLoc);
  CHECK_PROP(UnderscoreLoc);
  CHECK_PROP(ColonLoc);
  CHECK_PROP(Type);
  CHECK_PROP(TrailingCommaLoc);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CompositionTypeRepr) {
  CHECK_PTR_PROP(getTypes());
  CHECK_PTR_PROP(getCompositionRange());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(MetatypeTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getMetaLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(VarargTypeRepr) {
  CHECK_PTR_PROP(getElementType());
  CHECK_PTR_PROP(getEllipsisLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PackExpansionTypeRepr) {
  CHECK_PTR_PROP(getRepeatLoc());
  CHECK_PTR_PROP(getPatternType());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ProtocolTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getProtocolLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OpaqueReturnTypeRepr) {
  CHECK_PTR_PROP(getConstraint());
  CHECK_PTR_PROP(getOpaqueLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(NamedOpaqueReturnTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getGenericParams());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ExistentialTypeRepr) {
  CHECK_PTR_PROP(getConstraint());
  CHECK_PTR_PROP(getAnyLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(InverseTypeRepr) {
  CHECK_PTR_PROP(getConstraint());
  CHECK_PTR_PROP(getTildeLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PackTypeRepr) {
  CHECK_PTR_PROP(getKeywordLoc());
  CHECK_PTR_PROP(getBracesRange());
  CHECK_PTR_PROP(getElements());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PackElementTypeRepr) {
  CHECK_PTR_PROP(getEachLoc());
  CHECK_PTR_PROP(getPackType());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(PlaceholderTypeRepr) {
  CHECK_PTR_PROP(getUnderscoreLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SpecifierTypeRepr) {
  CHECK_PTR_PROP(getBase());
  CHECK_PTR_PROP(getSpecifierLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OwnershipTypeRepr) {
  CHECK_IMPL_AS(SpecifierTypeRepr);
  CHECK_PTR_PROP(getSpecifier());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(IsolatedTypeRepr) {
  CHECK_IMPL_AS(SpecifierTypeRepr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(CompileTimeConstTypeRepr) {
  CHECK_IMPL_AS(SpecifierTypeRepr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ResultDependsOnTypeRepr) {
  CHECK_IMPL_AS(SpecifierTypeRepr);
  return true;
}

DEFINE_CHECK_IMPL_FUNC(FixedTypeRepr) {
  CHECK_PTR_PROP(getType());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SILBoxTypeRepr) {
  llvm_unreachable("not a parsed AST");
  return true;
}

DEFINE_CHECK_IMPL_FUNC(SelfTypeRepr) {
  CHECK_PTR_PROP(getType());
  return true;
}

// MARK: - Patterns.

DEFINE_CHECK_IMPL_FUNC(ParenPattern) {
  CHECK_PTR_PROP(getSubPattern());
  CHECK_PTR_PROP(getLParenLoc());
  CHECK_PTR_PROP(getRParenLoc());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TuplePattern) {
  CHECK_PTR_PROP(getLParenLoc());
  CHECK_PTR_PROP(getRParenLoc());
  CHECK_PTR_PROP(getElements());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(NamedPattern) {
  CHECK_PTR_PROP(getDecl());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(AnyPattern) {
  CHECK_PTR_PROP(isAsyncLet());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(TypedPattern) {
  CHECK_PTR_PROP(getSubPattern());
  CHECK_PTR_PROP(getTypeRepr());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BindingPattern) {
  CHECK_PTR_PROP(getIntroducer());
  CHECK_PTR_PROP(getSubPattern());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(IsPattern) {
  CHECK_PTR_PROP(getSubPattern());
  CHECK_PTR_PROP(getCastKind());
  CHECK_PTR_PROP(getCastType());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(EnumElementPattern) {
  CHECK_PTR_PROP(getName());
  CHECK_PTR_PROP(getNameLoc());
  CHECK_PTR_PROP(getParentType());
  CHECK_PTR_PROP(getUnresolvedOriginalExpr());
  CHECK_PTR_PROP(getElementDecl());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(OptionalSomePattern) {
  CHECK_PTR_PROP(getQuestionLoc());
  CHECK_PTR_PROP(getSubPattern());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(BoolPattern) {
  CHECK_PTR_PROP(getValue());
  return true;
}

DEFINE_CHECK_IMPL_FUNC(ExprPattern) {
  CHECK_PTR_PROP(getSubExpr());
  CHECK_PTR_PROP(getDeclContext());
  CHECK_PTR_PROP(isResolved());
  CHECK_PTR_PROP(getCachedMatchExpr());
  CHECK_PTR_PROP(getCachedMatchVar());
  return true;
}

} // namespace

llvm::Error verifyASTEquivalence(ASTNode lhs, ASTNode rhs, std::string &error) {
  ::ASTComparison comparison;
  if (!comparison.doIt(lhs, rhs, "root"))
    return comparison.getError();
  return llvm::Error::success();
}
