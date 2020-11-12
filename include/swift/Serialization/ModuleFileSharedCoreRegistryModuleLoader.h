//===--- ModuleFileSharedCoreRegistryModuleLoader.h -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MODULE_FILE_SHARED_CORE_REGISTRY_MODULE_LOADER_H
#define SWIFT_MODULE_FILE_SHARED_CORE_REGISTRY_MODULE_LOADER_H

#include "swift/Basic/LLVM.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/ADT/StringMap.h"

namespace swift {
class ModuleDecl;
class ModuleFileSharedCore;

class ModuleFileSharedCoreRegistry {
public:
  struct Value {
    std::shared_ptr<const ModuleFileSharedCore> ModuleFileCore;
    std::shared_ptr<const ModuleFileSharedCore> ClangModuleFileCore;
    std::string ProxyName;
    bool IsSystemModule;

    Value(std::shared_ptr<const ModuleFileSharedCore> ModuleFileCore,
          bool IsSystemModule)
        : ModuleFileCore(ModuleFileCore), IsSystemModule(IsSystemModule) {}

    Value() : Value(nullptr, false) {}
  };

private:
  llvm::StringMap<Value> Storage;

  std::shared_ptr<const ModuleFileSharedCore>
  serializeClangModule(ModuleDecl *M);
  void registerClangModule(ModuleDecl *M);

public:
  ModuleFileSharedCoreRegistry() {}

  /// Clear the registry.
  void clear();

  /// Add a module to the registry.
  void registerModule(ModuleDecl *M);

  /// Lookup a module by name in the registry.
  Value lookup(StringRef name) const;
};

class ModuleFileSharedCoreRegistryModuleLoader
    : public SerializedModuleLoaderBase {
  std::shared_ptr<ModuleFileSharedCoreRegistry> Registry;

  ModuleFileSharedCoreRegistryModuleLoader(ASTContext &ctx,
                                           DependencyTracker *tracker,
                                           ModuleLoadingMode loadMode,
                                           bool IgnoreSwiftSourceInfo)
      : SerializedModuleLoaderBase(ctx, tracker, loadMode,
                                   IgnoreSwiftSourceInfo) {}

  ModuleDecl *loadModuleImpl(SourceLoc importLoc, ImportPath::Module path,
                             std::shared_ptr<const ModuleFileSharedCore> moduleFileCore,
                             bool isSystemModule, ModuleDecl *underlyingModule);

public:
  void
  setRegistry(const std::shared_ptr<ModuleFileSharedCoreRegistry> &Registry) {
    this->Registry = Registry;
  }

  void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override{};
  std::error_code findModuleFilesInDirectory(
      ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
      bool IsFramework) override;

  bool canImportModule(Located<Identifier> named) override;
  ModuleDecl *loadModule(SourceLoc importLoc,
                         ImportPath::Module path) override;

  static std::unique_ptr<ModuleFileSharedCoreRegistryModuleLoader>
  create(ASTContext &ctx, DependencyTracker *tracker) {
    return std::unique_ptr<ModuleFileSharedCoreRegistryModuleLoader>{
        new ModuleFileSharedCoreRegistryModuleLoader(
            ctx, tracker, ModuleLoadingMode::OnlySerialized,
            /*IgnoreSwiftSourceInfo=*/true)};
  }
};

} // namespace swift

#endif
