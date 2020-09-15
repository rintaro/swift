//===--- ModuleFileSharedCoreRegistryModuleLoader.cpp -----------*- C++ -*-===//
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

#include "swift/Serialization/ModuleFileSharedCoreRegistryModuleLoader.h"

#include "ModuleFile.h"
#include "ModuleFileSharedCore.h"
#include "Serialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/ClangImporter/ClangModule.h"

using namespace swift;

void ModuleFileSharedCoreRegistry::clear() { Storage.clear(); }

std::shared_ptr<const ModuleFileSharedCore>
ModuleFileSharedCoreRegistry::serializeClangModule(ModuleDecl *M) {
//  auto *clangM = dyn_cast<ClangModuleUnit>(M->getFiles().front())->getClangModule();


//  llvm::errs() << "Clang: " <<
//  dyn_cast<ClangModuleUnit>(M->getFiles().front())->getClangModule()->getFullModuleName() << " .." << M->getNameStr()
//  << "\n";
//
//  for (auto *sub : clangM->submodules()) {
//    llvm::errs() << "SUB: " << sub->getFullModuleName() << "\n";
//  }


  SerializationOptions serializationOpts;
  serializationOpts.ForImorterStateCache = true;
  std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;
  std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer;

  std::shared_ptr<const ModuleFileSharedCore> moduleCore;
  swift::serializeToBuffers(M, serializationOpts, &moduleBuffer,
                            &moduleDocBuffer, /*sourceLocBuffer*/nullptr);
//  llvm::errs() << "ModuleBufferSize:" << moduleBuffer->getBufferSize() << "\n";
//  llvm::errs() << "moduleDocBuffer:" << moduleDocBuffer->getBufferSize() << "\n";
  ModuleFileSharedCore::load(M->getModuleFilename(), std::move(moduleBuffer),
                             std::move(moduleDocBuffer), nullptr,
                             /*isFramework=*/false, moduleCore);
  return moduleCore;
}

void ModuleFileSharedCoreRegistry::registerModule(ModuleDecl *M) {
  if (M->failedToLoad() || M->getFiles().empty() || M->isBuiltinModule())
    return;

  if (M->isNonSwiftModule()) {
    if (Storage[M->getNameStr()].ClangModuleFileCore)
      return;

//    if (M->getNameStr() == "SwiftShims" || M->getNameStr() == "SwiftOverlayShims")
//      return;

    Storage[M->getNameStr()].ClangModuleFileCore = serializeClangModule(M);
  } else if (auto ASTFile =
                 dyn_cast<SerializedASTFile>(M->getFiles().front())) {
    if (Storage[M->getNameStr()].ModuleFileCore)
      return;
//    llvm::errs() << "RegisterModule: " << M->getName() << "\n";
    Storage[M->getNameStr()].ModuleFileCore = ASTFile->File.getCore();
    if (auto clangM = ASTFile->getUnderlyingModuleIfOverlay())
      Storage[M->getNameStr()].ClangModuleFileCore = serializeClangModule(clangM);
  } else {
    return;
  }

  Storage[M->getNameStr()].IsSystemModule = M->isSystemModule();
}

ModuleFileSharedCoreRegistry::Value
ModuleFileSharedCoreRegistry::lookup(StringRef name) const {
  return Storage.lookup(name);
}

std::error_code
ModuleFileSharedCoreRegistryModuleLoader::findModuleFilesInDirectory(
    ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
    SmallVectorImpl<char> *ModuleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
    bool IsFramework) {
  // This is a soft error instead of an llvm_unreachable because this API is
  // primarily used by LLDB which makes it more likely that unwitting changes to
  // the Swift compiler accidentally break the contract.
  assert(false && "not supported");
  return std::make_error_code(std::errc::not_supported);
}

bool ModuleFileSharedCoreRegistryModuleLoader::canImportModule(
    Located<Identifier> named) {
  assert(Registry);
  return bool(Registry->lookup(named.Item.str()).ModuleFileCore);
}

ModuleDecl *ModuleFileSharedCoreRegistryModuleLoader::loadModuleImpl(
    SourceLoc importLoc, Identifier name,
    std::shared_ptr<const ModuleFileSharedCore> moduleFileCore,
    bool isSystemModule, ModuleDecl *underlyingModule) {

  if (dependencyTracker)
    dependencyTracker->addDependency(moduleFileCore->getModuleFilename(),
                                     isSystemModule);

  auto *M = ModuleDecl::create(name, Ctx);
  M->setIsSystemModule(isSystemModule);
  Ctx.addLoadedModule(M);
  SWIFT_DEFER { M->setHasResolvedImports(); };

  std::unique_ptr<ModuleFile> loadedModuleFile =
      std::make_unique<ModuleFile>(moduleFileCore);

  // Set the underlying module before 'loadAST' loads it from ClangImporter.
  if (underlyingModule)
    loadedModuleFile->UnderlyingModule = underlyingModule;

  FileUnit *file = nullptr;
  auto status = loadAST(*M, importLoc, loadedModuleFile, file);
  if (status == serialization::Status::Valid) {
    M->addFile(*file);
  } else {
    llvm::errs() << "FAILED TO LOAD: " << name << "(" <<  int(status) << "\n";
      serialization::ValidationInfo loadInfo;
      loadInfo.status = status;

      serialization::diagnoseSerializedASTLoadFailure(Ctx, SourceLoc(), loadInfo, moduleFileCore->ModuleInputBuffer->getBufferIdentifier(), moduleFileCore->ModuleDocInputBuffer->getBufferIdentifier(), loadedModuleFile.get(), name);
    M->setFailedToLoad();
  }

  return M;
}

ModuleDecl *ModuleFileSharedCoreRegistryModuleLoader::loadModule(
    SourceLoc importLoc, ImportPath::Module path) {
  assert(Registry);

//  // TODO: How to support submodules?
  if (path.hasSubmodule())
    return nullptr;
//  llvm::errs() << "loadModule: ";
//  llvm::interleave(path, [](Located<Identifier> i) {llvm::errs() << i.Item; }, []() {llvm::errs() << "."; });
//  llvm::errs() << "\n";

  const auto &moduleID = path.front();

  auto cached = Registry->lookup(moduleID.Item.str());
  if (!cached.ModuleFileCore && !cached.ClangModuleFileCore) {
//    llvm::errs() << "NotFound: " << moduleID.Item.str() << "\n";
    return nullptr;
  }

  ModuleDecl *underlyingModule = nullptr;

  // Load serialized underlying module.
  if (cached.ClangModuleFileCore) {
    underlyingModule = loadModuleImpl(importLoc, moduleID.Item,
                                      cached.ClangModuleFileCore,
                                      cached.IsSystemModule,
                                      /*underlyingModule=*/nullptr);
    if (!cached.ModuleFileCore)
      return underlyingModule;
  }

  // Load the actual module.
  return loadModuleImpl(importLoc, moduleID.Item,
                        cached.ModuleFileCore,
                        cached.IsSystemModule,
                        underlyingModule);
}
