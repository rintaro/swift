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
#include "swift/Basic/Platform.h"
#include "swift/Basic/FileTypes.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/ClangImporter/ClangModule.h"

using namespace swift;

void ModuleFileSharedCoreRegistry::clear() { Storage.clear(); }

std::shared_ptr<const ModuleFileSharedCore>
ModuleFileSharedCoreRegistry::serializeClangModule(ModuleDecl *M) {
  auto *clangM = dyn_cast<ClangModuleUnit>(M->getFiles().front())->getClangModule();

//  llvm::errs() << "Clang: " <<
//  dyn_cast<ClangModuleUnit>(M->getFiles().front())->getClangModule()->getFullModuleName() << " .." << M->getNameStr()
//  << "\n";
//
//  for (auto *sub : clangM->submodules()) {
//    llvm::errs() << "SUB: " << sub->getFullModuleName() << " explicit:" << sub->IsExplicit << "\n";
//  }

  SerializationOptions serializationOpts;
  serializationOpts.ForImorterStateCache = true;
  std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;
  std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer;

  SmallString<256> modulePath;
  SmallString<256> moduleDocPath;
  if (!CacheDirPath.empty()) {
    llvm::sys::fs::create_directories(CacheDirPath);
    SmallString<64> fullModuleName;
    llvm::raw_svector_ostream fullModuleNameOS(fullModuleName);
    M->getReverseFullModuleName().printForward(fullModuleNameOS);

    modulePath.append(CacheDirPath);
    llvm::sys::path::append(modulePath, fullModuleName + "." + file_types::getExtension(file_types::TY_SwiftModuleFile));
    serializationOpts.OutputPath = modulePath.c_str();
    llvm::errs() << "modulePath: " << modulePath << "\n";

    moduleDocPath.append(CacheDirPath);
    llvm::sys::path::append(moduleDocPath, fullModuleName + "." + file_types::getExtension(file_types::TY_SwiftModuleDocFile));
    serializationOpts.DocOutputPath = moduleDocPath.c_str();
    llvm::errs() << "moduleDocPath: " << moduleDocPath << "\n";
  }

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

void ModuleFileSharedCoreRegistry::registerClangModule(ModuleDecl *M) {
  llvm::errs() << "registerClangModule: " << M->getName() << "\n";
  auto clangImporter = M->getASTContext().getClangModuleLoader();

  SmallVector<const clang::Module *, 32> modules;

  SmallVector<const clang::Module *, 8> stack;

  auto *clangM = dyn_cast<ClangModuleUnit>(M->getFiles().front())->getClangModule();
  stack.push_back(clangM);

  while (!stack.empty()) {
    auto entry = stack.pop_back_val();
    auto mod = clangImporter->getWrapperForModule(entry);
    auto explicitMod = getExplicitParentModule(entry);

    auto name = entry->getFullModuleName();

    if (entry == explicitMod) {
      if (Storage[name].ClangModuleFileCore)
        return;

      Storage[name].ClangModuleFileCore = serializeClangModule(mod);
      Storage[name].IsSystemModule |= entry->IsSystem;
    } else {
      Storage[name].ProxyName = explicitMod->getFullModuleName();
    }

    stack.append(entry->submodule_begin(), entry->submodule_end());
  }

}

void ModuleFileSharedCoreRegistry::registerModule(ModuleDecl *M) {
  //llvm::errs() << "registerModule: " << M->getName() << "\n";
  // Ensure this is a top-level module.
  M = M->getTopLevelModule();

  if (M->failedToLoad() || M->getFiles().empty() || M->isBuiltinModule())
    return;

  if (M->isNonSwiftModule()) {
    registerClangModule(M);
  } else if (auto ASTFile =
                 dyn_cast<SerializedASTFile>(M->getFiles().front())) {
    if (Storage[M->getNameStr()].ModuleFileCore)
      return;
//    llvm::errs() << "RegisterModule: " << M->getName() << "\n";
    Storage[M->getNameStr()].ModuleFileCore = ASTFile->File.getCore();
    if (auto clangM = ASTFile->getUnderlyingModuleIfOverlay())
      registerClangModule(clangM);
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
    SourceLoc importLoc, ImportPath::Module path,
    std::shared_ptr<const ModuleFileSharedCore> moduleFileCore,
    bool isSystemModule, ModuleDecl *underlyingModule) {

  if (dependencyTracker)
    dependencyTracker->addDependency(moduleFileCore->getModuleFilename(),
                                     isSystemModule);

  auto *M = ModuleDecl::create(path.back().Item, Ctx);
  auto *topM = M;
  
  // Ensure top-level module is loaded if this is a sub module.
  if (path.hasSubmodule()) {
    topM = loadModule(importLoc, path.getTopLevelPath());
    if (!topM)
      return nullptr;
  }
  
  M->setIsSystemModule(isSystemModule);
  Ctx.addLoadedModule(M);
  SWIFT_DEFER { M->setHasResolvedImports(); };

  std::unique_ptr<ModuleFile> loadedModuleFile =
      std::make_unique<ModuleFile>(moduleFileCore);

  // Set the underlying module before 'loadAST' loads it from ClangImporter.
  if (underlyingModule)
    loadedModuleFile->UnderlyingModule = underlyingModule;

  LoadedFile *file = nullptr;
  auto status = loadAST(*topM, importLoc, loadedModuleFile, file);
  if (status == serialization::Status::Valid) {
    
    // NOTE: Add submodule file to the top-level module because declarations
    // inside submodules are referenced by top-level module name.
    // e.g. 'TopModule.declName' instead of 'TopModule.SubModule.declName'.
    topM->addFile(*file);

    // Also add the file to the submodule just to satisfy the invariant that
    // loaded modules have a file.
    if (topM != M)
      M->addFile(*file);
    
  } else {
    llvm::errs() << "FAILED TO LOAD: " << path.back().Item << "(" <<  int(status) << ")\n";
    M->setFailedToLoad();
  }

  return M;
}

ModuleDecl *ModuleFileSharedCoreRegistryModuleLoader::loadModule(
    SourceLoc importLoc, ImportPath::Module path) {
  assert(Registry);

  SmallString<32> fullName;
  llvm::raw_svector_ostream OS(fullName);
  path.print(OS);
  auto cached = Registry->lookup(fullName);

  // If this is a implict sub module, import the explict parent module instead.
  if (!cached.ProxyName.empty()) {
    ImportPath::Module::Builder proxyPath(Ctx, cached.ProxyName, '.');
    return loadModule(importLoc, proxyPath.get());
  }

  if (!cached.ModuleFileCore && !cached.ClangModuleFileCore) {
//    llvm::errs() << "NotFound: " << moduleID.Item.str() << "\n";
    return nullptr;
  }

  ModuleDecl *underlyingModule = nullptr;

  // Load serialized underlying module.
  if (cached.ClangModuleFileCore) {
    underlyingModule = loadModuleImpl(importLoc, path,
                                      cached.ClangModuleFileCore,
                                      cached.IsSystemModule,
                                      /*underlyingModule=*/nullptr);
    // If the overlay isn't there, there's no overlay.
    if (!cached.ModuleFileCore)
      return underlyingModule;

    assert(!path.hasSubmodule() && "Clang sub modules can't have overlays");
  }

  // Load the overlay module.
  return loadModuleImpl(importLoc, path,
                        cached.ModuleFileCore,
                        cached.IsSystemModule,
                        underlyingModule);
}
