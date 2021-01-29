//===--- ModuleSourceFileInfo.cpp -------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/ModuleSourceFileInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Subsystems.h"

using namespace swift;
using namespace swift::ide;

StringRef swift::ide::getSourceFilePathForDecl(const Decl *D) {
  auto *DC = D->getDeclContext();
  FileUnit *fileUnit = dyn_cast<FileUnit>(DC->getModuleScopeContext());
  if (!fileUnit)
    return "";

  if (auto *SF = dyn_cast<SourceFile>(fileUnit))
    return SF->getFilename();
  if (auto loc = fileUnit->getBasicLocsForDecl(D))
    return loc->SourceFilePath;

  return "";
}

bool swift::ide::isSourceFileUpToDate(const BasicSourceFileInfo &info,
                                      ASTContext &Ctx) {
  auto &SM = Ctx.SourceMgr;
  auto stat = SM.getFileSystem()->status(info.FilePath);
  // If missing, it's not up-to-date.
  if (!stat)
    return false;

  // Assume up-to-date if the modification time and the size are the same.
  if (stat->getLastModificationTime() == info.LastModified &&
      stat->getSize() == info.FileSize)
    return true;

  // If the interface hash is unknown, we can't compare it.
  if (info.InterfaceHash == Fingerprint::ZERO())
    return false;

  // Check if the interface hash has changed.

  auto buffer = SM.getFileSystem()->getBufferForFile(info.FilePath);
  // If failed to open, it's not up-to-date.
  if (!buffer)
    return false;

  SourceManager tmpSM;
  auto tmpBufferID = tmpSM.addNewSourceBuffer(std::move(*buffer));

  LangOptions langOpts = Ctx.LangOpts;
  TypeCheckerOptions typechkOpts = Ctx.TypeCheckerOpts;
  SearchPathOptions searchPathOpts = Ctx.SearchPathOpts;
  DiagnosticEngine tmpDiags(tmpSM);
  ClangImporterOptions clangOpts;
  std::unique_ptr<ASTContext> tmpCtx(ASTContext::get(
      langOpts, typechkOpts, searchPathOpts, clangOpts, tmpSM, tmpDiags));
  registerParseRequestFunctions(tmpCtx->evaluator);
  registerIDERequestFunctions(tmpCtx->evaluator);
  registerTypeCheckerRequestFunctions(tmpCtx->evaluator);
  registerSILGenRequestFunctions(tmpCtx->evaluator);
  ModuleDecl *tmpM = ModuleDecl::create(Identifier(), *tmpCtx);
  SourceFile::ParsingOptions parseOpts;
  parseOpts |= SourceFile::ParsingFlags::EnableInterfaceHash;
  SourceFile *tmpSF = new (*tmpCtx)
      SourceFile(*tmpM, SourceFileKind::Library, tmpBufferID, parseOpts);
  auto fingerprint = tmpSF->getInterfaceHash();
  return fingerprint == info.InterfaceHash;
}

void swift::ide::getAllKnownSourceFileCurrentness(
    ASTContext &Ctx, SmallVectorImpl<SourceFileCurrentness> result) {
  assert(result.empty());

  SmallVector<ModuleDecl *, 8> loadedModules;
  loadedModules.reserve(Ctx.getNumLoadedModules());
  for (auto &entry : Ctx.getLoadedModules())
    loadedModules.push_back(entry.second);

  for (auto *M : loadedModules) {
    // We don't need to check system modules.
    if (M->isSystemModule())
      continue;

    M->collectBasicSourceFileInfo([&](const BasicSourceFileInfo &info) {
      auto isUpToDate = isSourceFileUpToDate(info, Ctx);
      result.emplace_back(info.FilePath, isUpToDate);
    });
  }
}
