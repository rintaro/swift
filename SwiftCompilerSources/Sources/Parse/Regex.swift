//===--- Regex.swift - Regex compilation bridging layer -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Bridging layer between 'string-processing' module and the compiler
//
//===----------------------------------------------------------------------===//


#if canImport(_MatchingEngine)
import ParseBridging
import _MatchingEngine

func registerRegexParser() {
  Parser_registerRegexLiteralParsingFn(parseRegexLiteral)
  Parser_registerRegexLiteralLexingFn(lexRegexLiteral)
}

// FIXME: Remove this when DiagnosticEngine is bridged.
private func copyCString(_ str: String) -> UnsafePointer<CChar> {
  let count = str.utf8.count + 1
  return str.withCString {
    assert($0[count-1] == 0)
    let ptr = UnsafeMutablePointer<CChar>.allocate(capacity: count)
    ptr.initialize(from: $0, count: count)
    return UnsafePointer(ptr)
  }
}

/// Interface for libswift.
///
/// Attempt to lex a regex literal string.
///
/// - Parameters:
///   - CurPtrPtr: A pointer to the current pointer of lexer, which should be
///                the start of the literal. This will be advanced to the point
///                at which the lexer should resume, or will remain the same if
///                this is not a regex literal.
///   - BufferEnd: A pointer to the end of the buffer, which should not be lexed
///                past.
///   - ErrorOut: If an error is encountered, this will be set to the error
///               string.
///
/// - Returns: A bool indicating whether lexing was completely erroneous, and
///            cannot be recovered from, or false if there either was no error,
///            or there was a recoverable error.
func lexRegexLiteral(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>?>?,
  _ bufferEndPtr: UnsafePointer<CChar>?,
  _ errOut: UnsafeMutablePointer<UnsafePointer<CChar>?>?
) -> /*CompletelyErroneous*/ CBool {
  guard let curPtrPtr = curPtrPtr, let inputPtr = curPtrPtr.pointee,
        let bufferEndPtr = bufferEndPtr
  else {
    fatalError("Expected lexing pointers")
  }
  guard let errOut = errOut else { fatalError("Expected error out param") }

  do {
    let (_, _, endPtr) = try lexRegex(start: inputPtr, end: bufferEndPtr)
    curPtrPtr.pointee = endPtr.assumingMemoryBound(to: CChar.self)
    return false
  } catch let error as LexError {
    if error.kind == .unknownDelimiter {
      // An unknown delimiter should be recovered from, as we may want to try
      // lex something else.
      return false
    }
    errOut.pointee = copyCString("\(error)")
    curPtrPtr.pointee = error.resumePtr.assumingMemoryBound(to: CChar.self)

    // For now, treat every error as unrecoverable.
    // TODO: We should ideally be able to recover from a regex with missing
    // closing delimiters, which would help with code completion.
    return true
  } catch {
    fatalError("Should be a LexError")
  }
}

/// Interface for libswift.
///
/// - Parameters:
///   - inputPtr: A null-terminated C string.
///   - errOut: A buffer accepting an error string upon error.
///   - versionOut: A buffer accepting a regex literal format
///     version.
///   - captureStructureOut: A buffer accepting a byte sequence representing the
///     capture structure.
///   - captureStructureSize: The size of the capture structure buffer. Must be
///     greater than or equal to `strlen(inputPtr)`.
func parseRegexLiteral(
  _ inputPtr: UnsafePointer<CChar>?,
  _ errOut: UnsafeMutablePointer<UnsafePointer<CChar>?>?,
  _ versionOut: UnsafeMutablePointer<CUnsignedInt>?,
  _ captureStructureOut: UnsafeMutableRawPointer?,
  _ captureStructureSize: CUnsignedInt
) {
  guard let s = inputPtr else { fatalError("Expected input param") }
  guard let errOut = errOut else { fatalError("Expected error out param") }
  guard let versionOut = versionOut else {
    fatalError("Expected version out param")
  }

  versionOut.pointee = currentRegexLiteralFormatVersion

  let str = String(cString: s)
  do {
    let ast = try parseWithDelimiters(str)
    // Serialize the capture structure for later type inference.
    if let captureStructureOut = captureStructureOut {
      assert(captureStructureSize >= str.utf8.count)
      let buffer = UnsafeMutableRawBufferPointer(
          start: captureStructureOut, count: Int(captureStructureSize))
      ast.captureStructure.encode(to: buffer)
    }
  } catch {
    errOut.pointee = copyCString(
      "cannot parse regular expression: \(String(describing: error))")
  }
}

#endif
