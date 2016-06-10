# swift_build_support/env.py ------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Environment variables
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os

__all__ = [
    "HOME",
    "SWIFT_SOURCE_ROOT",
    "SWIFT_BUILD_ROOT"
]


def _get_default_source_root():
    result = ""

    # Are we in a Swift checkout? Start from this file and check its parent
    # directories.
    #
    # $SWIFT_SOURCE_ROOT/
    #   swift/utils/swift_build_support/swift_build_support/env.py
    (swift_path, parent_dirname) = os.path.split(
        os.path.normpath(os.path.join(os.path.abspath(__file__), "../../..")))
    if parent_dirname != "utils":
        return result
    if not os.path.exists(os.path.join(swift_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(swift_path)

    # Are we in an LLVM checkout? Start from the Swift checkout and check /its/
    # parent directories.
    #
    # $SWIFT_SOURCE_ROOT/llvm/tools/
    #   swift/utils/swift_build_support/swift_build_support/env.py
    (llvm_path, parent_dirname) = os.path.split(result)
    if parent_dirname != "tools":
        return result
    if not os.path.exists(os.path.join(llvm_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(llvm_path)

    return result

# Set SWIFT_SOURCE_ROOT in your environment to control where the sources
# are found.
SWIFT_SOURCE_ROOT = os.environ.get(
    "SWIFT_SOURCE_ROOT", _get_default_source_root())

# Set SWIFT_BUILD_ROOT to a directory that will contain a subdirectory
# for each build configuration
SWIFT_BUILD_ROOT = os.environ.get(
    "SWIFT_BUILD_ROOT", os.path.join(SWIFT_SOURCE_ROOT, "build"))

HOME = os.environ.get("HOME", "/")
