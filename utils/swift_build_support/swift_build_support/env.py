# -- presets.py - build-preset.ini loader -----------------------*- python -*-
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

'''Provides environment variable in current Swift source tree.

HOME: Your home directory
SWIFT_SOURCE_ROOT: Default Swift source workspace directory
SWIFT_BUILD_ROOT: Default directory where built files should go in.
'''

# ---------------------------------------------------------------------------

import os
import sys


def _get_default_source_root():
    '''Return auto detected SWIFT_SOURCE_ROOT directory path string.

    Assuming `build-script` is in a Swift checkout, traverse ancestor
    directories and find SWIFT_SOURCE_ROOT that must have at least `swift`,
    `llvm` and `cmark` directory.

    Return `None` if not found.
    '''

    def _is_source_root(directory):
        for name in ['swift', 'llvm', 'cmark']:
            if not os.path.isdir(os.path.join(directory, name)):
                return False
        return True

    directory = os.path.dirname(os.path.abspath(sys.argv[0]))
    root = os.path.join(os.path.splitdrive(directory)[0], '/')
    while directory != root:
        if _is_source_root(directory):
            return directory
        directory = os.path.dirname(directory)
    return None


HOME = os.environ.get("HOME", "/")

# Set SWIFT_SOURCE_ROOT in your environment to control where the sources
# are found.
SWIFT_SOURCE_ROOT = os.environ.get(
    "SWIFT_SOURCE_ROOT", _get_default_source_root())

# Set SWIFT_BUILD_ROOT to a directory that will contain a subdirectory
# for each build configuration
SWIFT_BUILD_ROOT = os.environ.get(
    "SWIFT_BUILD_ROOT", os.path.join(SWIFT_SOURCE_ROOT, "build"))
