#!/usr/bin/env python
# utils/build-script - The ultimate tool for building Swift -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import argparse
import multiprocessing
import os
import platform
import shutil
import sys
import textwrap

from swift_build_support.util import (
    HOME,
    SWIFT_BUILD_ROOT,
    SWIFT_SOURCE_ROOT,
    WorkingDirectory,
    check_call,
    get_all_preset_names,
    get_preset_options,
    print_with_argv0,
    quote_shell_command,
)

import swift_build_support.clang
import swift_build_support.cmake
import swift_build_support.debug
from swift_build_support.migration import migrate_impl_args
import swift_build_support.ninja
import swift_build_support.tar
import swift_build_support.targets


# Main entry point for the preset mode.
def main_preset():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""Builds Swift using a preset.""")
    parser.add_argument(
        "--preset-file",
        help="load presets from the specified file",
        metavar="PATH",
        action="append",
        dest="preset_file_names",
        default=[])
    parser.add_argument(
        "--preset",
        help="use the specified option preset",
        metavar="NAME")
    parser.add_argument(
        "--show-presets",
        help="list all presets and exit",
        action="store_true")
    parser.add_argument(
        "--distcc",
        help="use distcc",
        action="store_true")
    parser.add_argument(
        "preset_substitutions_raw",
        help="'name=value' pairs that are substituted in the preset",
        nargs="*",
        metavar="SUBSTITUTION")
    args = parser.parse_args()

    if len(args.preset_file_names) == 0:
        args.preset_file_names = [
            os.path.join(HOME, ".swift-build-presets"),
            os.path.join(
                SWIFT_SOURCE_ROOT, "swift", "utils", "build-presets.ini")
        ]

    if args.show_presets:
        for name in sorted(get_all_preset_names(args.preset_file_names),
                           key=str.lower):
            print(name)
        return 0

    if not args.preset:
        print_with_argv0("Missing --preset option")
        return 1

    args.preset_substitutions = {}

    for arg in args.preset_substitutions_raw:
        name, value = arg.split("=", 1)
        args.preset_substitutions[name] = value

    preset_args = get_preset_options(
        args.preset_substitutions, args.preset_file_names, args.preset)

    build_script_args = [sys.argv[0]]
    build_script_args += preset_args
    if args.distcc:
        build_script_args += ["--distcc"]

    print_with_argv0(
        "using preset '" + args.preset + "', which expands to " +
        quote_shell_command(build_script_args))
    check_call(build_script_args)

    return 0


# Main entry point for the normal mode.
def main_normal():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""
Use this tool to build, test, and prepare binary distribution archives of Swift
and related tools.

Builds Swift (and, optionally, LLDB), incrementally, optionally
testing it thereafter.  Different build configurations are maintained in
parallel.""",
        epilog="""
Using option presets:

  --preset-file=PATH    load presets from the specified file

  --preset=NAME         use the specified option preset

  The preset mode is mutually exclusive with other options.  It is not
  possible to add ad-hoc customizations to a preset.  This is a deliberate
  design decision.  (Rationale: a preset is a certain important set of
  options that we want to keep in a centralized location.  If you need to
  customize it, you should create another preset in a centralized location,
  rather than scattering the knowledge about the build across the system.)

  Presets support substitutions for controlled customizations.  Substitutions
  are defined in the preset file.  Values for substitutions are supplied
  using the name=value syntax on the command line.


Any arguments passed after "--" are forwarded directly to Swift's
'build-script-impl'.  See that script's help for details.

Environment variables
---------------------

This script respects a few environment variables, should you
choose to set them:

SWIFT_SOURCE_ROOT: a directory containing the source for LLVM, Clang, Swift.
                   If this script is located in a Swift
                   source directory, the location of SWIFT_SOURCE_ROOT will be
                   inferred if the variable is not set.

'build-script' expects the sources to be laid out in the following way:

   $SWIFT_SOURCE_ROOT/llvm
                     /clang
                     /swift
                     /lldb                       (optional)
                     /llbuild                    (optional)
                     /swiftpm                    (optional, requires llbuild)
                     /compiler-rt                (optional)
                     /swift-corelibs-xctest      (optional)
                     /swift-corelibs-foundation  (optional)
                     /swift-corelibs-libdispatch (optional)

SWIFT_BUILD_ROOT: a directory in which to create out-of-tree builds.
                  Defaults to "$SWIFT_SOURCE_ROOT/build/".

Preparing to run this script
----------------------------

  See README.md for instructions on cloning Swift subprojects.

If you intend to use the -l, -L, --lldb, or --lldb-debug options.

That's it; you're ready to go!

Examples
--------

Given the above layout of sources, the simplest invocation of 'build-script' is
just:

  [~/src/s]$ ./swift/utils/build-script

This builds LLVM, Clang, Swift and Swift standard library in debug mode.

All builds are incremental.  To incrementally build changed files, repeat the
same 'build-script' command.

Typical uses of 'build-script'
------------------------------

To build everything with optimization without debug information:

  [~/src/s]$ ./swift/utils/build-script -R

To run tests, add '-t':

  [~/src/s]$ ./swift/utils/build-script -R -t

To run normal tests and validation tests, add '-T':

  [~/src/s]$ ./swift/utils/build-script -R -T

To build LLVM+Clang with optimization without debug information, and a
debuggable Swift compiler:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift

To build a debuggable Swift standard library:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift-stdlib

iOS build targets are always configured and present, but are not built by
default.  To build the standard library for OS X, iOS simulator and iOS device:

  [~/src/s]$ ./swift/utils/build-script -R -i

To run OS X and iOS tests that don't require a device:

  [~/src/s]$ ./swift/utils/build-script -R -i -t

To use 'make' instead of 'ninja', use '-m':

  [~/src/s]$ ./swift/utils/build-script -m -R

To create Xcode projects that can build Swift, use '-x':

  [~/src/s]$ ./swift/utils/build-script -x -R

Preset mode in build-script
---------------------------

All buildbots and automated environments use 'build-script' in *preset mode*.
In preset mode, the command line only specifies the preset name and allows
limited customization (extra output paths).  The actual options come from
the selected preset in 'utils/build-presets.ini'.  For example, to build like
the incremental buildbot, run:

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_incremental

To build with AddressSanitizer:

  [~/src/s]$ ./swift/utils/build-script --preset=asan

To build a root for Xcode XYZ, '/tmp/xcode-xyz-root.tar.gz':

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_BNI_internal_XYZ \\
      install_destdir="/tmp/install"
      install_symroot="/tmp/symroot"
      installable_package="/tmp/xcode-xyz-root.tar.gz"

If you have your own favorite set of options, you can create your own, local,
preset.  For example, let's create a preset called 'ds' (which stands for
Debug Swift):

  $ cat > ~/.swift-build-presets
  [preset: ds]
  release
  debug-swift
  debug-swift-stdlib
  test
  build-subdir=ds

To use it, specify the '--preset=' argument:

  [~/src/s]$ ./swift/utils/build-script --preset=ds
  ./swift/utils/build-script: using preset 'ds', which expands to
  ./swift/utils/build-script --release --debug-swift --debug-swift-stdlib \
     --test
  --build-subdir=ds --
  ...

Philosophy
----------

While you can invoke CMake directly to build Swift, this tool will save you
time by taking away the mechanical parts of the process, providing you controls
for the important options.

For all automated build environments, this tool is regarded as *the* *only* way
to build Swift.  This is not a technical limitation of the Swift build system.
It is a policy decision aimed at making the builds uniform across all
environments and easily reproducible by engineers who are not familiar with the
details of the setups of other systems or automated environments.""")

    targets_group = parser.add_argument_group(
        title="Host and cross-compilation targets")
    targets_group.add_argument(
        "--host-target",
        help="The host target. LLVM, Clang, and Swift will be built for this "
             "target. The built LLVM and Clang will be used to compile Swift "
             "for the cross-compilation targets.",
        default=swift_build_support.targets.host_target())

    projects_group = parser.add_argument_group(
        title="Options to select projects")
    projects_group.add_argument(
        "-l", "--lldb",
        help="build LLDB",
        action="store_true",
        dest="build_lldb")
    projects_group.add_argument(
        "-b", "--llbuild",
        help="build llbuild",
        action="store_true",
        dest="build_llbuild")
    projects_group.add_argument(
        "-p", "--swiftpm",
        help="build swiftpm",
        action="store_true",
        dest="build_swiftpm")
    projects_group.add_argument(
        "--xctest",
        help="build xctest",
        action="store_true",
        dest="build_xctest")
    projects_group.add_argument(
        "--foundation",
        help="build foundation",
        action="store_true",
        dest="build_foundation")
    projects_group.add_argument(
        "--libdispatch",
        help="build libdispatch",
        action="store_true",
        dest="build_libdispatch")

    extra_actions_group = parser.add_argument_group(
        title="Extra actions to perform before or in addition to building")
    extra_actions_group.add_argument(
        "-c", "--clean",
        help="do a clean build",
        action="store_true")
    extra_actions_group.add_argument(
        "--export-compile-commands",
        help="generate compilation databases in addition to building",
        action="store_true")
    extra_actions_group.add_argument(
        "--symbols-package",
        metavar="PATH",
        help="if provided, an archive of the symbols directory will be "
        "generated at this path")

    build_variant_group = parser.add_mutually_exclusive_group(required=False)
    build_variant_group.add_argument(
        "-d", "--debug",
        help="""
build the Debug variant of everything (LLVM, Clang, Swift host tools, target
Swift standard libraries, LLDB (if enabled)
(default)""",
        action="store_const",
        const="Debug",
        dest="build_variant")
    build_variant_group.add_argument(
        "-r", "--release-debuginfo",
        help="""
build the RelWithDebInfo variant of everything (default is Debug)""",
        action="store_const",
        const="RelWithDebInfo",
        dest="build_variant")
    build_variant_group.add_argument(
        "-R", "--release",
        help="build the Release variant of everything (default is Debug)",
        action="store_const",
        const="Release",
        dest="build_variant")

    build_variant_override_group = parser.add_argument_group(
        title="Override build variant for a specific project")
    build_variant_override_group.add_argument(
        "--debug-llvm",
        help="build the Debug variant of LLVM",
        action="store_const",
        const="Debug",
        dest="llvm_build_variant")
    build_variant_override_group.add_argument(
        "--debug-swift",
        help="build the Debug variant of Swift host tools",
        action="store_const",
        const="Debug",
        dest="swift_build_variant")
    build_variant_override_group.add_argument(
        "--debug-swift-stdlib",
        help="""
build the Debug variant of the Swift standard library and SDK overlay""",
        action="store_const",
        const="Debug",
        dest="swift_stdlib_build_variant")
    build_variant_override_group.add_argument(
        "--debug-lldb",
        help="build the Debug variant of LLDB",
        action="store_const",
        const="Debug",
        dest="lldb_build_variant")
    build_variant_override_group.add_argument(
        "--debug-cmark",
        help="build the Debug variant of CommonMark",
        action="store_const",
        const="Debug",
        dest="cmark_build_variant")
    build_variant_override_group.add_argument(
        "--debug-foundation",
        help="build the Debug variant of Foundation",
        action="store_const",
        const="Debug",
        dest="foundation_build_variant")
    build_variant_override_group.add_argument(
        "--debug-libdispatch",
        help="build the Debug variant of libdispatch",
        action="store_const",
        const="Debug",
        dest="libdispatch_build_variant")

    assertions_group = parser.add_mutually_exclusive_group(required=False)
    assertions_group.add_argument(
        "--assertions",
        help="enable assertions in all projects",
        action="store_const",
        const=True,
        dest="assertions")
    assertions_group.add_argument(
        "--no-assertions",
        help="disable assertions in all projects",
        action="store_const",
        const=False,
        dest="assertions")

    assertions_override_group = parser.add_argument_group(
        title="Control assertions in a specific project")
    assertions_override_group.add_argument(
        "--cmark-assertions",
        help="enable assertions in CommonMark",
        action="store_const",
        const=True,
        dest="cmark_assertions")
    assertions_override_group.add_argument(
        "--llvm-assertions",
        help="enable assertions in LLVM",
        action="store_const",
        const=True,
        dest="llvm_assertions")
    assertions_override_group.add_argument(
        "--no-llvm-assertions",
        help="disable assertions in LLVM",
        action="store_const",
        const=False,
        dest="llvm_assertions")
    assertions_override_group.add_argument(
        "--swift-assertions",
        help="enable assertions in Swift",
        action="store_const",
        const=True,
        dest="swift_assertions")
    assertions_override_group.add_argument(
        "--no-swift-assertions",
        help="disable assertions in Swift",
        action="store_const",
        const=False,
        dest="swift_assertions")
    assertions_override_group.add_argument(
        "--swift-stdlib-assertions",
        help="enable assertions in the Swift standard library",
        action="store_const",
        const=True,
        dest="swift_stdlib_assertions")
    assertions_override_group.add_argument(
        "--no-swift-stdlib-assertions",
        help="disable assertions in the Swift standard library",
        action="store_const",
        const=False,
        dest="swift_stdlib_assertions")
    assertions_override_group.add_argument(
        "--lldb-assertions",
        help="enable assertions in LLDB",
        action="store_const",
        const=True,
        dest="lldb_assertions")
    assertions_override_group.add_argument(
        "--no-lldb-assertions",
        help="disable assertions in LLDB",
        action="store_const",
        const=False,
        dest="lldb_assertions")

    cmake_generator_group = parser.add_argument_group(
        title="Select the CMake generator")
    cmake_generator_group.add_argument(
        "-x", "--xcode",
        help="use CMake's Xcode generator (default is Ninja)",
        action="store_const",
        const="Xcode",
        dest="cmake_generator")
    cmake_generator_group.add_argument(
        "-m", "--make",
        help="use CMake's Makefile generator (default is Ninja)",
        action="store_const",
        const="Unix Makefiles",
        dest="cmake_generator")
    cmake_generator_group.add_argument(
        "-e", "--eclipse",
        help="use CMake's Eclipse generator (default is Ninja)",
        action="store_const",
        const="Eclipse CDT4 - Ninja",
        dest="cmake_generator")

    run_tests_group = parser.add_argument_group(
        title="Run tests")
    run_tests_group.add_argument(
        "-t", "--test",
        help="test Swift after building",
        action="store_true")
    run_tests_group.add_argument(
        "-T", "--validation-test",
        help="run the validation test suite (implies --test)",
        action="store_true")
    run_tests_group.add_argument(
        "--host-test",
        help="run executable tests on host devices (such as iOS or tvOS)",
        action="store_true")
    run_tests_group.add_argument(
        "-B", "--benchmark",
        help="run the Swift Benchmark Suite after building",
        action="store_true")
    run_tests_group.add_argument(
        "--skip-test-linux",
        help="skip testing Swift stdlibs for Linux",
        action="store_true")
    run_tests_group.add_argument(
        "--skip-test-freebsd",
        help="skip testing Swift stdlibs for FreeBSD",
        action="store_true")
    run_tests_group.add_argument(
        "--skip-test-cygwin",
        help="skip testing Swift stdlibs for Cygwin",
        action="store_true")
    parser.add_argument(
        "--build-runtime-with-host-compiler",
        help="Use the host compiler, not the self-built one to compile the " +
        "Swift runtime",
        action="store_true")

    parser.add_argument(
        "-o", "--test-optimized",
        help="run the test suite in optimized mode too (implies --test)",
        action="store_true")

    run_build_group = parser.add_argument_group(
        title="Run build")
    run_build_group.add_argument(
        "-S", "--skip-build",
        help="generate build directory only without building",
        action="store_true")
    run_build_group.add_argument(
        "--skip-build-linux",
        help="skip building Swift stdlibs for Linux",
        action="store_true")
    run_build_group.add_argument(
        "--skip-build-freebsd",
        help="skip building Swift stdlibs for FreeBSD",
        action="store_true")
    run_build_group.add_argument(
        "--skip-build-cygwin",
        help="skip building Swift stdlibs for Cygwin",
        action="store_true")

    run_build_group.add_argument(
        "--skip-build-benchmarks",
        help="skip building Swift Benchmark Suite",
        action="store_true")

    skip_test_group = parser.add_argument_group(
        title="Skip testing specified targets")
    skip_test_group.add_argument(
        "--skip-test-ios",
        help="skip testing all iOS targets. Equivalent to specifying both "
        "--skip-test-ios-simulator and --skip-test-ios-host",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-ios-simulator",
        help="skip testing iOS simulator targets",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-ios-host",
        help="skip testing iOS device targets on the host machine (the phone "
        "itself)",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-tvos",
        help="skip testing all tvOS targets. Equivalent to specifying both "
        "--skip-test-tvos-simulator and --skip-test-tvos-host",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-tvos-simulator",
        help="skip testing tvOS simulator targets",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-tvos-host",
        help="skip testing tvOS device targets on the host machine (the TV "
        "itself)",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-watchos",
        help="skip testing all tvOS targets. Equivalent to specifying both "
        "--skip-test-watchos-simulator and --skip-test-watchos-host",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-watchos-simulator",
        help="skip testing watchOS simulator targets",
        action="store_true")
    skip_test_group.add_argument(
        "--skip-test-watchos-host",
        help="skip testing watchOS device targets on the host machine (the "
        "watch itself)",
        action="store_true")

    parser.add_argument(
        "-i", "--ios",
        help="""
also build for iOS, but disallow tests that require an iOS device""",
        action="store_true")

    parser.add_argument(
        "--tvos",
        help="""
also build for tvOS, but disallow tests that require a tvos device""",
        action="store_true")

    parser.add_argument(
        "--watchos",
        help="""
also build for watchOS, but disallow tests that require an watchOS device""",
        action="store_true")

    parser.add_argument(
        "--swift-analyze-code-coverage",
        help="""enable code coverage analysis in Swift (false, not-merged,
        merged).""",
        choices=["false", "not-merged", "merged"],
        default="false",  # so CMake can see the inert mode as a false value
        dest="swift_analyze_code_coverage")

    parser.add_argument(
        "--build-subdir",
        help="""
name of the directory under $SWIFT_BUILD_ROOT where the build products will be
placed""",
        metavar="PATH")
    parser.add_argument(
        "--install-prefix",
        help="The installation prefix. This is where built Swift products "
        "(like bin, lib, and include) will be installed.",
        metavar="PATH",
        default=swift_build_support.targets.install_prefix())
    parser.add_argument(
        "--install-symroot",
        help="the path to install debug symbols into",
        metavar="PATH")

    parser.add_argument(
        "-j", "--jobs",
        help="""
the number of parallel build jobs to use""",
        type=int,
        dest="build_jobs",
        default=multiprocessing.cpu_count())

    parser.add_argument(
        "--darwin-xcrun-toolchain",
        help="the name of the toolchain to use on Darwin",
        default="default")
    parser.add_argument(
        "--cmake",
        help="the path to a CMake executable that will be "
        "used to build Swift")
    parser.add_argument(
        "--show-sdks",
        help="print installed Xcode and SDK versions",
        action="store_true")

    parser.add_argument(
        "--extra-swift-args", help=textwrap.dedent("""
    Pass through extra flags to swift in the form of a cmake list
    'module_regexp;flag'. Can be called multiple times to add multiple such
    module_regexp flag pairs. All semicolons in flags must be escaped with
    a '\'"""),
        action="append", dest="extra_swift_args", default=[])

    parser.add_argument(
        "build_script_impl_args",
        help="",
        nargs="*")

    args = parser.parse_args(migrate_impl_args(sys.argv[1:], [
        '--darwin-xcrun-toolchain',
        '--cmake',
        '--host-target',
        '--skip-build',
        '--show-sdks',
        '--install-prefix',
        '--install-symroot',
        '--symbols-package',
        '--skip-test-ios',
        '--skip-test-ios-simulator',
        '--skip-test-ios-host',
        '--skip-test-tvos',
        '--skip-test-tvos-simulator',
        '--skip-test-tvos-host',
        '--skip-test-watchos',
        '--skip-test-watchos-simulator',
        '--skip-test-watchos-host',
    ]))

    if args.host_target is None:
        print_with_argv0("Unknown operating system.")
        return 1

    if args.symbols_package:
        if not os.path.isabs(args.symbols_package):
            print(
                '--symbols-package must be an absolute path '
                '(was \'{}\')'.format(args.symbols_package))
            return 1
        if not args.install_symroot:
            print_with_argv0(
                "--install-symroot is required when specifying "
                "--symbols-package.")
            return 1

    # Build cmark if any cmark-related options were specified.
    if (args.cmark_build_variant is not None):
        args.build_cmark = True

    # Build LLDB if any LLDB-related options were specified.
    if args.lldb_build_variant is not None or \
       args.lldb_assertions is not None:
        args.build_lldb = True

    # Set the default build variant.
    if args.build_variant is None:
        args.build_variant = "Debug"

    if args.cmark_build_variant is None:
        args.cmark_build_variant = args.build_variant

    # Propagate the default build variant.
    if args.llvm_build_variant is None:
        args.llvm_build_variant = args.build_variant

    if args.swift_build_variant is None:
        args.swift_build_variant = args.build_variant

    if args.swift_stdlib_build_variant is None:
        args.swift_stdlib_build_variant = args.build_variant

    if args.lldb_build_variant is None:
        args.lldb_build_variant = args.build_variant

    if args.foundation_build_variant is None:
        args.foundation_build_variant = args.build_variant

    if args.libdispatch_build_variant is None:
        args.libdispatch_build_variant = args.build_variant

    # Assertions are enabled by default.
    if args.assertions is None:
        args.assertions = True

    # Propagate the default assertions setting.
    if args.cmark_assertions is None:
        args.cmark_assertions = args.assertions

    if args.llvm_assertions is None:
        args.llvm_assertions = args.assertions

    if args.swift_assertions is None:
        args.swift_assertions = args.assertions

    if args.swift_stdlib_assertions is None:
        args.swift_stdlib_assertions = args.assertions

    # Set the default CMake generator.
    if args.cmake_generator is None:
        args.cmake_generator = "Ninja"

    # --validation-test implies --test.
    if args.validation_test:
        args.test = True

    # --test-optimized implies --test.
    if args.test_optimized:
        args.test = True

    # XCTest has a dependency on Foundation.
    # On OS X, Foundation is built automatically using xcodebuild.
    # On Linux, we must ensure that it is built manually.
    if args.build_xctest and platform.system() != "Darwin":
        args.build_foundation = True

    # --skip-test-ios is merely a shorthand for host and simulator tests.
    if args.skip_test_ios:
        args.skip_test_ios_host = True
        args.skip_test_ios_simulator = True
    # --skip-test-tvos is merely a shorthand for host and simulator tests.
    if args.skip_test_tvos:
        args.skip_test_tvos_host = True
        args.skip_test_tvos_simulator = True
    # --skip-test-watchos is merely a shorthand for host and simulator tests.
    if args.skip_test_watchos:
        args.skip_test_watchos_host = True
        args.skip_test_watchos_simulator = True

    build_script_impl_inferred_args = []

    if args.export_compile_commands:
        build_script_impl_inferred_args += [
            "--export-compile-commands"
        ]

    if not args.test:
        build_script_impl_inferred_args += [
            "--skip-test-cmark",
            "--skip-test-lldb",
            "--skip-test-swift",
            "--skip-test-llbuild",
            "--skip-test-swiftpm",
            "--skip-test-xctest",
            "--skip-test-foundation",
            "--skip-test-libdispatch",
            "--skip-test-linux",
            "--skip-test-freebsd",
            "--skip-test-cygwin",
            "--skip-test-osx",
            "--skip-test-ios-simulator",
            "--skip-test-ios-host",
            "--skip-test-tvos-simulator",
            "--skip-test-tvos-host",
            "--skip-test-watchos-simulator",
            "--skip-test-watchos-host",
        ]

    if not args.validation_test:
        build_script_impl_inferred_args += [
            "--skip-test-validation"
        ]

    if not args.host_test:
        build_script_impl_inferred_args += [
            "--skip-test-ios-host",
            "--skip-test-tvos-host",
            "--skip-test-watchos-host",
        ]

    if not args.benchmark:
        build_script_impl_inferred_args += [
            "--skip-test-benchmarks"
        ]

    if not args.test_optimized:
        build_script_impl_inferred_args += [
            "--skip-test-optimized"
        ]

    if not args.ios:
        build_script_impl_inferred_args += [
            "--skip-build-ios",
            "--skip-test-ios-simulator",
            "--skip-test-ios-host",
        ]

    if not args.tvos:
        build_script_impl_inferred_args += [
            "--skip-build-tvos",
            "--skip-test-tvos-simulator",
            "--skip-test-tvos-host",
        ]

    if not args.watchos:
        build_script_impl_inferred_args += [
            "--skip-build-watchos",
            "--skip-test-watchos-simulator",
            "--skip-test-watchos-host",
        ]

    if not args.build_lldb:
        build_script_impl_inferred_args += [
            "--skip-build-lldb"
        ]

    if not args.build_llbuild:
        build_script_impl_inferred_args += [
            "--skip-build-llbuild"
        ]

    if not args.build_swiftpm:
        build_script_impl_inferred_args += [
            "--skip-build-swiftpm"
        ]

    if not args.build_xctest:
        build_script_impl_inferred_args += [
            "--skip-build-xctest"
        ]

    if not args.build_foundation:
        build_script_impl_inferred_args += [
            "--skip-build-foundation"
        ]

    if not args.build_libdispatch:
        build_script_impl_inferred_args += [
            "--skip-build-libdispatch"
        ]

    if args.skip_build_benchmarks:
        build_script_impl_inferred_args += [
            "--skip-build-benchmarks"
        ]

    if args.skip_build:
        build_script_impl_inferred_args += [
            "--skip-build-cmark",
            "--skip-build-llvm",
            "--skip-build-swift",
            "--skip-build-linux",
            "--skip-build-freebsd",
            "--skip-build-cygwin",
            "--skip-build-osx",
            "--skip-build-ios",
            "--skip-build-ios-device",
            "--skip-build-ios-simulator",
            "--skip-build-tvos",
            "--skip-build-tvos-device",
            "--skip-build-tvos-simulator",
            "--skip-build-watchos",
            "--skip-build-watchos-device",
            "--skip-build-watchos-simulator",
            "--skip-build-lldb",
            "--skip-build-llbuild",
            "--skip-build-swiftpm",
            "--skip-build-xctest",
            "--skip-build-foundation",
            "--skip-build-libdispatch",
            "--skip-build-benchmarks",
        ]

    if platform.system() == 'Darwin':
        build_script_impl_inferred_args += [
            "--toolchain-prefix",
            swift_build_support.targets.darwin_toolchain_prefix(
                args.install_prefix),
        ]

    if args.build_subdir is None:
        # Create a name for the build directory.
        args.build_subdir = args.cmake_generator.replace(" ", "_")
        cmark_build_dir_label = args.cmark_build_variant
        if args.cmark_assertions:
            cmark_build_dir_label += "Assert"

        llvm_build_dir_label = args.llvm_build_variant
        if args.llvm_assertions:
            llvm_build_dir_label += "Assert"

        swift_build_dir_label = args.swift_build_variant
        if args.swift_assertions:
            swift_build_dir_label += "Assert"
        if args.swift_analyze_code_coverage != "false":
            swift_build_dir_label += "Coverage"

        swift_stdlib_build_dir_label = args.swift_stdlib_build_variant
        if args.swift_stdlib_assertions:
            swift_stdlib_build_dir_label += "Assert"

        # FIXME: mangle LLDB build configuration into the directory name.
        if (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Use a simple directory name if all projects use the same build
            # type.
            args.build_subdir += "-" + llvm_build_dir_label
        elif (llvm_build_dir_label != swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Swift build type differs.
            args.build_subdir += "-" + llvm_build_dir_label
            args.build_subdir += "+swift-" + swift_build_dir_label
        elif (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label != swift_stdlib_build_dir_label and
                llvm_build_dir_label == cmark_build_dir_label):
            # Swift stdlib build type differs.
            args.build_subdir += "-" + llvm_build_dir_label
            args.build_subdir += "+stdlib-" + swift_stdlib_build_dir_label
        elif (llvm_build_dir_label == swift_build_dir_label and
                llvm_build_dir_label == swift_stdlib_build_dir_label and
                llvm_build_dir_label != cmark_build_dir_label):
            # cmark build type differs.
            args.build_subdir += "-" + llvm_build_dir_label
            args.build_subdir += "+cmark-" + cmark_build_dir_label
        else:
            # We don't know how to create a short name, so just mangle in all
            # the information.
            args.build_subdir += "+cmark-" + cmark_build_dir_label
            args.build_subdir += "+llvm-" + llvm_build_dir_label
            args.build_subdir += "+swift-" + swift_build_dir_label
            args.build_subdir += "+stdlib-" + swift_stdlib_build_dir_label

    build_dir = os.path.join(SWIFT_BUILD_ROOT, args.build_subdir)

    if args.clean and os.path.isdir(build_dir):
        shutil.rmtree(build_dir)

    host_clang = swift_build_support.clang.host_clang(
        xcrun_toolchain=args.darwin_xcrun_toolchain)
    if not host_clang:
        print_with_argv0(
            "Can't find clang.  Please install clang-3.5 or a later version.")
        return 1

    host_cmake = args.cmake
    if not host_cmake:
        host_cmake = swift_build_support.cmake.host_cmake(
            args.darwin_xcrun_toolchain)
    if not host_cmake:
        print_with_argv0("Can't find CMake. Please install CMake.")
        return 1
    build_script_impl_args = [
        os.path.join(SWIFT_SOURCE_ROOT, "swift", "utils", "build-script-impl"),
        "--build-dir", build_dir,
        "--install-prefix", os.path.abspath(args.install_prefix),
        "--host-target", args.host_target,
        "--host-cc", host_clang.cc,
        "--host-cxx", host_clang.cxx,
        "--darwin-xcrun-toolchain", args.darwin_xcrun_toolchain,
        "--cmake", host_cmake,
        "--cmark-build-type", args.cmark_build_variant,
        "--llvm-build-type", args.llvm_build_variant,
        "--swift-build-type", args.swift_build_variant,
        "--swift-stdlib-build-type", args.swift_stdlib_build_variant,
        "--lldb-build-type", args.lldb_build_variant,
        "--llvm-enable-assertions", str(args.llvm_assertions).lower(),
        "--swift-enable-assertions", str(args.swift_assertions).lower(),
        "--swift-stdlib-enable-assertions", str(
            args.swift_stdlib_assertions).lower(),
        "--swift-analyze-code-coverage", str(
            args.swift_analyze_code_coverage).lower(),
        "--cmake-generator", args.cmake_generator,
        "--build-jobs", str(args.build_jobs),
        "--workspace", SWIFT_SOURCE_ROOT
    ]
    if args.install_symroot:
        build_script_impl_args += [
            "--install-symroot", os.path.abspath(args.install_symroot)
        ]
    if args.build_foundation:
        build_script_impl_args += [
            "--foundation-build-type", args.foundation_build_variant
        ]
    if args.cmake_generator == 'Ninja' and \
       not swift_build_support.ninja.is_ninja_installed():
        build_script_impl_args += ["--build-ninja"]
    if args.skip_build_linux:
        build_script_impl_args += ["--skip-build-linux"]
    if args.skip_build_freebsd:
        build_script_impl_args += ["--skip-build-freebsd"]
    if args.skip_build_cygwin:
        build_script_impl_args += ["--skip-build-cygwin"]
    if args.skip_test_linux:
        build_script_impl_args += ["--skip-test-linux"]
    if args.skip_test_freebsd:
        build_script_impl_args += ["--skip-test-freebsd"]
    if args.skip_test_cygwin:
        build_script_impl_args += ["--skip-test-cygwin"]
    if args.skip_test_ios_host:
        build_script_impl_args += ["--skip-test-ios-host"]
    if args.skip_test_ios_simulator:
        build_script_impl_args += ["--skip-test-ios-simulator"]
    if args.skip_test_tvos_host:
        build_script_impl_args += ["--skip-test-tvos-host"]
    if args.skip_test_tvos_simulator:
        build_script_impl_args += ["--skip-test-tvos-simulator"]
    if args.skip_test_watchos_host:
        build_script_impl_args += ["--skip-test-watchos-host"]
    if args.skip_test_watchos_simulator:
        build_script_impl_args += ["--skip-test-watchos-simulator"]
    if args.build_runtime_with_host_compiler:
        build_script_impl_args += ["--build-runtime-with-host-compiler"]
    build_script_impl_args += build_script_impl_inferred_args

    # If we have extra_swift_args, combine all of them together and then add
    # them as one command.
    if args.extra_swift_args:
        build_script_impl_args += [
            "--extra-swift-args",
            ";".join(args.extra_swift_args)]

    build_script_impl_args += args.build_script_impl_args

    # Unset environment variables that might affect how tools behave.
    for v in [
            'MAKEFLAGS',
            'SDKROOT',
            'MACOSX_DEPLOYMENT_TARGET',
            'IPHONEOS_DEPLOYMENT_TARGET']:
        os.environ.pop(v, None)

    if args.show_sdks:
        swift_build_support.debug.print_xcodebuild_versions([
            'iphonesimulator',
            'appletvsimulator',
            'watchsimulator',
        ])

    check_call(build_script_impl_args)

    if args.symbols_package:
        print('--- Creating symbols package ---')
        print('-- Package file: {} --'.format(args.symbols_package))

        if platform.system() == 'Darwin':
            prefix = swift_build_support.targets.darwin_toolchain_prefix(
                args.install_prefix)
        else:
            prefix = args.install_prefix

        # As a security measure, `tar` normally strips leading '/' from paths
        # it is archiving. To stay safe, we change working directories, then
        # run `tar` without the leading '/' (we remove it ourselves to keep
        # `tar` from emitting a warning).
        with WorkingDirectory(args.install_symroot):
            swift_build_support.tar.tar(source=prefix.lstrip('/'),
                                        destination=args.symbols_package)

    return 0


def main():
    if not SWIFT_SOURCE_ROOT:
        print_with_argv0(
            "Could not infer source root directory.  " +
            "Forgot to set $SWIFT_SOURCE_ROOT environment variable?")
        return 1

    if not os.path.isdir(SWIFT_SOURCE_ROOT):
        print_with_argv0(
            "Source root directory \'" + SWIFT_SOURCE_ROOT +
            "\' does not exist.  " +
            "Forgot to set $SWIFT_SOURCE_ROOT environment variable?")
        return 1

    # Determine if we are invoked in the preset mode and dispatch accordingly.
    if any([(opt.startswith("--preset") or opt == "--show-presets")
            for opt in sys.argv[1:]]):
        return main_preset()
    else:
        return main_normal()


if __name__ == "__main__":
    # Since this script is invoked from ./utils/build-script like:
    #   python -m swift_build_support ./utils/build-script --options
    # argv[0] is "path/to/__main__.py".
    # Shift argv here,  so that we can process it normally.
    sys.argv = sys.argv[1:]
    sys.exit(main())
