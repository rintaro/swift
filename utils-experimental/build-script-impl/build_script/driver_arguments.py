# build_script/driver_arguments.py ------------------------------*- python -*-
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
The build script command line arguments definitions, including parser factory.
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os.path
import argparse
import multiprocessing

from .host import host
from . import defaults
from .utils import ArgParserBuilder

__all__ = [
    'create_argparser',
    'Args',
]


class Args(object):
    ''' Simple object to store parsed arguments.
    '''

    @classmethod
    def freeze(cls, obj):
        '''
        Make given Args object immutable. After this, no one can set or delete
        any attributes on this object.
        '''
        obj.__dict__['@frozen@'] = True

    def __init__(self, **kwargs):
        for name in kwargs:
            setattr(self, name, kwargs[name])

    def __setattr__(self, attr, value):
        if self.__dict__.get('@frozen@'):
            raise AttributeError("Can't set attribute on frozen object")
        self.__dict__[attr] = value

    def __delattr__(self, attr):
        if self.__dict__.get('@frozen@'):
            raise AttributeError("Can't delete attribute on frozen object")
        del self.__dict__[attr]


def create_argparser():
    '''Return configured argument parser.
    '''

    builder = ArgParserBuilder(
        usage="%(prog)s [-h | --help] [OPTIONS...]",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=DESCRIPTION,
        epilog=EPILOG)
    # Note: DESCRIPTION and EPLOG are defined at the bottom of this file.

    # Prepare DSL functions
    in_group = builder.in_group
    option = builder.add_option
    set_defaults = builder.set_defaults
    mutually_exclusive_group = builder.mutually_exclusive_group
    set_ = builder.set_action
    disable = builder.disable_action
    enable = builder.enable_action
    append = builder.append_action

    def realpath(path):
        path = os.path.expanduser(path)
        path = os.path.abspath(path)
        path = os.path.realpath(path)
        return path

    # ------------------------------------------------------------------------

    option(["-n", "--dry-run"], enable, nargs=0, default=False,
           help="Print the commands that would be executed, but do not "
                "execute them")

    option("--show-sdks", enable, default=False, nargs=0,
           help="print installed Xcode and SDK versions")

    # ------------------------------------------------------------------------
    in_group("Workspace settings")

    option("--build-dir", set_('build_root', type=realpath),
           metavar="PATH",
           help="out-of-tree build directory; default is in-tree.")

    option("--workspace", set_('source_root', type=realpath),
           metavar="PATH",
           help="source directory containing llvm, clang, swift, and other "
                "optional products")

    option("--build-subdir", set_,
           metavar="PATH",
           help="name of the directory under $SWIFT_BUILD_ROOT where the "
                "build products will be placed")

    option("--darwin-xcrun-toolchain", set_, default="default",
           metavar="NAME",
           help="the name of the toolchain to use on Darwin")

    option("--cmake", set_,
           metavar="PATH",
           help="the path to a CMake executable that will be "
                "used to build Swift")

    option("--build-args", append(separator=" "),
           help="arguments to the build tool")

    option(["-j", "--jobs", "--build-jobs"], set_("build_jobs", type=int),
           default=multiprocessing.cpu_count(),
           metavar="NUM",
           help="the number of parallel build jobs to use")

    option("--native-llvm-tools-path", set_,
           metavar="PATH",
           help="directory that contains LLVM tools that are executable on "
                "the build machine")

    option("--native-clang-tools-path", set_,
           metavar="PATH",
           help="directory that contains Clang tools that are executable on "
                "the build machine")

    option("--native-swift-tools-path", set_,
           metavar="PATH",
           help="directory that contains Swift tools that are executable on "
                "the build machine")

    option('--host-cc', set_,
           metavar="PATH",
           help="the path to CC, the 'clang' compiler for the host platform.")

    option('--host-cxx', set_,
           metavar="PATH",
           help="the path to CXX, the 'clang++' compiler for the host "
                "platform.")

    option("--distcc", enable,
           help="use distcc in pump mode")

    # -----------------------------------------------------------------------
    in_group("Select the CMake generator")

    with mutually_exclusive_group():
        set_defaults(cmake_generator=defaults.CMAKE_GENERATOR)

        option(["-x", "--xcode"],
               set_("cmake_generator", const="Xcode"),
               help="use CMake's Xcode generator (default is Ninja)")

        option(["-m", "--make"],
               set_("cmake_generator", const="Unix Makefiles"),
               help="use CMake's Makefile generator (default is Ninja)")

        option(["-e", "--eclipse"],
               set_("cmake_generator", const="Eclipse CDT4 - Ninja"),
               help="use CMake's Eclipse generator (default is Ninja)")

        option("--cmake-generator", set_,
               help="kind of build system to generate; see output of 'cmake "
                    "--help' for choices")

    # ------------------------------------------------------------------------
    in_group("Extra actions to perform before or in addition to building")

    option(["-c", "--clean"], enable, default=False, nargs=0,
           help="do a clean build")

    option("--export-compile-commands", enable, default=False,
           help="generate compilation databases in addition to building")

    option("--reconfigure", enable,
           help="force a CMake configuration run even if CMakeCache.txt "
                "already exists")

    option("--verbose-build", enable,
           help="print the commands executed during the build of each "
                "products")

    # ------------------------------------------------------------------------
    in_group("Host and cross-compilation targets")

    option("--host-target", set_, default=host.deployment_target(),
           help="The host target. LLVM, Clang, and Swift will be built for "
                "this target. The built LLVM and Clang will be used to "
                "compile Swift for the cross-compilation targets.")

    option("--stdlib-deployment-targets", set_, nargs="*",
           help="list of targets to compile or cross-compile the Swift "
                "standard library for.")

    all_skip_ios = [
        'skip_build_ios',
        'skip_build_ios_simulator',
        'skip_build_ios_device',
        'skip_test_ios_simulator',
        'skip_test_ios_host']

    all_skip_tvos = [
        'skip_build_tvos',
        'skip_build_tvos_simulator',
        'skip_build_tvos_device',
        'skip_test_tvos_simulator',
        'skip_test_tvos_host']

    all_skip_watchos = [
        'skip_build_watchos',
        'skip_build_watchos_simulator',
        'skip_build_watchos_device',
        'skip_test_watchos_simulator',
        'skip_test_watchos_host']

    set_defaults(all_skip_ios, True)
    set_defaults(all_skip_tvos, True)
    set_defaults(all_skip_watchos, True)

    with mutually_exclusive_group():
        option("--ios", disable(all_skip_ios),
               help="also build for iOS, but disallow tests that require an "
                    "iOS device")
        option(["--skip-ios", "--skip-build-ios"], enable(all_skip_ios),
               help="set to skip everything iOS-related")

    option("--skip-build-ios-device", enable,
           help="set to skip building Swift stdlibs for iOS devices (i.e. "
                "build simulators only)")
    option("--skip-build-ios-simulator", enable,
           help="set to skip building Swift stdlibs for iOS simulators (i.e. "
                "build devices only)")

    with mutually_exclusive_group():
        option("--tvos", disable(all_skip_tvos),
               help="also build for tvOS, but disallow tests that require a "
                    "tvos device")
        option(["--skip-tvos", "--skip-build-tvos"], enable(all_skip_tvos),
               help="set to skip everything tvOS-related")

    option("--skip-build-tvos-device", enable,
           help="set to skip building Swift stdlibs for tvOS devices (i.e. "
                "build simulators only)")
    option("--skip-build-tvos-simulator", enable,
           help="set to skip building Swift stdlibs for tvOS simulators (i.e. "
                "build devices only)")

    with mutually_exclusive_group():
        option("--watchos", disable(all_skip_watchos),
               help="also build for watchOS, but disallow tests that require "
                    "an watchOS device")
        option(["--skip-watchos", "--skip-build-watchos"],
               disable(all_skip_watchos),
               help="set to skip everything watchOS-related")

    option("--skip-build-watchos-device", enable,
           help="set to skip building Swift stdlibs for Apple watchOS devices "
                "(i.e. build simulators only)")
    option("--skip-build-watchos-simulator", enable,
           help="set to skip building Swift stdlibs for Apple watchOS "
                "simulators (i.e. build devices only)")

    with mutually_exclusive_group():
        set_defaults(skip_build_android=True)
        option("--android", disable("skip_build_android"),
               help="also build for Android")
        option("--skip-build-android", enable,
               help="set to skip building Swift stdlibs for Android")

    option("--cross-compile-tools-deployment-targets", append(separator=" "),
           metavar="TARGET",
           help="space-separated list of targets to cross-compile host Swift "
                "tools for")

    option("--darwin-deployment-version-osx", set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_OSX,
           metavar="VERSION",
           help="minimum deployment target version for OS X")

    option("--darwin-deployment-version-ios", set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_IOS,
           metavar="VERSION",
           help="minimum deployment target version for iOS")

    option("--darwin-deployment-version-tvos", set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_TVOS,
           metavar="VERSION",
           help="minimum deployment target version for tvOS")

    option("--darwin-deployment-version-watchos", set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS,
           metavar="VERSION",
           help="minimum deployment target version for watchOS")

    # -----------------------------------------------------------------------
    in_group("Options to select projects")
    all_required_product_skip_builds = [
        "skip_build_cmark",
        "skip_build_llvm",
        "skip_build_swift"]

    all_optional_product_skip_builds = [
        "skip_build_lldb",
        "skip_build_llbuild",
        "skip_build_swiftpm",
        "skip_build_xctest",
        "skip_build_foundation",
        "skip_build_libdispatch"]

    set_defaults(all_required_product_skip_builds, False)
    set_defaults(all_optional_product_skip_builds, True)

    all_product_skip_builds = (
            all_required_product_skip_builds +
            all_optional_product_skip_builds)

    option(["-S", "--skip-build"], enable(all_product_skip_builds),
           help="generate build directory only without building")

    option("--skip-build-cmark", enable,
           help="set to skip building CommonMark")
    option("--skip-build-llvm", enable,
           help="set to skip building LLVM/Clang")
    option("--skip-build-swift", enable,
           help="set to skip building Swift")

    with mutually_exclusive_group():
        option(["-l", "--lldb"], disable('skip_build_lldb'),
               help="build LLDB")
        option("--skip-build-lldb", enable,
               help="set to skip building LLDB")

    with mutually_exclusive_group():
        option(["-b", "--llbuild"], disable('skip_build_llbuild'),
               help="build llbuild")
        option("--skip-build-llbuild", enable,
               help="set to skip building llbuild")

    with mutually_exclusive_group():
        option(["-p", "--swiftpm"], disable('skip_build_swiftpm'),
               help="build swiftpm")
        option("--skip-build-swiftpm", enable,
               help="set to skip building swiftpm")

    # XCTest has a dependency on Foundation.
    # On OS X, Foundation is built automatically using xcodebuild.
    # On Linux, we must ensure that it is built manually.
    skip_build_xctest_dst = ['skip_build_xctest']
    if not host.is_darwin():
        skip_build_xctest_dst.append('skip_build_foundation')

    with mutually_exclusive_group():
        option("--xctest", disable(skip_build_xctest_dst),
               help="build xctest")
        option("--skip-build-xctest", enable,
               help="set to skip building xctest")

    with mutually_exclusive_group():
        option("--foundation", disable("skip_build_foundation"),
               help="build foundation")
        option("--skip-build-foundation", enable,
               help="set to skip building foundation")

    with mutually_exclusive_group():
        option("--libdispatch", disable("skip_build_libdispatch"),
               help="build libdispatch")
        option("--skip-build-libdispatch", enable,
               help="set to skip building libdispatch")

    option("--build-ninja", enable(default=False),
           help="build the Ninja tool")

    # -----------------------------------------------------------------------
    in_group("Build variant for projects")

    with mutually_exclusive_group():
        all_build_types = [
            'cmark_build_type', 'llvm_build_type',
            'swift_build_type', 'swift_stdlib_build_type',
            'lldb_build_type', 'llbuild_build_type',
            'foundation_build_type']

        set_defaults(all_build_types, defaults.BUILD_TYPE)

        option(["-d", "--debug"],
               set_(all_build_types, const="Debug"),
               help="build the Debug variant of everything (LLVM, Clang, "
                    "Swift host tools, target Swift standard libraries, LLDB "
                    "(if enabled) (default)")

        option(["-r", "--release-debuginfo"],
               set_(all_build_types, const="RelWithDebInfo"),
               help="build the RelWithDebInfo variant of everything (default "
                    "is Debug)")

        option(["-R", "--release"],
               set_(all_build_types, const="Release"),
               help="build the Release variant of everything (default is "
                    "Debug)")

    build_types = ['Debug', 'RelWithDebInfo', 'Release', 'MinSizeRel']

    with mutually_exclusive_group():
        option("--debug-llvm",
               set_("llvm_build_tyoe", const="Debug"),
               help="build the Debug variant of LLVM")
        option("--llvm-build-type", set_(choices=build_types),
               help="the CMake build variant for LLVM and Clang")

    with mutually_exclusive_group():
        option("--debug-swift",
               set_("swift_build_type", const="Debug"),
               help="build the Debug variant of Swift host tools")
        option("--swift-build-type", set_(choices=build_types),
               help="the CMake build variant for Swift")

    with mutually_exclusive_group():
        option("--debug-swift-stdlib",
               set_("swift_stdlib_build_type", const="Debug"),
               help="build the Debug variant of the Swift standard library "
                    "and SDK overlay")
        option("--swift-stdlib-build-type", set_(choices=build_types),
               help="the CMake build variant for Swift standard library")

    with mutually_exclusive_group():
        option("--debug-cmark", set_("cmark_build_type", const="Debug"),
               help="build the Debug variant of CommonMark")
        option("--cmark-build-type", set_(choices=build_types),
               help="the CMake build variant for CommonMark")

    with mutually_exclusive_group():
        option("--debug-lldb",
               set_("lldb_build_type", const="Debug"),
               disable("skip_build_lldb"),
               help="build the Debug variant of LLDB")
        option("--lldb-build-type",
               set_("lldb_build_type", choices=build_types),
               disable("skip_build_lldb"),
               help="the CMake build variant for LLDB")

    with mutually_exclusive_group():
        option("--debug-foundation",
               set_("foundation_build_type", const="Debug"),
               disable("skip_build_foundation"),
               help="build the Debug variant of Foundation")
        option("--foundation-build-type",
               set_(choices=build_types),
               disable("skip_build_foundation"),
               help="the build variant for Foundation")

    with mutually_exclusive_group():
        option("--debug-llbuild",
               set_("llbuild_build_type", const="Debug"),
               disable("skip-build-llbuild"),
               help="build the Debug variant of Foundation")
        option("--llbuild-build-type",
               set_(choices=build_types),
               disable("skip_build_llbuild"),
               help="the build variant for Foundation")

    option("--debug-libdispatch",
           # FIXME: Not used in build-script-impl
           set_("libdispatch_build_type", const="Debug"),
           disable("skip_build_libdispatch"),
           help="build the Debug variant of libdispatch")

    # -----------------------------------------------------------------------
    in_group("Control assertions in each project")

    with mutually_exclusive_group():
        all_assertions = [
            "llvm_enable_assertions",
            "swift_enable_assertions",
            "swift_stdlib_enable_assertions",
            "llbuild_enable_assertions",

            # FIXME: these values are not used in build-script-impl
            "lldb_enable_assertions",
            "cmark_enable_assertions",
        ]

        set_defaults(all_assertions, True)

        option("--assertions", enable(all_assertions),
               help="enable assertions in all projects")
        option("--no-assertions", disable(all_assertions),
               help="enable assertions in all projects")

    # FIXME: cmark_enable_assertions doesn't exists in build-script-impl
    with mutually_exclusive_group():
        option("--cmark-assertions", enable("cmark_enable_assertions"),
               help="enable assertions in CommonMark")

    # TODO: --{product}-enable-assertions are added just for old
    #       `build-script-impl` compatibility. Shoud remove them.

    with mutually_exclusive_group():
        option(["--llvm-assertions", "--llvm-enable-assertions"],
               enable("llvm_enable_assertions"),
               help="enable assertions in LLVM")
        option("--no-llvm-assertions",
               disable("llvm_enable_assertions"),
               help="disable assertions in LLVM")

    # FIXME: Unused old build-script-impl option
    option("--enable-llvm-assertions", enable,
           help="set to enable llvm assertions")

    with mutually_exclusive_group():
        option(["--swift-assertions", "--swift-enable-assertions"],
               enable("swift_enable_assertions"),
               help="enable assertions in Swift")
        option("--no-swift-assertions",
               disable("swift_enable_assertions"),
               help="disable assertions in Swift")

    with mutually_exclusive_group():
        option(["--swift-stdlib-assertions",
                "--swift-stdlib-enable-assertions"],
               enable("swift_stdlib_enable_assertions"),
               help="enable assertions in the Swift standard library")
        option("--no-swift-stdlib-assertions",
               disable("swift_stdlib_enable_assertions"),
               help="disable assertions in the Swift standard library")

    with mutually_exclusive_group():
        option(["--llbuild-assertions", "--llbuild-enable-assertions"],
               enable("llbuild_enable_assertions"),
               disable("skip_build_llbuild"),
               help="enable assertions in LLBuild")
        option("--no-llbuild-assertions",
               disable("llbuild_enable_assertions"),
               disable("skip_build_llbuild"),
               help="disable assertions in LLBuild")

    with mutually_exclusive_group():
        option("--lldb-assertions",
               # FIXME: lldb-enable-assertions is not used in lldb builder
               enable("lldb_enable_assertions"),
               disable("skip_build_lldb"),
               help="enable assertions in LLDB")
        option("--no-lldb-assertions",
               disable("lldb_enable_assertions"),
               disable("skip_build_lldb"),
               help="disable assertions in LLDB")

    # ------------------------------------------------------------------------
    in_group("Sanitizers")

    option("--enable-asan", enable(),
           help="enable Address Sanitizer")

    option("--enable-ubsan", enable(),
           help="enable Undefined Behavior Sanitizer")

    # ------------------------------------------------------------------------
    in_group("Swift specific build options")

    option("--swift-sdks", set_,
           help="build target binaries only for specified SDKs "
                "(semicolon-separated list)")

    option("--swift-primary-variant-sdk", set_,
           help="default SDK for target binaries")

    option("--swift-primary-variant-arch", set_,
           help="default arch for target binaries")

    option("--swift-analyze-code-coverage",
           set_(choices=["false", "not-merged", "merged"], default="false"),
           # so CMake can see the inert mode as a false value
           help="enable code coverage analysis in Swift")

    option("--swift-enable-ast-verifier", enable(default=True),
           help="If enabled, and the assertions are enabled, the built Swift "
                "compiler will run the AST verifier every time it is invoked")

    option("--build-runtime-with-host-compiler", enable(default=True),
           help="Use the host compiler, not the self-built one to compile the "
                "Swift runtime")

    option("--extra-swift-args", append(join=";"),
           metavar="\"MODULE_REGEXP;FLAG\"",
           help="Pass through extra flags to swift in the form of a cmake "
                "list 'module_regexp;flag'. Can be called multiple times to "
                "add multiple such module_regexp flag pairs."),

    option("--swift-enable-lto", enable,
           help="enable LTO compilation of just Swift.")

    option("--swift-stdlib-enable-reflection-metadata", enable,
           help="build the Swift stdlib and overlays with remote reflection "
                "metadata")

    option("--swift-stdlib-enable-resilience", enable,
           help="build the Swift stdlib and overlays with resilience enabled")

    option("--swift-stdlib-sil-serialize-all", enable(default=True),
           help="build the Swift stdlib and overlays with all method bodies "
                "serialized")

    option("--build-serialized-stdlib-unittest", enable,
           help="set to 1 to build the StdlibUnittest module with "
                "-sil-serialize-all")

    option("--build-sil-debugging-stdlib", enable,
           help="set to 1 to build the Swift standard library with -gsil to "
                "enable debugging and profiling on SIL leve'")

    option("--build-swift-tools", enable(default=True),
           help="build Swift host tools")

    option("--build-swift-stdlib", enable(default=True),
           help="build the Swift standard library")

    option("--build-swift-stdlib-unittest-extra", enable,  # See tests sec.
           help="build optional StdlibUnittest components")

    option("--build-swift-sdk-overlay", enable(default=True),
           help="build the Swift SDK overlay")

    option("--build-swift-static-stdlib", enable,
           help="build static versions of the Swift standard library and SDK "
                "overlay")

    option("--build-swift-examples", enable(default=True),
           help="build static versions of the Swift standard library and SDK "
                "overlay")

    option("--embed-bitcode-section", enable,
           help="embed an LLVM bitcode section in stdlib/overlay binaries "
                "for supported platforms")

    option("--darwin-crash-reporter-client", enable,
           help="whether to enable CrashReporter integration")

    option("--darwin-stdlib-install-name-dir", set_,
           metavar="PATH",
           help="the directory of the install_name for standard library "
                "dylibs")

    option("--sil-verify-all", enable,
           help="If enabled, run the SIL verifier after each transform when "
                "building Swift files during this build process")

    option("--swift-runtime-enable-leak-checker", enable,
           help="Enable leaks checking routines in the runtime")

    # -----------------------------------------------------------------------
    in_group("Other build options")

    option("--use-gold-linker", enable,
           help="Enable using the gold linker")

    option("--source-tree-includes-tests", enable(default=True),
           help="set to 0 to allow the build to proceed when 'test' directory "
                "is missing (required for B&I builds)")

    option("--compiler-vendor", set_(choices=["none", "apple"]),
           default=defaults.COMPILER_VENDOR,
           help="compiler vendor name")

    option("--swift-compiler-version", set_,
           metavar="VERSION",
           help="string that indicates a compiler version for Swift")

    option("--clang-compiler-version", set_,
           metavar="VERSION",
           help="string that indicates a compiler version for Clang")

    option("--clang-user-visible-version", set_,
           default=defaults.CLANG_USER_VISIBLE_VERSION,
           metavar="VERSION",
           help="user-visible version of the embedded Clang and LLVM "
                "compilers")

    option("--swift-user-visible-version", set_,
           default=defaults.SWIFT_USER_VISIBLE_VERSION,
           metavar="VERSION",
           help="user-visible version of the Swift language")

    option("--build-llvm", enable(default=True),
           help="set to build LLVM and Clang (default: True)")

    option("--llvm-enable-lto", enable(default=False),
           help="enable LTO compilation of LLVM/Clang.")

    option("--lldb-extra-cmake-args", append(separator=" "),
           help="extra command line args to pass to lldb cmake")

    option("--lldb-extra-xcodebuild-args", set_,
           help="extra command line args to pass to lldb xcodebuild")

    option("--lldb-test-cc", set_,
           metavar="PATH",
           help="CC to use for building LLDB testsuite test inferiors. "
                "Defaults to just-built, in-tree clang. If set to "
                "'host-toolchain', sets it to same as host-cc.")

    option("--lldb-test-with-curses", enable,
           help="lldb-test-with-curses")

    option("--lldb-no-debugserver", enable,
           help="delete debugserver after building it, and don't try to "
                "codesign it")

    option("--lldb-use-system-debugserver", enable,
           help="don't try to codesign debugserver, and use the system's "
                "debugserver instead")

    option("--extra-cmake-options", append(separator=','),
           help="Pass through extra options to CMake in the form of comma "
                "separated options '-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp'. "
                "Can be called multiple times to add multiple such options.")

    option("--user-config-args", append(separator=" "),
           help="User-supplied arguments to cmake when used to do "
                "configuration")

    # -----------------------------------------------------------------------
    in_group("Build settings for Android")

    option("--android-ndk", set_,
           metavar="PATH",
           help="An absolute path to the NDK that will be used as a libc "
                "implementation for Android builds")

    option("--android-ndk-version", set_, default="21",
           help="A version of the NDK to use when building for Android. "
                "Currently only 21 or above is supported")

    option("--android-ndk-toolchain-version",
           set_(choices=["4.8", "4.9"]), default="4.8",
           help="A version of the toolchain to use when building for Android. "
                "Use 4.8 for 32-bit builds, 4.9 for 64-bit builds")

    option("--android-icu-uc", set_,
           metavar="PATH",
           help="Path to a directory containing libicuuc.so")

    option("--android-icu-uc-include", set_,
           metavar="PATH",
           help="Path to a directory containing headers for libicuuc")

    option("--android-icu-i18n", set_,
           metavar="PATH",
           help="Path to a directory containing libicui18n.so")

    option("--android-icu-i18n-include", set_,
           metavar="PATH",
           help="Path to a directory containing headers libicui18n")

    # -----------------------------------------------------------------------
    in_group("Run build")

    option("--skip-build-osx", enable,
           help="skip building Swift stdlibs for OS X")

    option("--skip-build-linux", enable,
           help="skip building Swift stdlibs for Linux")

    option("--skip-build-freebsd", enable,
           help="skip building Swift stdlibs for FreeBSD")

    option("--skip-build-cygwin", enable,
           help="skip building Swift stdlibs for Cygwin")

    option("--skip-build-benchmarks", enable, default=False,
           help="skip building Swift Benchmark Suite")

    # -----------------------------------------------------------------------
    in_group("Run tests")

    all_product_skip_tests = [
        "skip_test_cmark",
        "skip_test_swift",
        "skip_test_lldb",
        "skip_test_llbuild",
        "skip_test_swiftpm",
        "skip_test_xctest",
        "skip_test_foundation",
        "skip_test_libdispatch"]

    all_host_skip_tests = [
        "skip_test_linux",
        "skip_test_freebsd",
        "skip_test_cygwin",
        "skip_test_osx"]

    all_normal_skip_tests = all_product_skip_tests + all_host_skip_tests
    set_defaults(all_normal_skip_tests, True)
    set_defaults(skip_test_validation=True)
    set_defaults(skip_test_optimized=True)

    option(["-t", "--test"], disable(all_normal_skip_tests),
           help="test Swift after building")

    option(["-T", "--validation-test"],
           disable(all_normal_skip_tests + ['skip_test_validation']),
           help="run the validation test suite (implies --test)")

    option("--skip-test-validation", enable,
           help="set to skip validation test suite")

    option("--host-test", enable,
           help="run executable tests on host devices (such as iOS or tvOS)")

    with mutually_exclusive_group():
        option(["-B", "--benchmark"],
               disable("skip_test_benchmarks", default=True),
               help="run the Swift Benchmark Suite after building")
        option("--skip-test-benchmarks", enable,
               help="set to skip running Swift Benchmark Suite")

    option("--stress-test-sourcekit", enable, default=False,
           help="set to run the stress-SourceKit target")

    option("--skip-test-osx", enable,
           help="skip testing Swift stdlibs for OS X")
    option("--skip-test-linux", enable,
           help="skip testing Swift stdlibs for Linux")
    option("--skip-test-freebsd", enable,
           help="skip testing Swift stdlibs for FreeBSD")
    option("--skip-test-cygwin", enable,
           help="skip testing Swift stdlibs for Cygwin")

    with mutually_exclusive_group():
        option(["-o", "--test-optimized"],
               disable(all_normal_skip_tests + ['skip_test_optimized']),
               help="run the test suite in optimized mode too (implies "
                    "--test)")
        option("--skip-test-optimized", enable,
               help="set to skip testing the test suite in optimized mode")

    # ------------------------------------------------------------------------
    in_group("Skip testing specified target")

    option("--skip-test-ios",
           enable(["skip_test_ios_simulator", "skip_test_ios_host"]),
           help="skip testing all iOS targets. Equivalent to specifying both "
                "--skip-test-ios-simulator and --skip-test-ios-host")
    option("--skip-test-ios-simulator", enable,
           help="skip testing iOS simulator targets")
    option("--skip-test-ios-host", enable,
           help="skip testing iOS device targets on the host machine (the "
                "phone itself)")

    option("--skip-test-tvos",
           enable(["skip_test_tvos_simulator", "skip_test_tvos_host"]),
           help="skip testing all tvOS targets. Equivalent to specifying both "
                "--skip-test-tvos-simulator and --skip-test-tvos-host")
    option("--skip-test-tvos-simulator", enable,
           help="skip testing tvOS simulator targets")
    option("--skip-test-tvos-host", enable,
           help="skip testing tvOS device targets on the host machine (the TV "
                "itself)")

    option("--skip-test-watchos",
           enable(["skip_test_watchos_simulator", "skip_test_watchos_host"]),
           help="skip testing all tvOS targets. Equivalent to specifying both "
                "--skip-test-watchos-simulator and --skip-test-watchos-host")
    option("--skip-test-watchos-simulator", enable,
           help="skip testing watchOS simulator targets")
    option("--skip-test-watchos-host", enable,
           help="skip testing watchOS device targets on the host machine (the "
                "watch itself)")

    # ------------------------------------------------------------------------
    in_group("Skip testing specified products")

    option("--skip-test-cmark", enable,
           help="set to skip testing CommonMark")

    option("--skip-test-swift", enable,
           help="set to skip testing Swift")

    option("--skip-test-lldb", enable,
           help="set to skip testing lldb")

    option("--skip-test-llbuild", enable,
           help="set to skip testing llbuild")

    option("--skip-test-swiftpm", enable,
           help="set to skip testing swiftpm")

    option("--skip-test-xctest", enable,
           help="set to skip testing xctest")

    option("--skip-test-foundation", enable,
           help="set to skip testing foundation")

    option("--skip-test-libdispatch", enable,
           help="set to skip testing libdispatch")

    # ------------------------------------------------------------------------
    in_group("Installation related options")

    option("--install-destdir", set_, type=realpath,
           metavar="PATH",
           help="the path to use as the filesystem root for the installation")

    if host.is_darwin():
        set_defaults(install_prefix=defaults.DARWIN_INSTALL_PREFIX)
    else:
        set_defaults(install_prefix=defaults.UNIX_INSTALL_PREFIX)
    option("--install-prefix", set_,
           metavar="PATH",
           help="The installation prefix. This is where built Swift products "
                "(like bin, lib, and include) will be installed.")

    option("--install-symroot", set_,
           metavar="PATH",
           help="the path to install debug symbols into")

    option("--swift-install-components", append(separator=";"),
           metavar="COMPONENTS",
           help="a semicolon-separated list of Swift components to install",
           dest="swift_install_components")

    option("--llvm-install-components", append(separator=";"),
           metavar="COMPONENTS",
           help="a semicolon-separated list of LLVM components to install")

    option("--install-cmark", enable,
           help="install cmark")
    option("--install-swift", enable,
           help="install Swift")
    option("--install-lldb", enable,
           help="install LLDB")
    option("--install-llbuild", enable,
           help="install llbuild")
    option("--install-swiftpm", enable,
           help="install swiftpm")
    option("--install-xctest", enable,
           help="install xctest")
    option("--install-foundation", enable,
           help="install foundation")
    option("--install-libdispatch", enable,
           help="install libdispatch")

    option("--toolchain-prefix", set_,
           metavar="PATH",
           help="the path to the .xctoolchain directory that houses the "
                "install prefix path. (Default: auto determined from "
                "install-prefix)")

    option("--darwin-install-extract-symbols", enable,
           help="whether to extract symbols with dsymutil during "
                "installations")

    # ------------------------------------------------------------------------
    in_group("Packaging options")

    option("--installable-package", set_(type=realpath),
           metavar="PATH",
           help="the path to the archive of the installation directory")

    option("--test-installable-package", enable,
           help="whether to run post-packaging tests on the produced package")

    option("--symbols-package", set_(type=realpath),
           metavar="PATH",
           help="if provided, an archive of the symbols directory will be "
           "generated at this path")

    option("--skip-merge-lipo-cross-compile-tools", enable,
           help="set to skip running merge-lipo after installing "
                "cross-compiled host Swift tools")

    option("--darwin-toolchain-bundle-identifier", set_,
           help="CFBundleIdentifier for xctoolchain info plist")

    option("--darwin-toolchain-display-name", set_,
           help="Display Name for xctoolcain info plist")

    option("--darwin-toolchain-name", set_,
           help="Directory name for xctoolchain")

    option("--darwin-toolchain-version", set_,
           metavar="VERSION",
           help="Version for xctoolchain info plist and installer pkg")

    option("--darwin-toolchain-application-cert", set_,
           help="Application Cert name to codesign xctoolchain")

    option("--darwin-toolchain-installer-cert", set_,
           help="Installer Cert name to create installer pkg")

    option("--darwin-toolchain-installer-package", set_,
           help="The path to installer pkg")

    option("--darwin-toolchain-alias", set_,
           help="Swift alias for toolchain")

    return builder.build()


# ----------------------------------------------------------------------------
DESCRIPTION = """
Use this tool to build, test, and prepare binary distribution archives of Swift
and related tools.

Builds Swift (and, optionally, LLDB), incrementally, optionally
testing it thereafter.  Different build configurations are maintained in
parallel."""

EPILOG = """
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
details of the setups of other systems or automated environments."""
