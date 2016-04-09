# build_script/products/swift.py --------------------------------*- python -*-
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
Swift builder
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os

from ..host import host
from ..cmake import CMakeOptions
from .. import targets
from ..exceptions import BuildError
from ..utils import printf


class Swift(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swift')
        if cls.source_dir is None:
            raise BuildError("Couldn't find Swift source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 cmake,
                 cmark_build,
                 llvm_build,
                 host_llvm_build,
                 host_swift_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir
        self.cmake = cmake
        self.cmark_build = cmark_build
        self.llvm_build = llvm_build
        self.host_llvm_build = host_llvm_build
        self.host_swift_build = host_swift_build
        self.args = args

    # Public property accessors

    @property
    def bin_dir(self):
        if self.args.cmake_generator == 'Xcode':
            return os.path.join(self.build_dir,
                                'bin', self.swift_build_type)
        else:
            return os.path.join(self.build_dir, 'bin')

    @property
    def swiftc_bin_path(self):
        return os.path.join(self.bin_dir, 'swiftc')

    @property
    def swift_bin_path(self):
        return os.path.join(self.bin_dir, 'swift')

    @property
    def swift_stdlib_tool_source_path(self):
        return os.path.join(self.source_dir,
                            'utils', 'swift-stdlib-tool-substitute')

    @property
    def swiftlib_path(self):
        return os.path.join(self.build_dir, 'lib', 'swift')

    # Private utilities

    def stdlib_deployment_targets(self):
        '''
        All possible deployment targets where this host can cross compile.
        '''
        host_target = host.deployment_target()
        sys, arch = targets.split(host_target)

        if sys == 'linux':
            return (host_target, "android-armv7", )
        if sys == 'freebsd':
            return (host_target, )
        if sys == 'cygwin':
            return (host_target, )

        if sys == 'macosx':
            return (
                host_target,
                "iphonesimulator-i386",
                "iphonesimulator-x86_64",
                "appletvsimulator-x86_64",
                "watchsimulator-i386",

                # Put iOS native targets last so that we test them last
                # (it takes a long time).
                "iphoneos-arm64",
                "iphoneos-armv7",
                "appletvos-arm64",
                "watchos-armv7k")

        raise BuildError("Unknown operating system")

    def _stdlib_build_targets(self):
        stdlib_targets = []
        benchmark_targets = []
        run_benchmark_targets = []
        test_targets = []

        stdlib_deployment_targets = self.args.stdlib_deployment_targets
        if stdlib_deployment_targets is None:
            stdlib_deployment_targets = self.stdlib_deployment_targets()

        for deployment_tgt in stdlib_deployment_targets:
            build_for_this_target = True
            test_this_target = True
            test_host_only = False
            build_benchmark_this_target = False

            sys, arch = targets.split(deployment_tgt)
            if sys == 'linux':
                build_for_this_target = not self.args.skip_build_linux
                test_this_target = not self.args.skip_test_linux
            elif sys == 'freebsd':
                build_for_this_target = not self.args.skip_build_freebsd
                test_this_target = not self.args.skip_test_freebsd
            elif sys == 'cygwin':
                build_for_this_target = not self.args.skip_build_cygwin
                test_this_target = not self.args.skip_test_cygwin
            elif sys == 'macosx':
                build_for_this_target = not self.args.skip_build_osx
                test_this_target = not self.args.skip_test_osx
                build_benchmark_this_target = build_for_this_target
            elif sys == 'iphoneos':
                build_for_this_target = not self.args.skip_build_ios_device
                if not self.args.skip_test_ios_host:
                    test_host_only = True
                else:
                    test_this_target = False
                build_benchmark_this_target = build_for_this_target
            elif sys == 'iphonesimulator':
                build_for_this_target = not self.args.skip_build_ios_simulator
                test_this_target = not self.args.skip_test_ios_simulator
            elif sys == 'appletvos':
                build_for_this_target = not self.args.skip_build_tvos_device
                if not self.args.skip_test_tvos_host:
                    test_host_only = True
                else:
                    test_this_target = False
                build_benchmark_this_target = build_for_this_target
            elif sys == 'appletvsimulator':
                build_for_this_target = not self.args.skip_build_tvos_simulator
                test_this_target = not self.args.skip_test_tvos_simulator
            elif sys == 'watchos':
                build_for_this_target = not self.args.skip_build_watchos_device
                if not self.args.skip_test_watchos_host:
                    test_host_only = True
                else:
                    test_this_target = False
                build_benchmark_this_target = build_for_this_target
            elif sys == 'watchsimulator':
                build_for_this_target = (
                    not self.args.skip_build_watchos_simulator)
                test_this_target = (
                    not self.args.skip_test_watchos_simulator)
            elif sys == 'android':
                build_for_this_target = not self.args.skip_build_android
                # FIXME: Allow Android host tests to be enabled/disabled by the
                #        build script.
                test_this_target = False
            else:
                raise BuildError("Unknown compiler deployment target: " %
                                 deployment_tgt)

            # Build
            if build_for_this_target:
                if self.args.build_swift_stdlib_unittest_extra:
                    stdlib_targets += [
                        "swift-stdlib-" + deployment_tgt]
                elif self.args.skip_test_validation:
                    stdlib_targets += [
                        "swift-test-stdlib-" + deployment_tgt]
                else:
                    stdlib_targets += [
                        "swift-stdlib-" + deployment_tgt]

            # Benchmark
            if build_benchmark_this_target:
                benchmark_targets += ["swift-benchmark-" + deployment_tgt]
                if not self.args.skip_test_benchmarks:
                    run_benchmark_targets += [
                        "check-swift-benchmark-" + deployment_tgt]

            # Test
            if test_this_target:
                test_target_suffix = ""
                if test_host_only:
                    test_target_suffix = "non-executable-"

                def should_optimzed_test():
                    return (not self.args.skip_test_optimized and
                            not test_host_only)

                if self.args.skip_test_validation:
                    test_targets += [
                        "check-swift-" + test_target_suffix + deployment_tgt]
                    if should_optimzed_test():
                        test_targets += [
                            "check-swift-optimize-" + deployment_tgt]
                else:
                    test_targets += [
                        "check-swift-all-" +
                        test_target_suffix + deployment_tgt]
                    if should_optimzed_test():
                        test_targets += [
                            "check-swift-all-optimize-" + deployment_tgt]

        return (stdlib_targets,
                benchmark_targets,
                test_targets,
                run_benchmark_targets)

    def _host_variants(self, target):
        sys, arch = targets.split(target)
        if sys == 'iphonesimulator':
            return (sys, 'IOS_SIMULATOR', arch)
        if sys == 'iphoneos':
            return (sys, 'IOS', arch)
        if sys == 'appletvsimulator':
            return (sys, 'TVOS_SIMULATOR', arch)
        if sys == 'appletvos':
            return (sys, 'TVOS', arch)
        if sys == 'watchsimulator':
            return (sys, 'WATCHOS_SIMULATOR', arch)
        if sys == 'watchos':
            return (sys, 'WATCHOS', arch)
        return None

    def _c_flags(self):
        cflags = []

        # Don't pass common_cross_c_flags to Swift because CMake code in the
        # Swift project is itself aware of cross-compilation for the host
        # tools and standard library.
        if self.cmake.is_release_build_type(self.args.llvm_build_type):
            cflags += ['-fno-stack-protector', ]
        return cflags

    def configure(self):
        if (not self.args.reconfigure and
                self.cmake.is_configured(self.build_dir)):
            return

        printf("--- Configuring Swift ---")
        # Configure cmake options
        cmake_options = CMakeOptions()
        define = cmake_options.define

        if self.args.use_gold_linker:
            define('SWIFT_ENABLE_GOLD_LINKER:BOOL', True)

        if not self.args.skip_build_android:
            define('SWIFT_ANDROID_NDK_PATH:STRING',
                   self.args.android_ndk)
            define('SWIFT_ANDROID_NDK_TOOLCHAIN_VERSION:STRING',
                   self.args.android_ndk_toolchain_version)
            define('SWIFT_ANDROID_SDK_PATH',
                   os.path.join(self.args.android_ndk,
                                'platforms',
                                'android-%s' % self.args.android_ndk_version,
                                'arch-arm'))
            define('SWIFT_ANDROID_ICU_UC:STRING',
                   self.args.android_icu_uc)
            define('SWIFT_ANDROID_ICU_UC_INCLUDE:STRING',
                   self.args.android_icu_uc_include)
            define('SWIFT_ANDROID_ICU_I18N:STRING',
                   self.args.android_icu_i18n)
            define('SWIFT_ANDROID_ICU_I18N_INCLUDE:STRING',
                   self.args.android_icu_i18n_include)

        native_llvm_tools_path = None
        native_clang_tools_path = None
        native_swift_tools_path = None
        if self.deployment_target != self.args.host_target:
            # Cross tools target
            build_benchmark_this_time = False
            build_tests_this_time = False

            native_llvm_tools_path = self.host_llvm_build.bin_dir
            native_clang_tools_path = self.host_llvm_build.bin_dir
            native_swift_tools_path = self.host_swift_build.bin_dir

            define('LLVM_TOOLS_BINARY_DIR:PATH', self.llvm_build.bin_dir)
            define('LLVM_LIBRARY_DIR:PATH', self.llvm_build.lib_dir)
            define('LLVM_MAIN_INCLUDE_DIR:PATH', self.llvm_build.include_dir)
            define('LLVM_BINARY_DIR:PATH', self.llvm_build.build_dir)
            define('LLVM_MAIN_SRC_DIR:PATH', self.llvm_build.source_dir)
        else:
            # Host target
            build_benchmark_this_time = not self.args.skip_build_benchmarks
            build_tests_this_time = self.args.source_tree_includes_tests

        # Command-line parameters override any autodetection that we
        # might have done.
        if self.args.native_llvm_tools_path is not None:
            native_llvm_tools_path = self.args.native_llvm_tools_path
        if self.args.native_clang_tools_path is not None:
            native_clang_tools_path = self.args.native_clang_tools_path
        if self.args.native_swift_tools_path is not None:
            native_swift_tools_path = self.args.native_swift_tools_path

        if not self.args.build_llvm:
            define("LLVM_TOOLS_BINARY_DIR:PATH", "/tmp/dummy")
            define("LLVM_LIBRARY_DIR:PATH", self.build_dir)
            define("LLVM_MAIN_INCLUDE_DIR:PATH", "/tmp/dummy")
            define("LLVM_BINARY_DIR:PATH", self.llvm_build.build_dir)
            define("LLVM_MAIN_SRC_DIR:PATH", self.llvm_build.source_dir)

        define("CMAKE_C_FLAGS", " ".join(self._c_flags()))
        define("CMAKE_CXX_FLAGS", " ".join(self._c_flags()))
        define("CMAKE_BUILD_TYPE:STRING", self.args.llvm_build_type)
        define("LLVM_ENABLE_ASSERTIONS:BOOL",
               self.args.swift_enable_assertions)
        define("SWIFT_ANALYZE_CODE_COVERAGE:STRING",
               self.args.swift_analyze_code_coverage.upper())
        define("SWIFT_STDLIB_BUILD_TYPE:STRING",
               self.args.swift_stdlib_build_type)
        define("SWIFT_STDLIB_ASSERTIONS:BOOL",
               self.args.swift_stdlib_enable_assertions)
        define("SWIFT_STDLIB_ENABLE_REFLECTION_METADATA:BOOL",
               self.args.swift_stdlib_enable_reflection_metadata)
        define("SWIFT_STDLIB_ENABLE_RESILIENCE:BOOL",
               self.args.swift_stdlib_enable_resilience)
        define("SWIFT_STDLIB_SIL_SERIALIZE_ALL:BOOL",
               self.args.swift_stdlib_sil_serialize_all)
        define("SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING",
               native_llvm_tools_path)
        define("SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING",
               native_clang_tools_path)
        define("SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING",
               native_swift_tools_path)
        define("SWIFT_BUILD_TOOLS:BOOL",
               self.args.build_swift_tools)
        define("SWIFT_BUILD_STDLIB:BOOL",
               self.args.build_swift_stdlib)
        define("SWIFT_SERIALIZE_STDLIB_UNITTEST:BOOL",
               self.args.build_serialized_stdlib_unittest)
        define("SWIFT_STDLIB_SIL_DEBUGGING:BOOL",
               self.args.build_sil_debugging_stdlib)
        define("SWIFT_BUILD_SDK_OVERLAY:BOOL",
               self.args.build_swift_sdk_overlay)
        define("SWIFT_BUILD_STATIC_STDLIB:BOOL",
               self.args.build_swift_static_stdlib)
        define("SWIFT_BUILD_PERF_TESTSUITE:BOOL",
               build_benchmark_this_time)
        define("SWIFT_BUILD_EXAMPLES:BOOL",
               self.args.build_swift_examples)
        define("SWIFT_INCLUDE_TESTS:BOOL",
               build_tests_this_time)
        define("SWIFT_INSTALL_COMPONENTS:STRING",
               ";".join(self.args.swift_install_components))
        define("SWIFT_EMBED_BITCODE_SECTION:BOOL",
               self.args.embed_bitcode_section)
        define("SWIFT_ENABLE_LTO:BOOL",
               self.args.swift_enable_lto)
        define("SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL",
               self.args.build_runtime_with_host_compiler)

        # deployment target based options
        host_variant_opts = self._host_variants(self.deployment_target)
        if host_variant_opts is not None:
            (variant, variant_sdk, variant_arch) = host_variant_opts
            define('SWIFT_HOST_VARIANT', variant)
            define('SWIFT_HOST_VARIANT_SDK', variant_sdk)
            define('SWIFT_HOST_VARIANT_ARCH', variant_arch)

        if targets.is_darwin_type(self.deployment_target):
            if self.args.swift_enable_lto:
                if self.cmake.needs_to_specify_standard_computed_defaults:
                    define("CMAKE_C_STANDARD_COMPUTED_DEFAULT", "AppleClang")
                    define("CMAKE_CXX_STANDARD_COMPUTED_DEFAULT", "AppleClang")

                    # Currently with -gline-tables-only swift is ~5-6GB on
                    # Darwin. Use the formula GB Memory/6GB to get the number
                    # of parallel link threads we can support.
                    define("SWIFT_PARALLEL_LINK_JOBS",
                           self.cmake.num_parallel_lto_link_jobs(6.0))

            define("SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX",
                   self.args.darwin_deployment_version_osx)
            define("SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS",
                   self.args.darwin_deployment_version_ios)
            define("SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS",
                   self.args.darwin_deployment_version_tvos)
            define("SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS",
                   self.args.darwin_deployment_version_watchos)
            define("LLVM_ENABLE_LIBCXX:BOOL", True)

        if self.args.compiler_vendor == "none":
            pass
        elif self.args.compiler_vendor == "apple":
            define("SWIFT_VENDOR", "Apple")
            define("SWIFT_VENDOR_UTI", "com.apple.compilers.llvm.swift")
            define("SWIFT_VERSION", self.args.swift_user_visible_version)
            define("SWIFT_COMPILER_VERSION", self.args.swift_compiler_version)
        else:
            raise BuildError("unknown compiler vendor")

        clang_compiler_version = self.args.clang_compiler_version
        swift_compiler_version = self.args.swift_compiler_version
        if swift_compiler_version is None:
            swift_compiler_version = clang_compiler_version
        if clang_compiler_version is not None:
            define("CLANG_COMPILER_VERSION", clang_compiler_version)
            define("SWIFT_COMPILER_VERSION", swift_compiler_version)

        if self.args.darwin_toolchain_version is not None:
            define("DARWIN_TOOLCHAIN_VERSION",
                   self.args.darwin_toolchain_version),

        if self.args.enable_asan is not None:
            define("SWIFT_SOURCEKIT_USE_INPROC_LIBRARY:BOOL", True)

        if self.args.darwin_crash_reporter_client:
            define("SWIFT_RUNTIME_CRASH_REPORTER_CLIENT:BOOL", True)

        define("SWIFT_DARWIN_XCRUN_TOOLCHAIN:STRING",
               self.args.darwin_xcrun_toolchain)

        if self.args.darwin_stdlib_install_name_dir:
            define("SWIFT_DARWIN_STDLIB_INSTALL_NAME_DIR:STRING",
                   self.args.darwin_stdlib_install_name_dir)

        if len(self.args.extra_swift_args):
            define('SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS',
                   self.args.extra_swift_args)

        define("SWIFT_AST_VERIFIER:BOOL", self.args.swift_enable_ast_verifier)
        define("SWIFT_SIL_VERIFY_ALL:BOOL", self.args.sil_verify_all)
        define("SWIFT_RUNTIME_ENABLE_LEAK_CHECKER:BOOL",
               self.args.swift_runtime_enable_leak_checker)

        define("CMAKE_INSTALL_PREFIX:PATH", self.args.install_prefix)
        define("LLVM_CONFIG:PATH", self.llvm_build.llvm_config_bin_path)
        define("SWIFT_PATH_TO_CLANG_SOURCE:PATH",
               self.llvm_build.clang_source_dir)
        define("SWIFT_PATH_TO_CLANG_BUILD:PATH", self.llvm_build.build_dir)
        define("SWIFT_PATH_TO_LLVM_SOURCE:PATH", self.llvm_build.source_dir)
        define("SWIFT_PATH_TO_LLVM_BUILD:PATH", self.llvm_build.build_dir)
        define("SWIFT_PATH_TO_CMARK_SOURCE:PATH", self.cmark_build.source_dir)
        define("SWIFT_PATH_TO_CMARK_BUILD:PATH", self.cmark_build.build_dir)
        define('SWIFT_CMARK_LIBRARY_DIR:PATH', self.cmark_build.library_dir)

        if self.args.swift_sdks:
            define("SWIFT_SDKS:STRING", self.args.swift_sdks)

        if self.args.swift_primary_variant_sdk:
            define("SWIFT_PRIMARY_VARIANT_SDK:STRING",
                   self.args.swift_primary_variant_sdk)
            define("SWIFT_PRIMARY_VARIANT_ARCH:STRING",
                   self.args.swift_primary_variant_arch)

        if build_benchmark_this_time:
            define("SWIFT_EXEC:STRING", self.swiftc_bin_path)

        self.cmake.configure(
            source_dir=self.source_dir,
            build_dir=self.build_dir,
            options=cmake_options)

    def _cmake_config_opts(self):
        config_opts = []
        if self.args.cmake_generator == 'Xcode':
            config_opts += [
                '--config', self.args.swift_build_type]
        return config_opts

    def build(self):
        if self.args.skip_build_swift:
            return

        printf("--- Building Swift ---")

        (stdlib_build_targets,
         benchmark_build_targets,
         _, _) = self._stdlib_build_targets()

        build_targets = ["all", ]
        build_targets += stdlib_build_targets

        build_benchmark_this_time = (
            self.deployment_target == self.args.host_target and
            not self.args.skip_build_benchmarks)
        if build_benchmark_this_time:
            build_targets += benchmark_build_targets

        self.cmake.build(
            build_targets=build_targets,
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

    def test(self):
        if self.deployment_target != self.args.host_target:
            return

        (_, _,
         test_targets,
         run_benchmark_targets) = self._stdlib_build_targets()

        executable_targets = []
        results_targets = []

        if not self.args.skip_test_swift:
            executable_targets += ['SwiftUnitTests', ]
            results_targets += test_targets
            if self.args.stress_test_sourcekit:
                results_targets += ['stress-SourceKit', ]
        if not self.args.skip_test_benchmarks:
            results_targets += run_benchmark_targets

        if not len(results_targets):
            # Nothing to test
            return

        printf("--- Building tests for Swift ---")
        self.cmake.build(
            build_targets=executable_targets,
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

        printf("--- Running tests for Swift ---")
        self.cmake.test(
            test_targets=results_targets,
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

    def install(self):
        if not self.args.install_swift:
            return

        printf("--- Installing Swift ---")
        self.cmake.install(
            dest_dir=self.install_destdir,
            build_dir=self.build_dir)
