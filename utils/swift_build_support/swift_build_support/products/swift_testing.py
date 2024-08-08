# swift_build_support/products/swift_testing.py -----------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import cmake_product
from . import product
from . import swift


class SwiftTesting(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def product_source_name(cls):
        return "swift-testing"

    @classmethod
    def get_dependencies(cls):
        return [swift.Swift]

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing_macros

    def _impl_product(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, '%s-%s' % (self.product_name(), host_target))

        return SwiftTestingImpl(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=build_dir)

    def _build_impl(self, host_target):
        self._impl_product(host_target).build(host_target)

    def build(self, host_target):
        self._build_impl(host_target)

        # For Darwin host, 'build' is only called for the builder.
        # Manually iterate tor the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(host_target):
            for target in self.args.cross_compile_hosts:
                self._build_impl(target)

        # FIXME: build testing library for 'stdlib_deployment_targets'?
        pass

    def _install_impl(self, host_target):
        self._impl_product(host_target).install(host_target)

    def install(self, host_target):
        self._install_impl(host_target)

        # For Darwin host, 'install' is only called for the builder.
        # Manually iterate tor the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(host_target):
            for target in self.args.cross_compile_hosts:
                self._install_impl(target)


class SwiftTestingImpl(cmake_product.CMakeProduct):
    def build(self, host_target):
        override_version = None
        if host_target.startswith('macosx'):
            override_version = '10.15'

        self.cmake_options.define('BUILD_SHARED_LIBS', 'YES')

        # Use empty CMake install prefix, since the `DESTDIR` env var is set by
        # `install_with_cmake` later which already has the same prefix.
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '')

        self.cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        # FIXME: If we build macros for the builder, specify the path.
        self.cmake_options.define('SwiftTesting_MACRO', 'NO')

        self.generate_toolchain_file_for_darwin_or_linux(
            host_target, override_deployment_version=override_version)
        self.build_with_cmake([], self.args.build_variant, [],
                              prefer_native_toolchain=True)

    def install(self, host_target):
        install_destdir = self.host_install_destdir(host_target)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)


class SwiftTestingMacros(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def product_source_name(cls):
        return "swift-testing/Sources/TestingMacros"

    @classmethod
    def get_dependencies(cls):
        return [swift.Swift]

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing_macros

    def _impl_product(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, '%s-%s' % (self.product_name(), host_target))

        return SwiftTestingMacrosImpl(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=build_dir)

    def _build_impl(self, host_target):
        self._impl_product(host_target).build(host_target)

    def build(self, host_target):
        self._build_impl(host_target)

        # For Darwin host, 'build' is only called for the builder.
        # Manually iterate tor the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(host_target):
            for target in self.args.cross_compile_hosts:
                self._build_impl(target)

    def _install_impl(self, host_target):
        self._impl_product(host_target).install(host_target)

    def install(self, host_target):
        self._install_impl(host_target)

        # For Darwin host, 'install' is only called for the builder.
        # Manually iterate tor the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(host_target):
            for target in self.args.cross_compile_hosts:
                self._install_impl(target)


class SwiftTestingMacrosImpl(cmake_product.CMakeProduct):
    def build(self, host_target):
        override_version = None
        if host_target.startswith('macosx'):
            override_version = '10.15'

        # Use empty CMake install prefix, since the `DESTDIR` env var is set by
        # `install_with_cmake` later which already has the same prefix.
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '')

        self.cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        build_root = os.path.dirname(self.build_dir)
        swift_build_dir = os.path.join(
            '..', build_root, '%s-%s' % ('swift', host_target))
        swift_cmake_dir = os.path.join(swift_build_dir, 'cmake', 'modules')
        self.cmake_options.define('SwiftSyntax_DIR:PATH', swift_cmake_dir)

        self.generate_toolchain_file_for_darwin_or_linux(
            host_target, override_deployment_version=override_version)
        self.build_with_cmake([], self.args.build_variant, [],
                              prefer_native_toolchain=True)

    def install(self, host_target):
        install_destdir = self.host_install_destdir(host_target)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)
