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

'''Load preset options from presets.ini
'''

# ---------------------------------------------------------------------------

try:
    # Python 2
    import ConfigParser
except ImportError:
    # Python 3
    import configparser as ConfigParser

import sys

from .util import (
    print_with_argv0,
)


def _load_preset_files_impl(preset_file_names, substitutions={}):
    config = ConfigParser.SafeConfigParser(substitutions, allow_no_value=True)
    if config.read(preset_file_names) == []:
        print_with_argv0(
            "preset file not found (tried " + str(preset_file_names) + ")")
        sys.exit(1)
    return config


_PRESET_PREFIX = "preset: "


def _get_preset_options_impl(config, substitutions, preset_name):
    section_name = _PRESET_PREFIX + preset_name
    if section_name not in config.sections():
        return (None, None, None)

    build_script_opts = []
    build_script_impl_opts = []
    missing_opts = []
    dash_dash_seen = False

    for o in config.options(section_name):
        try:
            a = config.get(section_name, o)
        except ConfigParser.InterpolationMissingOptionError as e:
            # e.reference contains the correctly formatted option
            missing_opts.append(e.reference)
            continue

        if not a:
            a = ""

        if o in substitutions:
            continue

        opt = None
        if o == "mixin-preset":
            # Split on newlines and filter out empty lines.
            mixins = filter(None, [m.strip() for m in a.splitlines()])
            for mixin in mixins:
                (base_build_script_opts,
                    base_build_script_impl_opts,
                    base_missing_opts) = \
                    _get_preset_options_impl(config, substitutions, mixin)
                build_script_opts += base_build_script_opts
                build_script_impl_opts += base_build_script_impl_opts
                missing_opts += base_missing_opts
        elif o == "dash-dash":
            dash_dash_seen = True
        elif a == "":
            opt = "--" + o
        else:
            opt = "--" + o + "=" + a

        if opt:
            if not dash_dash_seen:
                build_script_opts.append(opt)
            else:
                build_script_impl_opts.append(opt)

    return (build_script_opts, build_script_impl_opts, missing_opts)


def get_preset_options(substitutions, preset_file_names, preset_name):
    config = _load_preset_files_impl(preset_file_names, substitutions)

    (build_script_opts, build_script_impl_opts, missing_opts) = \
        _get_preset_options_impl(config, substitutions, preset_name)
    if not build_script_opts:
        print_with_argv0("preset '" + preset_name + "' not found")
        sys.exit(1)
    if missing_opts:
        print_with_argv0("missing option(s) for preset '" + preset_name +
                         "': " + ", ".join(missing_opts))
        sys.exit(1)

    return build_script_opts + ["--"] + build_script_impl_opts


def get_all_preset_names(preset_file_names):
    config = _load_preset_files_impl(preset_file_names)
    return [name[len(_PRESET_PREFIX):] for name in config.sections()
            if name.startswith(_PRESET_PREFIX)]
