
import os
import sys
import unittest

from swift_build_support import env
from swift_build_support import shell

def get_value(name, **env):
    env['PYTHONPATH'] = os.path.dirname(os.path.dirname(__file__))
    script = (
        'from swift_build_support.env import {name}; '
        'print({name})').format(name=name)
    cmd = [sys.executable, '-c', script]
    return shell.capture(cmd, env=env, echo=False).rstrip()

class EnvTestCase(unittest.TestCase):

    def test_default(self):
        self.assertNotEqual(get_value('SWIFT_SOURCE_ROOT'), "")
        self.assertNotEqual(get_value('SWIFT_BUILD_ROOT'), "")
        self.assertNotEqual(get_value('HOME'), "")

    def test_custom(self):
        self.assertEqual(
            get_value('SWIFT_SOURCE_ROOT', SWIFT_SOURCE_ROOT="myroot"),
            "myroot")
        self.assertEqual(
            get_value('SWIFT_BUILD_ROOT', SWIFT_SOURCE_ROOT="myroot"),
            os.path.join("myroot", "build"))
        self.assertEqual(
            get_value('SWIFT_BUILD_ROOT', SWIFT_BUILD_ROOT="mybuild"),
            "mybuild")
        self.assertEqual(
            get_value('HOME', HOME="myhome"),
            "myhome")
