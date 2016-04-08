
import os
import sys
import pipes
import subprocess


def print_with_argv0(message):
    print(sys.argv[0] + ": " + message)


def quote_shell_command(args):
    return " ".join([pipes.quote(a) for a in args])


def check_call(args, print_command=False, verbose=False):
    if print_command:
        print(os.getcwd() + "$ " + quote_shell_command(args))
    try:
        return subprocess.check_call(args)
    except subprocess.CalledProcessError as e:
        print_with_argv0(
            "command terminated with a non-zero exit status " +
            str(e.returncode) + ", aborting")
        sys.stdout.flush()
        sys.exit(1)
    except OSError as e:
        print_with_argv0(
            "could not execute '" + quote_shell_command(args) +
            "': " + e.strerror)
        sys.stdout.flush()
        sys.exit(1)


def check_output(args, print_command=False, verbose=False):
    if print_command:
        print(os.getcwd() + "$ " + quote_shell_command(args))
    try:
        return subprocess.check_output(args)
    except subprocess.CalledProcessError as e:
        print_with_argv0(
            "command terminated with a non-zero exit status " +
            str(e.returncode) + ", aborting")
        sys.stdout.flush()
        sys.exit(1)
    except OSError as e:
        print_with_argv0(
            "could not execute '" + quote_shell_command(args) +
            "': " + e.strerror)
        sys.stdout.flush()
        sys.exit(1)
