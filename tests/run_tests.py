#!/usr/bin/env python3

import os
import subprocess
import sys
from typing import List

TESTS_DIR = os.path.join(os.path.dirname(__file__))


def gather_cases() -> List[str]:
    case_paths = []
    for filename in os.listdir(TESTS_DIR):
        if filename.endswith('.scm'):
            case_paths.append(os.path.join(TESTS_DIR, filename))
    return sorted(case_paths)


def run_test(args: List[str]) -> bool:
    proc = subprocess.Popen(
        args,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL)
    stdout, _ = proc.communicate(None)
    return stdout.strip() == b'#t'


def main(argv: List[str]) -> int:
    runner_args = argv[1:]
    if not runner_args:
        print('usage: run_tests.py args...')
        return 1

    case_paths = gather_cases()
    num_failure = 0
    for case_path in case_paths:
        success = run_test(runner_args + [case_path])
        print('%s: %s' %
              ('OK' if success else 'FAIL', os.path.basename(case_path)))
        if not success:
            num_failure += 1

    print('%d/%d passed' % (len(case_paths) - num_failure, len(case_paths)))
    return 0 if num_failure == 0 else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv))
