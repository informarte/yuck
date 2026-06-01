#! /usr/bin/python3

import argparse
import os
import shutil
import subprocess
import sys

from datetime import datetime

def git(args):
    result = subprocess.run(['git'] + args, check = True, capture_output = True, text = True)
    return result.stdout.splitlines()

NOISE_PREFIXES = [
    # The Serviceability Agent (jdk.sa) was removed in Java 22.
    # JOL, however, is still looking for it, although it works fine without it.
    '# WARNING: Unable to attach Serviceability Agent',
]

def is_noise(line):
    return any(line.startswith(prefix) for prefix in NOISE_PREFIXES)

def mill(args, capture_stdout = False):
    if capture_stdout:
        result = subprocess.run(
            ['./mill', '--disable-ticker'] + args,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE,
            text = True)
        for line in result.stderr.splitlines():
            if not is_noise(line):
                sys.stderr.write(line + '\n')
        return result
    else:
        with subprocess.Popen(
            ['./mill', '--disable-ticker'] + args,
            stdout = subprocess.PIPE,
            stderr = subprocess.STDOUT,
            text = True,
            bufsize = 1) as proc:
            buf = ''
            for char in iter(lambda: proc.stdout.read(1), ''):
                if char == '\n':
                    if not is_noise(buf):
                        sys.stdout.write(buf + '\n')
                        sys.stdout.flush()
                    buf = ''
                else:
                    buf += char
                    if not any(p.startswith(buf) or buf.startswith(p) for p in NOISE_PREFIXES):
                        sys.stdout.write(buf)
                        sys.stdout.flush()
                        buf = ''
            if buf and not is_noise(buf):
                sys.stdout.write(buf)
                sys.stdout.flush()
        return proc

def main():

    parser = argparse.ArgumentParser(
        description = 'Runs the test methods from the given test class',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--mode', dest = 'mode', choices = ['SINGLE_JVM', 'ONE_JVM_PER_TEST_METHOD'], default = 'SINGLE_JVM')
    parser.add_argument('--take', dest = 'take', type = int, help = 'Run only the given number of test methods')
    parser.add_argument('--archive', dest = 'archive', action = 'store_true', default = False, help = 'Archive the artifacts')
    parser.add_argument('testClasses', metavar = 'test-class', nargs = 1)
    args = parser.parse_args()

    testClass = args.testClasses[0]
    exitCode = 0
    if args.mode == 'SINGLE_JVM':
        result = mill(['yuck.test.run', testClass])
        exitCode = result.returncode
        os.sync()
    else:
        result = mill(['yuck.test.run', '--list-test-methods', 'true', testClass], capture_stdout = True)
        exitCode = result.returncode
        if exitCode == 0:
            testMethods = result.stdout.splitlines()
            if args.take:
                testMethods = testMethods[:args.take]
            for testMethod in testMethods:
                print(testMethod)
                result = mill(['yuck.test.run', testMethod])
                if result.returncode != 0:
                    exitCode = result.returncode
                os.sync()

    if args.archive:
        commitDate = git(['log', '-1', '--pretty=format:%cd', '--date=format:%Y-%m-%d'])[0]
        commitHash = git(['rev-parse', '--short=8', 'HEAD'])[0]
        branch = git(['rev-parse', '--abbrev-ref', 'HEAD'])[0].replace('/', '-')
        now = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
        tag = f'run-{now}-{branch}-{commitHash}-{args.testClasses[0]}'
        os.chdir('logs')
        os.mkdir(tag)
        for item in os.listdir('../tmp'):
            if item != '.gitkeep':
                shutil.move(f'../tmp/{item}', f'{tag}/{item}')
        subprocess.run(['tar', 'cjf', f'{tag}.tar.bz2', tag], check = True)
        shutil.rmtree(tag)
        git(['tag', '-f', '-m', tag, tag])

    exit(exitCode)

main()
