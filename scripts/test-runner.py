#! /usr/bin/python3

import argparse
import os
import shutil
import subprocess

from datetime import datetime


def git(args):
    result = subprocess.run(['git'] + args, check = True, capture_output = True, text = True)
    return result.stdout.splitlines()

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
        result = subprocess.run(['./mill', 'yuck.dev.test.run', testClass])
        exitCode = result.returncode
        os.sync()
    else:
        result = subprocess.run(
            ['./mill', 'yuck.dev.test.run', '--list-test-methods', 'true', testClass],
            capture_output = True, text = True)
        exitCode = result.returncode
        if exitCode == 0:
            testMethods = result.stdout.splitlines()
            if args.take:
                testMethods = testMethods[:args.take]
            for testMethod in testMethods:
                print(testMethod)
                result = subprocess.run(['./mill', 'yuck.dev.test.run', testMethod])
                if result.returncode != 0:
                    exitCode = result.returncode
                os.sync()

    if args.archive:
        commitDate = git(['log', '-1', '--pretty=format:%cd', '--date=format:%Y-%m-%d'])[0]
        commitHash = git(['rev-parse', '--short=8', 'HEAD'])[0]
        branch = git(['rev-parse', '--abbrev-ref', 'HEAD'])[0]
        now = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
        tag = 'run-{}-{}-{}-{}'.format(now, branch.replace('/', '-'), commitHash, args.testClasses[0])
        os.chdir('logs')
        os.mkdir(tag)
        for item in os.listdir('../tmp'):
            if item != '.gitkeep':
                shutil.move('../tmp/{}'.format(item), '{}/{}'.format(tag, item))
        subprocess.run(['tar', 'cjf', '{}.tar.bz2'.format(tag), tag], check = True)
        shutil.rmtree(tag)
        git(['tag', '-f', '-m', tag, tag])

    exit(exitCode)

main()
