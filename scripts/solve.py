#! /usr/bin/python3

# This script runs a given solver and collects basic performance data.
#
# Among other things, the runner needs the following information:
# - <solver>: the MiniZinc solver id (e.g. gecode or gecode@6.3.0)
# - <suite>: the location of the test suite
# - <problem>: the problem name
# - <model>: the model name (without ".mzn" suffix)
# - <instance>: the (optional) instance name (without ".dzn" suffix)
#
# The runner assumes that the model and the instances reside in the directory
# <suite>/<problem>.
#
# When no instance is given, the runner recursively collects all instances of
# the given problem and runs the solver on each instance.
#
# For each instance <instance>, the runner leaves logs and a summary in the
# directory $PWD/<suite>/<problem>/<model>/<instance> under the names
# <solver>.log and <solver>.json.

import argparse
import json
import os
import platform
import re
import resource
import subprocess
import sys
import time

from pathlib import Path


def currentTimeInMillis():
    return int(round(time.time() * 1000))

def checkExistence(path):
    if not path.exists():
        raise ValueError('{} does not exist'.format(path))

def identifyOs():
    return {'arch': platform.machine(), 'name': platform.system(), 'version': platform.release()}

def identifyMiniZinc():
    queryResult = subprocess.run(['minizinc', '--version'], check = True, capture_output = True, text = True)
    pattern = re.compile('.*, version ([\\.\\d]+), build (\\d+)')
    match = pattern.match(queryResult.stdout.splitlines()[0])
    return {'version': match.group(1), 'build': match.group(2)}

def identifySolver(args):
    queryResult = subprocess.run(['minizinc', '--solvers-json'], check = True, capture_output = True, text = True)
    data = json.loads(queryResult.stdout)
    pattern = re.compile('(.*)@(.*)')
    match = pattern.match(args.solver)
    if match:
        name = match.group(1).strip().lower()
        version = match.group(2).strip().lower()
        candidates = [item for item in data if item['name'].lower() == name and item['version'].lower() == version]
        if len(candidates) == 0:
            raise ValueError('Solver id {} is unknown'.format(args.solver))
        if len(candidates) > 1:
            raise ValueError('Solver id {} is ambigious'.format(args.solver))
    else:
        name = args.solver.strip().lower()
        candidates = [item for item in data if item['name'].lower() == name]
        if len(candidates) == 0:
            raise ValueError('Solver id {} is unknown'.format(args.solver))
        if len(candidates) > 1:
            raise ValueError('Solver id {} is ambigious'.format(args.solver))
        version = candidates[0]['version']
    return {'name': name, 'version': version}

def run(args):
    print('Running {} on {}'.format(args.solver, args.instance))
    suitePath = Path(args.suite)
    checkExistence(suitePath)
    suiteName = suitePath.stem
    if not suiteName:
        raise ValueError('No suite name given')
    modelPath = suitePath / args.problem / '{}.mzn'.format(args.model)
    checkExistence(modelPath)
    instancePath = suitePath / args.problem / '{}.dzn'.format(args.instance)
    checkExistence(instancePath)
    outputPath = Path.cwd() / suiteName / args.problem / args.model / args.instance
    logFilePath = outputPath / '{}.log'.format(args.solver)
    summaryFilePath = outputPath / '{}.json'.format(args.solver)
    outputPath.mkdir(parents = True, exist_ok = True)
    data = {
        'env': {
            'minizinc': identifyMiniZinc(),
            'os': identifyOs(),
            'java-opts': os.environ.get('JAVA_OPTS', ''),
            'resource-limits': {
                'as': resource.getrlimit(resource.RLIMIT_AS),
                'data': resource.getrlimit(resource.RLIMIT_DATA),
                'stack': resource.getrlimit(resource.RLIMIT_STACK),
                'rss': resource.getrlimit(resource.RLIMIT_RSS)
            }
        },
        'task': {
            'suite': suiteName,
            'problem': args.problem,
            'problem-type': args.problemType,
            'model': args.model,
            'instance': args.instance
        },
        'solver': identifySolver(args),
        'solver-configuration': {
            'runtime-limit-in-seconds': args.runtimeLimitInSeconds,
            'extra-args': args.extraSolverArgs
        }
    }
    objectivePattern = re.compile('_objective\s*=\s*(.*);')
    timeElapsedPattern = re.compile('% time elapsed:\s*(.*)\s*s')
    solverArgs = \
        ['--solver', args.solver, \
         '-a', '--output-mode', 'dzn', '--output-time', '--output-objective'] \
         + (['--time-limit', str(args.runtimeLimitInSeconds * 1000)] if args.runtimeLimitInSeconds else []) \
         + [arg for dir in args.includeDirs for arg in ['-I', dir]] \
         + (['-f'] if args.freeSearch else []) \
         + (['-p', str(args.numberOfThreads)] if args.numberOfThreads else []) \
         + [arg for dataAssignment in args.dataAssignments for arg in ['-D', dataAssignment]] \
         + [arg for extraArg in args.extraSolverArgs for arg in ['--fzn-flag', extraArg]] \
         + [str(modelPath), str(instancePath)]
    startTimeInMillis = currentTimeInMillis()
    solverResult = subprocess.run(['minizinc'] + solverArgs, capture_output = True, text = True)
    runtimeInMillis = currentTimeInMillis() - startTimeInMillis
    with open(logFilePath, 'w') as logFile:
        logFile.write(solverResult.stdout)
        logFile.write(solverResult.stderr)
    if solverResult.returncode == 0:
        solved = False
        complete = False
        quality = None
        elapsedTimeInMs = None
        qualityStepFunction = []
        for line in solverResult.stdout.splitlines():
            match = objectivePattern.match(line)
            if match:
                quality = int(match.group(1))
            else:
                match = timeElapsedPattern.match(line)
                if match:
                    elapsedTimeInMs = int(float(match.group(1)) * 1000)
                elif line == '----------':
                    solved = True
                    if quality:
                        qualityStepFunction += [elapsedTimeInMs, quality]
                elif line == '==========':
                    complete = True
        data['result'] = {
            'solved': solved,
            'complete': complete
        }
        if quality:
            if complete:
                data['task']['optimum'] = quality
            data['result']['quality'] = quality
            data['result']['optimal'] = quality and complete
            data['result']['quality-step-function'] = qualityStepFunction
        data['solver-statistics'] = {
            'runtime-in-seconds': runtimeInMillis / 1000
        }
    else:
        data['result'] = {
            'error': {
                'return-code': solverResult.returncode,
                'stderr': solverResult.stderr.strip()
            }
        }
    with open(summaryFilePath, 'w') as summaryFile:
        json.dump(data, summaryFile, sort_keys = True, indent = 2)

def main():
    parser = argparse.ArgumentParser(
        description = 'Runs a given solver and collects basic performance data',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--suite', dest = 'suite', required = True, help = 'Suite path')
    parser.add_argument('--problem', dest = 'problem', required = True, help = 'Problem name')
    parser.add_argument('--problem-type', dest = 'problemType', choices = ['SAT', 'MIN', 'MAX'], required = True, help = 'Problem type')
    parser.add_argument('--model', dest = 'model', required = True, help = 'Model name (without \".mzn" suffix)')
    parser.add_argument('--instance', dest = 'instance', help = 'Instance name (without \".dzn" suffix)')
    parser.add_argument('-D', '--cmdline-data', dest = 'dataAssignments', action = 'append', help = 'Additional data assignments')
    parser.add_argument('-I', '--search-dir', dest = 'includeDirs', action = 'append', help = 'Additional include directory')
    parser.add_argument('-f', '--free-search', dest = 'freeSearch', action = 'store_true', default = False, help = 'Allows the solver to ignore the given search strategy')
    parser.add_argument('-p', '--parallel', dest = 'numberOfThreads', type = int, default = 1, help = 'Number of threads to use during search')
    parser.add_argument('-v', '--verbose', action = 'store_true')
    parser.add_argument('--solver', dest = 'solver', default = 'gecode', help = 'Solver to run')
    parser.add_argument('--solver-arg', '--fzn-flag', dest = 'extraSolverArgs', action = 'append', help = 'Extra argument to pass to the solver')
    parser.add_argument('--runtime-limit', dest = 'runtimeLimitInSeconds', type = int, help = 'Runtime limit in seconds')
    args = parser.parse_args()
    if not args.dataAssignments:
        args.dataAssignments = []
    if not args.extraSolverArgs:
        args.extraSolverArgs = []
    if not args.includeDirs:
        args.includeDirs = []
    problemPath = Path('{}/{}/'.format(args.suite, args.problem))
    try:
        checkExistence(problemPath)
        if args.instance:
            run(args)
        else:
            instances = [str(path).replace(str(problemPath) + '/', '').replace('.dzn', '')
                         for path in problemPath.glob('**/*.dzn')]
            if not instances:
                raise ValueError('No instances found in {}'.format(problemPath))
            for instance in sorted(instances):
                args.instance = instance
                run(args)
    except ValueError as e:
        print(e, file = sys.stderr)

main()
