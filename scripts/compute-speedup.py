#! /usr/bin/python3

# This script computes speedups for a given set of Yuck integration test runs.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import re
import sqlite3
import sys

import common

def computeSpeedups(cursor, args):
    runs = [args.referenceRun] + args.runs
    query = 'SELECT run, problem, model, instance, moves_per_second, runtime_in_seconds FROM result WHERE run IN (%s)' % ','.join('?' for run in runs)
    tasks = set()
    data = {}
    problemPattern = re.compile(args.problemFilter)
    modelPattern = re.compile(args.modelFilter)
    instancePattern = re.compile(args.instanceFilter)
    for (run, problem, model, instance, movesPerSecond, runtimeInSeconds) in cursor.execute(query, runs):
        if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance):
            task = (problem, model, instance)
            tasks.add(task)
            data[(run, task)] = {'mps': movesPerSecond, 'rts': runtimeInSeconds}
    for task in tasks:
        if not (args.referenceRun, task) in data:
            (problem, model, instance) = task
            print('Warning: No reference result found for instance {}/{}/{}'.format(problem, model, instance, file = sys.stderr))
    return {
        run: {
            task:
            newData['mps'] / refData['mps']
            for (task, newData, refData) in
            [(task, data[(run, task)], data[(args.referenceRun, task)])
             for task in tasks
             if (run, task) in data
             if (args.referenceRun, task) in data]
            if newData['mps']
            if refData['mps']
            if newData['rts'] and newData['rts'] >= args.minRuntimeInSeconds
            if refData['rts'] and refData['rts'] >= args.minRuntimeInSeconds
        }
        for run in args.runs
    }

def plotDiagrams(args, results):
    title = 'Speedups (wrt. {})'.format(args.referenceRun)
    filters = \
        ([args.problemFilter] if args.problemFilter else []) + \
        ([args.modelFilter] if args.modelFilter else []) + \
        ([args.instanceFilter] if args.instanceFilter else [])
    if filters:
        title += ' ({})'.format(', '.join(filters))
    common.plotDiagrams(
        [run for run in results],
        lambda run: (lambda result: [result[task] for task in result])(results[run]),
        title = title,
        xlabel = 'Speedup',
        legendLocation = 'center right')

def main():
    parser = argparse.ArgumentParser(
        description = 'Computes speedups for a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--problem-filter', dest = 'problemFilter', default = '', help = 'Consider only problems that match the given regexp')
    parser.add_argument('--model-filter', dest = 'modelFilter', default = '', help = 'Consider only models that match the given regexp')
    parser.add_argument('--instance-filter', dest = 'instanceFilter', default = '', help = 'Consider only instances that match the given regexp')
    parser.add_argument('--min-runtime', dest = 'minRuntimeInSeconds', type = int, default = 1, help = 'Ignore runs quicker than the lower bound given in seconds')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = computeSpeedups(cursor, args)
        if results:
            for run in results:
                if not results[run]:
                    print('Warning: No data for run {}'.format(run, file = sys.stderr))
            postprocessedResults = {run: common.analyzeResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(args, results)

main()
