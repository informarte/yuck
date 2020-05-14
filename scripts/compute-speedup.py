#! /usr/bin/python3

# This script computes speedups for a given set of Yuck integration test runs.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import sqlite3
import sys

import common

def computeSpeedups(cursor, args):
    runs = [args.referenceRun] + args.runs
    query = 'SELECT run, problem, model, instance, moves_per_second, runtime_in_seconds FROM result WHERE run IN (%s)' % ','.join('?' for run in runs)
    tasks = set()
    data = {}
    for (run, problem, model, instance, movesPerSecond, runtimeInSeconds) in cursor.execute(query, runs):
        task = (problem, model, instance)
        tasks.add(task)
        data[(run, task)] = {'mps': movesPerSecond, 'rts': runtimeInSeconds}
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
            if newData['rts'] and newData['rts'] >= args.minRuntime
            if refData['rts'] and refData['rts'] >= args.minRuntime
        }
        for run in args.runs
    }

def plotDiagrams(args, results):
    common.plotDiagrams(
        [run for run in results],
        lambda run: (lambda result: [result[task] for task in result])(results[run]),
        title = 'Speedups (wrt. {})'.format(args.referenceRun),
        xlabel = 'Speedup',
        legendLocation = 'center right')

def main():
    parser = argparse.ArgumentParser(
        description = 'Computes speedups for a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--min-runtime', dest = 'minRuntime', type = int, default = 1, help = 'Ignore quicker runs')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = computeSpeedups(cursor, args)
        if results:
            postprocessedResults = {run: common.analyzeResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(args, results)

main()
