#! /usr/bin/python3

# This script was inspired by the area scoring procedure as defined by the rules of the
# MiniZinc challenge:
# For each instance of an optimization problem, Yuck's test infrastructure provides
# the integral of the quality step function over the runtime horizon (called "area")
# when no negative objective values were encountered during solving the instance.
# This script retrieves the areas and, for each given run, compares the areas to those
# from a given reference run resulting in a ratio for each instance.
# For satisfaction problems - which don't have an area - the runtime is used instead.
#
# To simplify the interpretation of the output, the following filters apply:
# - Results from maximization problems are ignored when runtimes differ considerably.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import sqlite3
import sys

import common

def computeAreaRatios(cursor, args):
    runs = [args.referenceRun] + args.runs
    query = 'SELECT run, problem, model, instance, problem_type, quality, area, runtime_in_seconds FROM result WHERE run IN (%s)' % ','.join('?' for run in runs)
    tasks = set()
    data = {}
    for (run, problem, model, instance, problemType, quality, area, runtimeInSeconds) in cursor.execute(query, runs):
        task = (problem, model, instance)
        tasks.add(task)
        data[(run, task)] = {'problemType': problemType, 'quality': quality, 'area': area, 'rts': runtimeInSeconds}
    return {
        run: {
            task:
            newData['rts'] / refData['rts']
            if newData['problemType'] == 'SAT'
            else newData['area'] / refData['area']
            if newData['problemType'] == 'MIN'
            else refData['area'] / newData['area']
            for (task, newData, refData) in
            [(task, data[(run, task)], data[(args.referenceRun, task)])
             for task in tasks
             if (run, task) in data
             if (args.referenceRun, task) in data]
            if (newData['problemType'] != 'SAT' or
                (newData['rts'] and refData['rts']))
            if (newData['problemType'] == 'SAT' or
                (newData['area'] and refData['area']))
            if (newData['problemType'] != 'MAX' or
                ((newData['rts'] or 0) >= (refData['rts'] or 0) * (1 - args.runtimeTolerance) and
                 (newData['rts'] or 0) <= (refData['rts'] or 0) * (1 + args.runtimeTolerance)))
            if (newData['rts'] or 0) >= args.minRuntime
            if (refData['rts'] or 0) >= args.minRuntime
        }
        for run in args.runs
    }

def plotDiagrams(results):
    common.plotDiagrams(
        [run for run in results],
        lambda run: (lambda result: [result[task] for task in result])(results[run]),
        title = 'Area ratios',
        xlabel = 'Area ratio (lower is better)',
        legendLocation = 'center right')

def main():
    parser = argparse.ArgumentParser(
        description = 'Computes area ratios for a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--min-runtime', dest = 'minRuntime', type = int, default = 1, help = 'Ignore quicker runs')
    parser.add_argument('--runtime-tolerance', dest = 'runtimeTolerance', type = float, default = 0.05, help = 'Ignore result of run when it was considerably quicker or slower than the reference run (applies to maximization only)')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = computeAreaRatios(cursor, args)
        if results:
            postprocessedResults = {run: common.analyzeResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(results)

main()
