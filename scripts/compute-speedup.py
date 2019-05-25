#! /usr/bin/python3

# This script computes speedups for a given set of Yuck integration test runs.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
import numpy
import matplotlib.pyplot as plt
from urllib.request import pathname2url
import sqlite3
import statistics
import sys

def computeSpeedups(cursor, args):
    runs = [args.referenceRun] + args.runs
    query = 'SELECT run, problem, model, instance, moves_per_second, runtime_in_seconds FROM result WHERE run IN (%s)' % ','.join('?' for run in runs)
    tasks = set()
    mps = {}
    rt = {}
    for (run, problem, model, instance, movesPerSecond, runtimeInSeconds) in cursor.execute(query, runs):
        task = (problem, model, instance)
        tasks.add(task)
        mps[(run, task)] = movesPerSecond
        rt[(run, task)] = runtimeInSeconds
    result = {run: {task: mps[(run, task)] / mps[(args.referenceRun, task)]
                    for task in tasks
                    if (run, task) in mps and mps[(run, task)] and
                       (args.referenceRun, task) in mps and mps[(args.referenceRun, task)] and
                    (not (run, task) in rt or not rt[(run, task)] or
                     not (args.referenceRun, task) in rt or not rt[(args.referenceRun, task)] or
                     (rt[(run, task)] >= args.minRuntime and rt[(args.referenceRun, task)] >= args.minRuntime))}
              for run in args.runs}
    return {run: result[run] for run in result if result[run]}

def getSpeedups(result):
    return [result[task] for task in result]

def analyzeSpeedups(result):
    speedups = getSpeedups(result)
    quartiles = numpy.percentile(speedups, [25, 50, 75])
    q1 = quartiles[0]
    q2 = quartiles[1]
    q3 = quartiles[2]
    iqr = q3 - q1
    return {
        'speedup-min': min(speedups),
        'speedup-q1': q1,
        'speedup-q2': q2,
        'speedup-q3': q3,
        'speedup-max': max(speedups),
        # harmonic mean might be more appropriate, but it is available only from Python 3.6
        'speedup-mean': statistics.mean(speedups),
        'speedup-pstdev': statistics.pstdev(speedups),
        'speedup-histogram': numpy.histogram(speedups, 'auto')[0].tolist(),
        'extreme-values': {task[0] + "/" + task[1] + "/" + task[2]: result[task]
                           for task in result
                           if result[task] < q1 - 1.5 * iqr or result[task] > q3 + 1.5 * iqr}
    }

def plotDiagrams(results):
    fig, (ax1, ax2, ax3) = plt.subplots(ncols = 1, nrows = 3, sharex = True, constrained_layout=True)
    fig.suptitle('Speedups', fontsize = 'x-large')
    for run in results:
        speedups = getSpeedups(results[run])
        ax1.hist(speedups, 100, histtype = 'step', cumulative = False, label = run)
        ax2.hist(speedups, 100, histtype = 'step', cumulative = True, label = run)
    ax3.boxplot([getSpeedups(results[run]) for run in results], vert = False, labels = [run for run in results], showmeans = True)
    ax1.grid(True)
    ax1.legend(loc='center right', fontsize = 'medium')
    ax1.set_ylabel('Number of results')
    ax2.grid(True)
    ax2.set_ylabel('Cumulative number of results')
    ax3.grid(True)
    ax3.set_xlabel('Speedup')
    plt.show()

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
            postprocessedResults = {run: analyzeSpeedups(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(results)

main()
