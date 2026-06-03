#! /usr/bin/python3

# This script helps to compare a set of integration test runs to a reference run.
#
# Given the runs and a metric, the script retrieves the data points in order to compute,
# for each run (other than the reference run) and instance, the ratio of the value measured
# in the run to the value measured in the reference run.
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

def computeRatios(cursor, args):
    runs = [args.referenceRun] + args.runs
    query = \
        f'''SELECT run, problem, model, instance, {args.metric}
            FROM result
            WHERE run IN ({','.join('?' for _ in runs)}) and {args.filter}'''
    tasks = set()
    metricValues = {}
    for (run, problem, model, instance, metricValue) in cursor.execute(query, runs):
        task = (problem, model, instance)
        tasks.add(task)
        metricValues[(run, task)] = metricValue
    for task in tasks:
        if not (args.referenceRun, task) in metricValues:
            print(f'Warning: No reference result found for {task}', file = sys.stderr)
    return {
        run: {
            task:
            value / refValue
            for (task, value, refValue) in
            [(task, metricValues[(run, task)], metricValues[(args.referenceRun, task)])
             for task in tasks
             if (run, task) in metricValues
             if (args.referenceRun, task) in metricValues]
            if value
            if refValue and refValue != 0
        }
        for run in args.runs
    }

def plotDiagrams(args, results):
    title = f'{args.metric} wrt. {args.referenceRun}'
    if args.filter != 'true':
        title += f' (where {args.filter})'
    common.plotDiagrams(
        [run for run in results],
        lambda run: list(
            filter(
                lambda value: abs(value) <= 10,
                (results[run][task] for task in results[run]))),
        title = title,
        xlabel = 'New value / reference value (without extreme outliers)',
        legendLocation = 'center right')

def main():
    parser = argparse.ArgumentParser(
        description = 'Computes metric stats for a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('--metric', default = 'moves_per_second', help = 'Some numeric column created and filled by import-results.py')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--filter', dest = 'filter', default = 'true', help = 'SQL filter expression')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = f'file:{pathname2url(args.database)}?mode=ro'
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = computeRatios(cursor, args)
        if results:
            for run in results:
                if not results[run]:
                    print(f'Warning: No data for run {run}', file = sys.stderr)
            postprocessedResults = {run: common.analyzeResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if args.plotDiagrams:
                plotDiagrams(args, results)

main()
