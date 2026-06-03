#! /usr/bin/python3

# This script analyzes data points produced by integration test runs.
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

def retrieveValues(cursor, args):
    query = \
        f'''SELECT run, problem, model, instance, {args.metric}
            FROM result
            WHERE run IN ({','.join('?' for _ in args.runs)}) AND {args.filter} AND {args.metric} IS NOT NULL'''
    results = {run: {} for run in args.runs}
    for (run, problem, model, instance, value) in cursor.execute(query, args.runs):
        task = (problem, model, instance)
        results[run][task] = value
    return results

def plotDiagrams(args, results):
    title = args.metric
    if args.filter != 'true':
        title += f' (where {args.filter})'
    common.plotDiagrams(
        args.runs,
        lambda run: list(results[run][task] for task in results[run]),
        title = title,
        xlabel = 'Value',
        legendLocation = 'center right')

def main():
    parser = argparse.ArgumentParser(
        description = 'Computes metric stats for a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('--metric', default = 'moves_per_second', help = 'Some numeric column created and filled by import-results.py')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--filter', dest = 'filter', default = 'true', help = 'SQL filter expression')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = retrieveValues(cursor, args)
        if results:
            for run in results:
                if not results[run]:
                    print(f'Warning: No data for run {run}', file = sys.stderr)
            postprocessedResults = {run: common.analyzeResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if args.plotDiagrams:
                plotDiagrams(args, results)

main()
