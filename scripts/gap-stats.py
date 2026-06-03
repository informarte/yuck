#! /usr/bin/python3

# This script helps to compare a given set of integration test runs.
#
# For each instance and each run, this script computes the gap between the best known
# objective value and the value achieved in the run.
#
# In the end the script prints, for each given run, the gaps in terms of their mean,
# standard deviation, and median.
#
# This script only works for minimization problems with non-negative objective values.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import sqlite3
import sys

import common
import re

def computeGaps(cursor, args):
    jobQuery = \
        f'''SELECT DISTINCT problem, model, instance, problem_type
            FROM result
            WHERE run IN ({','.join('?' for _ in args.runs)}) AND {args.filter}
            ORDER BY problem, model, instance'''
    jobs = list(cursor.execute(jobQuery, args.runs))
    if not jobs:
        print('No results found', file = sys.stderr)
        return {}
    results = {}
    for run in args.runs:
        resultQuery = 'SELECT problem, model, instance, solved, objective_value FROM result WHERE run = ?'
        for row in cursor.execute(resultQuery, (run,)):
            (problem, model, instance, solved, objectiveValue) = row
            if not run in results:
                results[run] = {}
            task = (problem, model, instance)
            results[run][task] = {'solved': solved, 'objective-value': objectiveValue}
        if not run in results:
            print(f'Warning: No results found for run {run}', file = sys.stderr)
            results[run] = {}
        elif len(results[run]) != len(jobs):
            print(f'Warning: Expected {len(jobs)} results for run {run}, but found {len(results[run])}', file = sys.stderr)
    for (problem, model, instance, problemType) in jobs:
        if problemType != 'MIN':
            raise ValueError(f'Unsupported problem type {problemType}')
        task = (problem, model, instance)
        objectiveValues = [int(result['objective-value']) for result in [results[run][task] for run in results if task in results[run]] if result['objective-value']]
        objectiveValues += [int(result[0]) for result in cursor.execute('SELECT optimum FROM result WHERE problem = ? AND model = ? AND instance = ? AND optimum IS NOT NULL', task)]
        objectiveValues += [int(result[0]) for result in cursor.execute('SELECT high_score FROM result WHERE problem = ? AND model = ? AND instance = ? AND high_score IS NOT NULL', task)]
        bestObjectiveValue = None if not objectiveValues else min(objectiveValues)
        if not bestObjectiveValue:
            raise ValueError(f'{(problem, model, instance)}: No best known solution')
        if bestObjectiveValue < 0:
            raise ValueError(f'{(problem, model, instance)}: Best known solution has negative objective value')
        if args.verbose:
            print('-' * 80)
            print(problem, model, instance, problemType, bestObjectiveValue)
        for run in results:
            if task in results[run]:
                result = results[run][task]
                solved = result['solved']
                objectiveValue = result['objective-value']
                if solved:
                    if not objectiveValue:
                        raise ValueError(f'{(run, problem, model, instance)}: No objective value')
                    if objectiveValue < 0:
                        raise ValueError(f'{(run, problem, model, instance)}: Negative objective value')
                    gap = objectiveValue / bestObjectiveValue
                    if args.verbose:
                        print(run, objectiveValue, gap)
                    result['gap'] = gap
    return results

def postprocessResult(result):
    analysis = {}
    task2gap = {task: result[task]['gap'] for task in result if 'gap' in result[task]}
    gaps = [task2gap[task] for task in task2gap]
    if gaps:
        analysis = common.analyzeResult(task2gap, range = (0, max(gaps)))
    analysis['failures'] = len([task for task in result if not result[task]['solved']])
    return analysis

def plotDiagrams(args, results):
    title = 'Gaps'
    if args.filter != 'true':
        title += f' (where {args.filter})'
    common.plotDiagrams(
        [run for run in results],
        lambda run: [results[run][task]['gap'] for task in results[run] if 'gap' in results[run][task]],
        title = title,
        xlabel = 'Objective value / known best objective value',
        legendLocation = 'upper center')

def main():
    parser = argparse.ArgumentParser(
        description = 'Helps to compare a given set of integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('-v', '--verbose', action = 'store_true')
    parser.add_argument('--filter', dest = 'filter', default = 'true', help = 'SQL filter expression')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = f'file:{pathname2url(args.database)}?mode=ro'
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        try:
            results = computeGaps(cursor, args)
            if results:
                postprocessedResults = {run: postprocessResult(results[run]) for run in results}
                print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
                if args.plotDiagrams:
                    plotDiagrams(args, results)
        except ValueError as e:
            print(e, file = sys.stderr)

main()
