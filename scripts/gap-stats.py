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
    jobQuery = 'SELECT DISTINCT problem, model, instance, problem_type FROM result WHERE run IN (%s) ORDER BY problem, model, instance' % ','.join('?' for run in args.runs)
    problemPattern = re.compile(args.problemFilter)
    modelPattern = re.compile(args.modelFilter)
    instancePattern = re.compile(args.instanceFilter)
    jobs = [(problem, model, instance, problemType)
            for (problem, model, instance, problemType) in cursor.execute(jobQuery, args.runs)
            if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance)]
    if not jobs:
        print('No results found', file = sys.stderr)
        return {}
    results = {}
    for run in args.runs:
        resultQuery = 'SELECT problem, model, instance, solved, objective_value FROM result WHERE run = ?';
        for row in cursor.execute(resultQuery, (run,)):
            (problem, model, instance, solved, objectiveValue) = row
            if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance):
                if not run in results:
                    results[run] = {}
                task = (problem, model, instance)
                results[run][task] = {'solved': solved, 'objective-value': objectiveValue}
        if not run in results:
            print('Warning: No results found for run {}'.format(run), file = sys.stderr)
            results[run] = {}
        elif len(results[run]) != len(jobs):
            print('Warning: Expected {} results for run {}, but found {}'.format(len(jobs), run, len(results[run])), file = sys.stderr)
    for (problem, model, instance, problemType) in jobs:
        if problemType != 'MIN':
            raise ValueError('Unsupported problem type {}'.format(problemType))
        task = (problem, model, instance)
        objectiveValues = [int(result['objective-value']) for result in [results[run][task] for run in results if task in results[run]] if result['objective-value']]
        objectiveValues += [int(result[0]) for result in cursor.execute('SELECT optimum FROM result WHERE problem = ? AND model = ? AND instance = ? AND optimum IS NOT NULL', task)]
        objectiveValues += [int(result[0]) for result in cursor.execute('SELECT high_score FROM result WHERE problem = ? AND model = ? AND instance = ? AND high_score IS NOT NULL', task)]
        bestObjectiveValue = None if not objectiveValues else min(objectiveValues)
        if not bestObjectiveValue:
            raise ValueError('{}/{}/{}: No best known solution'.format(problem, model, instance))
        if bestObjectiveValue < 0:
            raise ValueError('{}/{}/{}: Best known solution has negative objective value'.format(problem, model, instance))
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
                        raise ValueError('{}/{}/{}/{}: No objective value'.format(run, problem, model, instance))
                    if objectiveValue < 0:
                        raise ValueError('{}/{}/{}/{}: Negative objective value'.format(run, problem, model, instance))
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
    filters = \
        ([args.problemFilter] if args.problemFilter else []) + \
        ([args.modelFilter] if args.modelFilter else []) + \
        ([args.instanceFilter] if args.instanceFilter else [])
    if filters:
        title += ' ({})'.format(', '.join(filters))
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
    parser.add_argument('--problem-filter', dest = 'problemFilter', default = '', help = 'Consider only problems that match the given regexp')
    parser.add_argument('--model-filter', dest = 'modelFilter', default = '', help = 'Consider only models that match the given regexp')
    parser.add_argument('--instance-filter', dest = 'instanceFilter', default = '', help = 'Consider only instances that match the given regexp')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
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
