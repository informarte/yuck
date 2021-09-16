#! /usr/bin/python3

# This script helps to compare a given set of integration test runs.
#
# For each instance, the script retrieves the objective values of the worst and the
# best solutions in order to compute, for each given run, a penalty between 0 and 1
# (using feature scaling) where 0 means the solution is one of the best and 1 means
# it is one of the worst. (In case there is no solution, the penalty is 1.)
#
# In the end the script prints, for each given run, the number of instances it failed on,
# and the penalties in terms of their mean, standard deviation, and median.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.
#
# Notice that, by default, feature scaling uses all results that the database provides.
# To restrict the analysis to the given runs, use the -r option.

import argparse
import json
import numpy
from urllib.request import pathname2url
import re
import sqlite3
import statistics
import sys

import common

def compareRuns(cursor, args):
    runQuery = 'SELECT DISTINCT run from result'
    runsInScope = args.runs if args.ignoreOtherRuns else list({result[0] for result in cursor.execute(runQuery)}.union(args.runs))
    jobQuery = 'SELECT DISTINCT problem, model, instance, problem_type FROM result WHERE run IN (%s) ORDER BY problem, model, instance' % ','.join('?' for run in runsInScope)
    problemPattern = re.compile(args.problemFilter)
    modelPattern = re.compile(args.modelFilter)
    instancePattern = re.compile(args.instanceFilter)
    jobs = [(problem, model, instance, problemType)
            for (problem, model, instance, problemType) in cursor.execute(jobQuery, runsInScope)
            if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance)]
    if not jobs:
        print('No results found', file = sys.stderr)
        return {}
    results = {}
    for run in runsInScope:
        resultQuery = 'SELECT problem, model, instance, solved, quality FROM result WHERE run = ?'
        for result in cursor.execute(resultQuery, (run,)):
            (problem, model, instance, solved, quality) = result
            if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance):
                if not run in results:
                    results[run] = {}
                task = (problem, model, instance)
                results[run][task] = {'solved': solved, 'quality': quality}
        if not run in results:
            print('Warning: No results found for run {}'.format(run, file = sys.stderr))
            results[run] = {}
        elif len(results[run]) != len(jobs):
            print('Warning: Expected {} results for run {}, but found {}'.format(len(jobs), run, len(results[run])), file = sys.stderr)
    for (problem, model, instance, problemType) in jobs:
        task = (problem, model, instance)
        qualities = [int(result['quality']) for result in [results[run][task] for run in results if task in results[run]] if result['quality']]
        optima = [int(result[0]) for result in cursor.execute('SELECT optimum FROM result WHERE problem = ? AND model = ? AND instance = ? AND optimum IS NOT NULL', task)]
        qualities += optima
        highScores = [int(result[0]) for result in cursor.execute('SELECT high_score FROM result WHERE problem = ? AND model = ? AND instance = ? AND high_score IS NOT NULL', task)]
        qualities += highScores
        (low, high) = (None, None) if not qualities else (min(qualities), max(qualities))
        if args.verbose:
            print('-' * 80)
            print(problem, model, instance, problemType, low, high)
        for run in args.runs:
            if run in results and task in results[run]:
                result = results[run][task]
                solved = result['solved']
                quality = result['quality']
                if solved:
                    if high == low:
                        penalty = 0
                    elif problemType == 'MIN':
                        penalty = (quality - low) / (high - low)
                    else:
                        penalty = 1 - ((quality - low) / (high - low))
                    if args.verbose:
                        print(run, quality, penalty)
                else:
                    penalty = 1
                result['penalty'] = penalty
    return {run: results[run] for run in args.runs}

def postprocessResult(result):
    analysis = {}
    task2penalty = {task: result[task]['penalty'] for task in result if 'penalty' in result[task]}
    penalties = [task2penalty[task] for task in task2penalty]
    if penalties:
        analysis = common.analyzeResult(task2penalty, range = (0, max(penalties)))
    analysis['failures'] = len([task for task in result if not result[task]['solved']])
    return analysis

def plotDiagrams(args, results):
    title = 'Penalties'
    filters = \
        ([args.problemFilter] if args.problemFilter else []) + \
        ([args.modelFilter] if args.modelFilter else []) + \
        ([args.instanceFilter] if args.instanceFilter else [])
    if filters:
        title += ' ({})'.format(', '.join(filters))
    common.plotDiagrams(
        [run for run in results],
        lambda run: [results[run][task]['penalty'] for task in results[run] if 'penalty' in results[run][task]],
        title = title,
        xlabel = 'Penalty',
        legendLocation = 'upper center')

def main():
    parser = argparse.ArgumentParser(
        description = 'Helps to compare a given set of integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('-r', '--ignore-other-runs', dest = 'ignoreOtherRuns', action = 'store_true', help = 'Ignore results from runs other than the given ones')
    parser.add_argument('--problem-filter', dest = 'problemFilter', default = '', help = 'Consider only problems that match the given regexp')
    parser.add_argument('--model-filter', dest = 'modelFilter', default = '', help = 'Consider only models that match the given regexp')
    parser.add_argument('--instance-filter', dest = 'instanceFilter', default = '', help = 'Consider only instances that match the given regexp')
    parser.add_argument('-v', '--verbose', action = 'store_true')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = compareRuns(cursor, args)
        if results:
            postprocessedResults = {run: postprocessResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(args, results)

main()
