#! /usr/bin/python3

# This script helps to evaluate a given set of Yuck integration test runs.
#
# For each instance, the script retrieves the objective value of the best solution
# in order to compute, for each given run, a penalty between 0 and 1 (using feature
# scaling) where 0 means the solution is one of the best and 1 means it is one of the
# worst. (In case there is no solution, the penalty is 1.)
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
import sqlite3
import statistics
import sys

import common

def evalRuns(cursor, args):
    runsInScope = args.runs if args.ignoreOtherRuns else list(map(lambda result: result[0], cursor.execute('SELECT DISTINCT run from result')));
    jobQuery = 'SELECT DISTINCT problem, model, instance, problem_type FROM result WHERE run IN (%s) ORDER BY problem, model, instance' % ','.join('?' for run in runsInScope)
    jobs = list(cursor.execute(jobQuery, runsInScope))
    if not jobs:
        print('No results found', file = sys.stderr)
        return {}
    results = {}
    penalties = {}
    failures = {}
    for run in runsInScope:
        resultQuery = 'SELECT problem, model, instance, solved, quality FROM result WHERE run = ?';
        for result in cursor.execute(resultQuery, (run,)):
            (problem, model, instance, solved, quality) = result
            if not run in results:
                results[run] = {}
            results[run][(problem, model, instance)] = (solved, quality)
        if len(results[run]) != len(jobs):
            print('Warning: Expected {} results for run {}, but found {}'.format(len(jobs), run, len(results[run])), file = sys.stderr)
    for run in args.runs:
        penalties[run] = []
        failures[run] = 0
    for (problem, model, instance, problemType) in jobs:
        task = (problem, model, instance)
        if not args.problemType or args.problemType == problemType:
            qualities = [int(result[1]) for result in [results[run][task] for run in runsInScope if task in results[run]] if result[0] and result[1]]
            optima = [int(result[0]) for result in cursor.execute('SELECT optimum FROM result WHERE problem = ? AND model = ? AND instance = ? AND optimum IS NOT NULL', (problem, model, instance))]
            qualities += optima
            highScores = [int(result[0]) for result in cursor.execute('SELECT high_score FROM result WHERE problem = ? AND model = ? AND instance = ? AND high_score IS NOT NULL', (problem, model, instance))]
            qualities += highScores
            (low, high) = (None, None) if not qualities else (min(qualities), max(qualities))
            if args.verbose:
                print('-' * 80)
                print(problem, instance, problemType, low, high)
            for run in args.runs:
                if task in results[run]:
                    (solved, quality) = results[run][task]
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
                        failures[run] += 1
                        penalty = 1
                else:
                    failures[run] += 1
                    penalty = 1
                penalties[run] += [penalty]
    return {run: {'failures': failures[run], 'penalties': penalties[run]} for run in args.runs}

def postprocessResult(result):
    penalties = result['penalties']
    quartiles = numpy.percentile(penalties, [25, 50, 75])
    return {
        'failures': result['failures'],
        'penalties': {
            'min': min(penalties),
            'q1': quartiles[0],
            'q2': quartiles[1],
            'q3': quartiles[2],
            'max': max(penalties),
            'mean': statistics.mean(penalties),
            'pstdev': statistics.pstdev(penalties),
            'histogram': numpy.histogram(penalties, 10, (0, 1))[0].tolist()
        }
    }

def plotDiagrams(results):
    common.plotDiagrams(
        [run for run in results],
        lambda run: results[run]['penalties'],
        title = 'Run comparison',
        xlabel = 'Penalty',
        legendLocation = 'upper center')


def main():
    parser = argparse.ArgumentParser(
        description = 'Helps to evaluate a given set of Yuck integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('-r', '--ignore-other-runs', dest = 'ignoreOtherRuns', action = 'store_true', help = 'Ignore results from runs other than the given ones')
    parser.add_argument('-t', '--problem-type', dest = 'problemType', choices = ['SAT', 'MIN', 'MAX'], help = 'Restrict analysis to given problem type')
    parser.add_argument('-v', '--verbose', action = 'store_true')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = evalRuns(cursor, args)
        if results:
            postprocessedResults = {run: postprocessResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(results)

main()
