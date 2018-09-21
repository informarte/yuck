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
# The database is expected to reside in the working directory under the name results.db.
#
# Notice that, by default, feature scaling uses all results that the database provides.
# To restrict the analysis to the given runs, use the -r option.

import argparse
import json
import matplotlib.pyplot as plt
import numpy
import sqlite3
import statistics
import sys

def evalRuns(cursor, args):
    jobQuery = 'SELECT DISTINCT problem, model, instance, problem_type FROM result'
    if args.problemType:
        jobQuery += ' WHERE problem_type = ?'
    jobQuery += ' ORDER BY problem, model, instance';
    jobs = list(cursor.execute(jobQuery, (args.problemType, )) if args.problemType else cursor.execute(jobQuery))
    if not jobs:
        print('No results found', file = sys.stderr)
        return {}
    runsInScope = args.runs if args.ignoreOtherRuns else list(map(lambda result: result[0], cursor.execute('SELECT DISTINCT run from result')));
    results = {}
    penalties = {}
    failures = {}
    for run in runsInScope:
        resultQuery = 'SELECT solved, quality FROM result WHERE run = ?';
        if args.problemType:
            resultQuery += ' AND problem_type = ?'
        resultQuery += ' ORDER BY problem, model, instance';
        results[run] = list(cursor.execute(resultQuery, (run, args.problemType)) if args.problemType else cursor.execute(resultQuery, (run,)))
        if len(results[run]) != len(jobs):
            print('Expected {} results for run {}, but found {}'.format(len(jobs), run, len(results[run])), file = sys.stderr)
            return {}
    for run in args.runs:
        penalties[run] = []
        failures[run] = 0
    for i in range(0, len(jobs)):
        (problem, model, instance, problemType) = jobs[i]
        qualities = [int(result[1]) for result in [results[run][i] for run in runsInScope] if result[0] and result[1]]
        optima = [int(result[0]) for result in cursor.execute('SELECT optimum FROM result WHERE problem = ? AND model = ? AND instance = ? AND optimum IS NOT NULL', (problem, model, instance))]
        qualities += optima
        highScores = [int(result[0]) for result in cursor.execute('SELECT high_score FROM result WHERE problem = ? AND model = ? AND instance = ? AND high_score IS NOT NULL', (problem, model, instance))]
        qualities += highScores
        (low, high) = (None, None) if not qualities else (min(qualities), max(qualities))
        if args.verbose:
            print('-' * 80)
            print(problem, instance, problemType, low, high)
        for run in args.runs:
            (solved, quality) = results[run][i]
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
            penalties[run] += [penalty]
    return {run: {'failures': failures[run], 'penalties': penalties[run]} for run in args.runs}

def postprocessResult(result):
    penalties = result['penalties']
    quartiles = numpy.percentile(penalties, [25, 50, 75])
    return {
        'failures': result['failures'],
        'penalty-min': min(penalties),
        'penalty-q1': quartiles[0],
        'penalty-q2': quartiles[1],
        'penalty-q3': quartiles[2],
        'penalty-max': max(penalties),
        'penalty-mean': statistics.mean(penalties),
        'penalty-pstdev': statistics.pstdev(penalties),
        'penalty-histogram': numpy.histogram(penalties, 10, (0, 1))[0].tolist()
    }

def plotDiagrams(results):
    fig, (ax1, ax2, ax3) = plt.subplots(ncols = 1, nrows = 3, sharex = True, constrained_layout=True)
    fig.suptitle('Run comparison', fontsize = 'x-large')
    for run in results:
        ax1.hist(results[run]['penalties'], 25, histtype = 'step', cumulative = False, label = run)
        ax2.hist(results[run]['penalties'], 100, histtype = 'step', cumulative = True, label = run)
        ax3.boxplot([results[run]['penalties'] for run in results], vert = False, labels = [run for run in results], showmeans = True)
    ax1.grid(True)
    ax1.legend(loc='upper center', fontsize = 'medium')
    ax1.set_ylabel('Number of results')
    ax2.grid(True)
    ax2.set_ylabel('Cumulative number of results')
    ax3.grid(True)
    ax3.set_xlabel('Penalty')
    plt.show()

def main():
    parser = argparse.ArgumentParser(description = 'Helps to evaluate a given set of Yuck integration test runs')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action='store_true', help = 'Plot diagrams')
    parser.add_argument('-r', '--ignore-other-runs', dest = 'ignoreOtherRuns', action='store_true', help = 'Ignore results from runs other than the given ones')
    parser.add_argument('-t', '--problem-type', dest = 'problemType', choices = ['SAT', 'MIN', 'MAX'], help = 'Restrict analysis to given problem type')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    with sqlite3.connect("results.db") as conn:
        cursor = conn.cursor()
        results = evalRuns(cursor, args)
        if results:
            postprocessedResults = {run: postprocessResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if (args.plotDiagrams):
                plotDiagrams(results)

main()
