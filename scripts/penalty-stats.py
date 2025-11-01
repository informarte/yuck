#! /usr/bin/python3

# This script helps to compare a given set of integration test runs.
#
# The comparison is limited to optimization problems.
#
# For each run and each instance, the script computes a penalty. In the end,
# a five-numbers penalty summary is printed for each run.
#
# The result database is expected to reside in the working directory under the
# name results.db unless another name is specified by way of the --db option.
#
# Two methods are implemented:
#
# - Method A compares to results from the MiniZinc challenge.
# - Method B compares the runs to each other.
#
# Method A requires an additional database to be specified by means of the --ref-db
# option, see https://github.com/informarte/minizinc-challenge-results.
#
# Method A
#
# For each instance, the script retrieves the objective values of the worst and
# the best solutions from the given challenge results in order to compute, for each
# given run, a penalty (using feature scaling) where:
#
# - <0 means the solution is even better than the best challenge results.
# - 0 means the solution is as good as the best challenge results.
# - 1 means the solution is as bad as the worst challenge results.
# - >1 means the solution is even worse than the worst challenge results.
#
# Unsolved instances are ignored.
#
# If all reference solutions to a given instance have the same objective value and
# the solution to the instance is worse than the reference solutions, then one bound
# is moved to facilitate feature scaling.
#
# Method B
#
# For each instance, the script retrieves the objective values of the worst and the
# best solutions in order to compute, for each given run, a penalty between 0 and 1
# (using feature scaling) where 0 means the solution is one of the best and 1 means
# it is one of the worst.
#
# Only results from the given runs are considered and unsolved instances are ignored.
#
# When retrieving the worst and best objective values, only the given runs are considered.

import argparse
import json
from urllib.request import pathname2url
import os
import re
import sqlite3
import sys

import common

# Problem and instance names sometimes differ across the databases.
nameTranslationTable = {
    'fjsp': 'flexible-jobshop',
    'MZNC_connect': 'connect',
    'javarouting': 'java-routing',
    'l2p': 'linear-to-program',
    'hoist-benchmark-for-minizinc': 'hoist-scheduling',
    'p1f-pjs': 'p1f',
    'peacable_queens': 'peaceable-queens',
    'tower_challenge': 'tower'
}

def normalizeName(name):
    if name in nameTranslationTable:
        return nameTranslationTable[name]
    else:
        return os.path.basename(name).replace('_', '-').lower()

def readReferenceResultsA(cursor):
    ref = {}
    query = \
        '''SELECT r.problem, p.type, r.instance, MIN(r.objective_value), MAX(r.objective_value)
           FROM result r, problem p, solver s
           WHERE r.solved = 1 AND r.objective_value IS NOT NULL AND r.solver = s.name AND s.name NOT LIKE "yuck%" AND r.problem = p.name
           GROUP BY r.problem, p.type, r.instance'''
    for row in cursor.execute(query, ()):
        (problem, problemType, instance, minObjectiveValue, maxObjectiveValue) = row
        task = (normalizeName(problem), normalizeName(instance))
        if minObjectiveValue == maxObjectiveValue:
            # There is only one objective value in the reference data, so we move one of the bounds to facilitate
            # feature scaling.
            print('Warning: Reference results have only one objective value for {}'.format(task), file = sys.stderr)
            if problemType == 'MIN':
                maxObjectiveValue += 1
            else:
                minObjectiveValue -= 1
        ref[task] = (problemType, minObjectiveValue, maxObjectiveValue)
    return ref

def computePenaltyA(row, ref):
    (problem, model, instance, objectiveValue) = row
    refKey1 = (normalizeName(problem), normalizeName(instance))
    refKey2 = (normalizeName(model), refKey1[1])
    if refKey1 in ref:
        (problemType, minObjectiveValue, maxObjectiveValue) = ref[refKey1]
    elif refKey2 in ref:
        (problemType, minObjectiveValue, maxObjectiveValue) = ref[refKey2]
    else:
        print('Warning: No reference results for {}, skipping it'.format((problem, model, instance)), file = sys.stderr)
        return None
    if problemType == 'MIN':
        penalty = (objectiveValue - minObjectiveValue) / (maxObjectiveValue - minObjectiveValue)
    else:
        penalty = 1 - ((objectiveValue - minObjectiveValue) / (maxObjectiveValue - minObjectiveValue))
    return penalty

def readReferenceResultsB(cursor, args):
    ref = {}
    query = \
        '''SELECT problem, problem_type, model, instance, optimum, high_score, MIN(objective_value), MAX(objective_value)
           FROM result
           WHERE solved = 1 AND objective_value IS NOT NULL AND run IN (%s)
           GROUP BY problem, problem_type, model, instance''' \
        % ','.join('?' for _ in args.runs)
    for row in cursor.execute(query, args.runs):
        (problem, problemType, model, instance, optimum, highScore, minObjectiveValue, maxObjectiveValue) = row
        objectiveValues = [minObjectiveValue, maxObjectiveValue]
        if optimum:
            objectiveValues += [optimum]
        elif highScore:
            objectiveValues += [highScore]
        ref[(problem, model, instance)] = (problemType, min(objectiveValues), max(objectiveValues))
    if not ref:
        print('No results found', file = sys.stderr)
    return ref

def computePenaltyB(row, ref):
    (problem, model, instance, objectiveValue) = row
    (problemType, minObjectiveValue, maxObjectiveValue) = ref[(problem, model, instance)]
    if maxObjectiveValue == minObjectiveValue:
        penalty = 0
    elif problemType == 'MIN':
        penalty = (objectiveValue - minObjectiveValue) / (maxObjectiveValue - minObjectiveValue)
    else:
        penalty = 1 - ((objectiveValue - minObjectiveValue) / (maxObjectiveValue - minObjectiveValue))
    return penalty

def compareRuns(cursor, ref, computePenalty, args):
    problemPattern = re.compile(args.problemFilter)
    modelPattern = re.compile(args.modelFilter)
    instancePattern = re.compile(args.instanceFilter)
    query = \
        '''SELECT problem, model, instance, objective_value
            FROM result
            WHERE run = ? AND solved = 1 AND objective_value IS NOT NULL'''
    results = {}
    for run in args.runs:
        for row in cursor.execute(query, (run,)):
            (problem, model, instance, objectiveValue) = row
            if problemPattern.match(problem) and modelPattern.match(model) and instancePattern.match(instance):
                if not run in results:
                    results[run] = {}
                task = (problem, model, instance)
                penalty = computePenalty(row, ref)
                if penalty is not None:
                    if args.verbose:
                        print(run, problem, model, instance, objectiveValue, penalty)
                    results[run][task] = {'solved': True, 'objective-value': objectiveValue, 'penalty': penalty}
        if not run in results:
            print('Warning: No results found for run {}'.format(run), file = sys.stderr)
            results[run] = {}
    return {run: results[run] for run in args.runs}

def postprocessResult(result):
    analysis = {}
    task2penalty = {task: result[task]['penalty'] for task in result}
    penalties = [task2penalty[task] for task in task2penalty]
    if penalties:
        analysis = common.analyzeResult(task2penalty, range = (0, max(penalties)))
    return analysis

def plotDiagrams(args, results):
    title = 'Penalties (without extreme outliers)'
    filters = \
        ([args.problemFilter] if args.problemFilter else []) + \
        ([args.modelFilter] if args.modelFilter else []) + \
        ([args.instanceFilter] if args.instanceFilter else [])
    if filters:
        title += ' ({})'.format(', '.join(filters))
    common.plotDiagrams(
        [run for run in results],
        lambda run: list(
            filter(
                lambda penalty: abs(penalty) <= 10,
                (results[run][task]['penalty'] for task in results[run]))),
        title = title,
        xlabel = 'Penalty (lower is better)',
        legendLocation = 'upper center')

def main():
    parser = argparse.ArgumentParser(
        description = 'Helps to compare a given set of integration test runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'resultsDb', default = 'results.db', help = 'Define results database')
    parser.add_argument('--ref-db', '--reference-database', dest = 'refDb', default = None, help = 'Define database with reference results')
    parser.add_argument('-p', '--plot', dest = 'plotDiagrams', action = 'store_true', help = 'Plot diagrams')
    parser.add_argument('--problem-filter', dest = 'problemFilter', default = '', help = 'Consider only problems that match the given regexp')
    parser.add_argument('--model-filter', dest = 'modelFilter', default = '', help = 'Consider only models that match the given regexp')
    parser.add_argument('--instance-filter', dest = 'instanceFilter', default = '', help = 'Consider only instances that match the given regexp')
    parser.add_argument('-v', '--verbose', action = 'store_true')
    parser.add_argument('runs', metavar = 'run', nargs = '+')
    args = parser.parse_args()
    with sqlite3.connect('file:{}?mode=ro'.format(pathname2url(args.resultsDb)), uri = True) as conn:
        if args.refDb:
            # A
            with sqlite3.connect('file:{}?mode=ro'.format(pathname2url(args.refDb)), uri = True) as refConn:
                ref = readReferenceResultsA(refConn.cursor())
                results = compareRuns(conn.cursor(), ref, computePenaltyA, args)
        else:
            # B
            cursor = conn.cursor()
            ref = readReferenceResultsB(cursor, args)
            results = compareRuns(cursor, ref, computePenaltyB, args)
        if results:
            postprocessedResults = {run: postprocessResult(results[run]) for run in results}
            print(json.dumps(postprocessedResults, sort_keys = True, indent = 4))
            if args.plotDiagrams:
                plotDiagrams(args, results)

main()
