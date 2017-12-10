#! /usr/bin/python3

# This script compares the performance of Yuck on two given test runs.
#
# The result database is expected to reside in the working directory under the name results.db.

import argparse
import json
import numpy
import sqlite3
import statistics
import sys

def computeSpeedups(cursor, run1, run2):
    tasks = list(cursor.execute('SELECT DISTINCT problem, model, instance, problem_type FROM result ORDER BY problem, model, instance'))
    resultQuery = 'SELECT solved, moves_per_second FROM result WHERE run = ? AND problem = ? AND model = ? AND instance = ?'
    for problem, model, instance, problemType in tasks:
        result1 = cursor.execute(resultQuery, (run1, problem, model, instance))
        solved1, mps1 = result1.fetchone()
        result2 = cursor.execute(resultQuery, (run2, problem, model, instance))
        solved2, mps2 = result2.fetchone()
        yield {'problem': problem, 'model': model, 'instance': instance, 'speedup': mps2 / mps1 if mps1 and mps2 else None}

def main():
    parser = argparse.ArgumentParser(description = 'Compares the performance of Yuck on two given test runs')
    parser.add_argument('run1', metavar = 'run1')
    parser.add_argument('run2', metavar = 'run2')
    args = parser.parse_args()
    with sqlite3.connect("results.db") as conn:
        cursor = conn.cursor()
        results = list(computeSpeedups(cursor, args.run1, args.run2))
        speedups = sorted([result['speedup'] for result in results])
        stats = {
            'speedup-min': min(speedups),
            'speedup-max': max(speedups),
            'speedup-mean': statistics.mean(speedups),
            # harmonic mean might be more appropriate, but it is available only from Python 3.6
            'speedup-pstdev': statistics.pstdev(speedups),
            'speedup-median': statistics.median(speedups),
            'speedup-histogram': numpy.histogram(speedups, 'auto')[0].tolist()
        }
        print(json.dumps(stats, sort_keys = True, indent = 4))

main()
