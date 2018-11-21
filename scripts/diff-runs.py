#! /usr/bin/python3

# Compares two runs and finds about lost and new solutions.

import argparse
import json
import sqlite3
import sys

def diffResults(cursor, args):
    query = 'SELECT suite, problem, model, instance FROM RESULT WHERE run = ? AND solved = 1'
    refResults = set(cursor.execute(query, (args.referenceRun, )))
    newResults = set(cursor.execute(query, (args.newRun, )))
    def formatResult(result):
        (suite, problem, model, instance) = result
        return {'suite': suite, 'problem': problem, 'model': model, 'instance': instance}
    return {
        'lostSolutions': [formatResult(result) for result in (refResults - newResults)],
        'newSolutions': [formatResult(result) for result in (newResults - refResults)]
    }

def main():
    parser = argparse.ArgumentParser(description = 'Diffs two given runs')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('newRun', metavar = 'new-run')
    args = parser.parse_args()
    with sqlite3.connect("results.db") as conn:
        cursor = conn.cursor()
        results = diffResults(cursor, args)
        if results:
            print(json.dumps(results, sort_keys = True, indent = 4))

main()

