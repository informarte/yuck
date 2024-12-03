#! /usr/bin/python3

# Compares two runs and finds out about lost and new solutions.
#
# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import sqlite3

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
    parser = argparse.ArgumentParser(
        description = 'Diffs two given runs',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('referenceRun', metavar = 'reference-run')
    parser.add_argument('newRun', metavar = 'new-run')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = diffResults(cursor, args)
        if results:
            print(json.dumps(results, sort_keys = True, indent = 4))

main()
