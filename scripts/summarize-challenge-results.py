#! /usr/bin/python3

# Summarizes MiniZinc challenge results for use with Yuck's test infrastructure.

# The result database is expected to reside in the working directory under the name results.db
# unless another name is specified by way of the --db option.

import argparse
import json
from urllib.request import pathname2url
import sqlite3
import sys

# Replacement for sqlite3 package providing stats functions: https://antonz.org/sqlean/

def createSummary(row):
    (year, problem, problemType, instance, solved, complete, minObjectiveValue, maxObjectiveValue) = row
    summary = {
        'year': year,
        'problem': problem,
        'problem-type': problemType,
        'instance': instance
    }
    if solved:
        summary['satisfiable'] = True
        if problemType != 'SAT':
            if minObjectiveValue != None and maxObjectiveValue != None:
                summary['min-objective-value'] = minObjectiveValue
                summary['max-objective-value'] = maxObjectiveValue
                summary['solved-to-optimality'] = not not complete
            else:
                # In the 2012 results, some objective values are missing.
                print('Warning: Optimization result without objective value for {}'
                      .format((year, problem, instance)),
                      file = sys.stderr)
    elif complete:
        summary['satisfiable'] = False
    return summary

def readResults(cursor):
    query = \
        '''SELECT r.year, r.problem, p.type, r.instance,
                  MAX(r.solved), MAX(r.complete), MIN(r.objective_value), MAX(r.objective_value)
           FROM problem p, result r
           WHERE r.solver NOT LIKE "yuck%" AND r.problem = p.name
           GROUP BY r.year, r.problem, r.instance'''
    return [createSummary(row) for row in cursor.execute(query, ())]

def main():
    parser = argparse.ArgumentParser(
        description = "Dumps challenge results for use with Yuck's test infrastructure",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    args = parser.parse_args()
    dburi = 'file:{}?mode=ro'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        results = readResults(cursor)
        print(json.dumps(results, sort_keys = True, indent = 4))

main()
