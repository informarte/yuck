#! /usr/bin/python3

# This script creates a database of Yuck integration test results (results.db).
#
# To import results, call the script with a list of JSON result files on the command line.
# When the database already exists, the given results will be added unless they are
# already in the database.

import argparse
import json
import sqlite3
from itertools import repeat

def createDb(cursor):
    cursor.execute('CREATE TABLE IF NOT EXISTS result (run TEXT NOT NULL, suite TEXT NOT NULL, problem TEXT NOT NULL, model TEXT NOT NULL, instance TEXT NOT NULL, problem_type TEXT NOT NULL CONSTRAINT result_problem_type_constraint CHECK (problem_type IN ("MIN", "MAX", "SAT")), optimum INT, high_score INT, solved INT NOT NULL CONSTRAINT result_solved_constraint CHECK (solved in (0, 1)), violation INT CONSTRAINT result_violation_constraint CHECK (violation >= 0), quality INT, runtime_in_seconds DOUBLE CONSTRAINT result_runtime_in_seconds_constraint CHECK (runtime_in_seconds >= 0), moves_per_second DOUBLE CONSTRAINT result_moves_per_second_constraint CHECK (moves_per_second >= 0), number_of_variables INT NOT NULL CONSTRAINT result_number_of_variables_constraint CHECK (number_of_variables >= 0), number_of_constraints INT NOT NULL CONSTRAINT result_number_of_constraints_constraint CHECK (number_of_constraints >= 0), CONSTRAINT result_unique_constraint UNIQUE (run, problem, model, instance) ON CONFLICT IGNORE)')
    cursor.execute('CREATE INDEX IF NOT EXISTS result_index ON result(run, problem, model, instance)')

def importResults(run, file, cursor):
    results = json.load(file)
    solverStatistics = results.get('solver-statistics')
    cursor.execute(
        'INSERT INTO result VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
        (run,
         results['task']['suite'],
         results['task']['problem'],
         results['task']['model'],
         results['task']['instance'],
         results['task']['problem-type'],
         results['task'].get('optimum'),
         results['task'].get('high-score'),
         results['result']['solved'],
         results['result'].get('violation'),
         results['result'].get('quality'),
         solverStatistics['runtime-in-seconds'] if solverStatistics else None,
         solverStatistics['moves-per-second'] if solverStatistics else None,
         results['yuck-model-statistics']['number-of-search-variables'] + results['yuck-model-statistics']['number-of-channel-variables'],
         results['yuck-model-statistics']['number-of-constraints']))

def main():
    parser = argparse.ArgumentParser(description = 'Puts Yuck integration test results into database')
    parser.add_argument('run', metavar = 'run')
    parser.add_argument('filenames', metavar = 'json-result-file', nargs = '+')
    args = parser.parse_args()
    with sqlite3.connect("results.db") as conn:
        cursor = conn.cursor()
        createDb(cursor)
        cursor.execute('PRAGMA foreign_keys = ON');
        for filename in args.filenames:
            with open(filename) as file:
                importResults(args.run, file, cursor)
                conn.commit()

main()
