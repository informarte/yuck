#! /usr/bin/python3

# This script creates a database of Yuck integration test results.
#
# To import results, call the script with a list of JSON result files on the command line.
# When the database already exists, the given results will be added unless they are
# already in the database.
#
# The result database is created in the working directory under the name results.db
# unless another name is specified by way of the --db option.


import argparse
import json
from urllib.request import pathname2url
import sqlite3
from itertools import repeat

def createDb(cursor):
    cursor.execute('CREATE TABLE IF NOT EXISTS result (run TEXT NOT NULL, suite TEXT NOT NULL, problem TEXT NOT NULL, model TEXT NOT NULL, instance TEXT NOT NULL, problem_type TEXT NOT NULL CONSTRAINT result_problem_type_constraint CHECK (problem_type IN ("MIN", "MAX", "SAT")), optimum INT, high_score INT, solved INT NOT NULL CONSTRAINT result_solved_constraint CHECK (solved in (0, 1)), violation INT CONSTRAINT result_violation_constraint CHECK (violation >= 0), quality INT, runtime_in_seconds DOUBLE CONSTRAINT result_runtime_in_seconds_constraint CHECK (runtime_in_seconds >= 0), moves_per_second DOUBLE CONSTRAINT result_moves_per_second_constraint CHECK (moves_per_second >= 0), number_of_variables INT CONSTRAINT result_number_of_variables_constraint CHECK (number_of_variables >= 0), number_of_constraints INT CONSTRAINT result_number_of_constraints_constraint CHECK (number_of_constraints >= 0), CONSTRAINT result_unique_constraint UNIQUE (run, problem, model, instance) ON CONFLICT IGNORE)')
    cursor.execute('CREATE INDEX IF NOT EXISTS result_index ON result(run, problem, model, instance)')

def importResults(run, file, cursor):
    data = json.load(file)
    task = data.get('task')
    modelStatistics = data.get('yuck-model-statistics')
    result = data.get('result')
    solverStatistics = data.get('solver-statistics')
    if not task:
         print("No task (MiniZinc compiler error?)")
    elif not modelStatistics:
        print("No model statistics (FlatZinc compiler error?)")
    else:
        cursor.execute(
            'INSERT INTO result VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
            (run,
             task['suite'],
             task['problem'],
             task['model'],
             task['instance'],
             task['problem-type'],
             task.get('optimum'),
             task.get('high-score'),
             result['solved'] if result else False,
             result.get('violation') if result else None,
             result.get('quality') if result else None,
             solverStatistics['runtime-in-seconds'] if solverStatistics else None,
             solverStatistics['moves-per-second'] if solverStatistics else None,
             modelStatistics['number-of-search-variables'] + modelStatistics['number-of-channel-variables'],
             modelStatistics['number-of-constraints']))

def main():
    parser = argparse.ArgumentParser(
        description = 'Puts Yuck integration test results into a database',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('run', metavar = 'run')
    parser.add_argument('filenames', metavar = 'json-result-file', nargs = '+')
    args = parser.parse_args()
    dburi = 'file:{}?mode=rwc'.format(pathname2url(args.database))
    with sqlite3.connect(dburi, uri = True) as conn:
        cursor = conn.cursor()
        createDb(cursor)
        cursor.execute('PRAGMA foreign_keys = ON');
        for filename in args.filenames:
            print("Importing ", filename)
            with open(filename) as file:
                importResults(args.run, file, cursor)
                conn.commit()

main()
