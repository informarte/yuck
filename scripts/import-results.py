#! /usr/bin/python3

# This script puts integration test results into a database.
#
# To import results, call the script with a list of JSON result files on the command line.
# When the database already exists, the given results will be added unless they are
# already in the database.
#
# The result database is created in the working directory under the name results.db
# unless another name is specified by way of the --db option.
#
# The --model-override option addresses the following use case:
# When we study a particular problem, we want to compare different models or different
# solvers which require specific models. However, as all the downstream scripts distinguish
# results by the key (problem, model, instance), results with different models are not
# comparable. To obtain comparable results, organize results for different models or
# from different solvers into separate runs and use --model-override to replace the models
# from the result files with the given model.

import argparse
import json
from urllib.request import pathname2url
import sqlite3
from itertools import repeat

def createDb(cursor):
    cursor.execute(
        'CREATE TABLE IF NOT EXISTS result ('\
        'run TEXT NOT NULL, '\
        'solver TEXT, '\
        'solver_version TEXT, '\
        'suite TEXT NOT NULL, '\
        'problem TEXT NOT NULL, '\
        'model TEXT NOT NULL, '\
        'instance TEXT NOT NULL, '\
        'problem_type TEXT NOT NULL CONSTRAINT result_problem_type_constraint CHECK (problem_type IN ("MIN", "MAX", "SAT")), '\
        'optimum INT, '\
        'high_score INT, '\
        'solved INT NOT NULL CONSTRAINT result_solved_constraint CHECK (solved in (0, 1)), '\
        'violation INT CONSTRAINT result_violation_constraint CHECK (violation >= 0), '\
        'quality INT, '\
        'number_of_variables INT CONSTRAINT result_number_of_variables_constraint CHECK (number_of_variables >= 0), '\
        'number_of_constraints INT CONSTRAINT result_number_of_constraints_constraint CHECK (number_of_constraints >= 0), '\
        'area DOUBLE CONSTRAINT result_area_constraint CHECK (area >= 0), '\
        'solving_time_in_seconds DOUBLE CONSTRAINT result_solving_time_in_seconds_constraint CHECK (solving_time_in_seconds >= 0), '\
        'runtime_to_best_solution_in_seconds DOUBLE CONSTRAINT result_runtime_to_best_solution_in_seconds_constraint CHECK (runtime_in_seconds >= 0), '\
        'runtime_in_seconds DOUBLE CONSTRAINT result_runtime_in_seconds_constraint CHECK (runtime_in_seconds >= 0), '\
        'moves_per_second DOUBLE CONSTRAINT result_moves_per_second_constraint CHECK (moves_per_second >= 0), '\
        'consultations_per_move DOUBLE CONSTRAINT result_consultations_per_move_constraint CHECK (consultations_per_move >= 0), '\
        'commitments_per_move DOUBLE CONSTRAINT result_commitments_per_move_constraint CHECK (commitments_per_move >= 0), '\
        'CONSTRAINT result_unique_constraint UNIQUE (run, solver, solver_version, problem, model, instance) ON CONFLICT IGNORE)')
    cursor.execute('CREATE INDEX IF NOT EXISTS result_index ON result(run, solver, solver_version, problem, model, instance)')

def importResults(args, file, cursor):
    data = json.load(file)
    task = data.get('task')
    modelStatistics = data.get('yuck-model-statistics')
    result = data.get('result')
    solver = data.get('solver')
    solverStatistics = data.get('solver-statistics')
    if not task:
         print("No task (MiniZinc compiler error?)")
    elif data['env'].get('yuck') and not modelStatistics:
        print("No model statistics (FlatZinc compiler error?)")
    else:
        cursor.execute(
            'INSERT INTO result VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
            (args.run,
             solver['name'] if solver else None,
             solver['version'] if solver else None,
             task['suite'],
             task['problem'],
             args.modelOverride if args.modelOverride else task['model'],
             task['instance'],
             task['problem-type'],
             task.get('optimum'),
             task.get('high-score'),
             result['solved'] if result and 'solved' in result else False,
             result.get('violation') if result else None,
             result.get('quality') if result else None,
             modelStatistics['number-of-search-variables'] + modelStatistics['number-of-channel-variables'] if modelStatistics else None,
             modelStatistics['number-of-constraints'] if modelStatistics else None,
             solverStatistics.get('area') if solverStatistics else None,
             solverStatistics.get('solving-time-in-seconds') if solverStatistics else None,
             solverStatistics.get('runtime-to-best-solution-in-seconds') if solverStatistics else None,
             solverStatistics.get('runtime-in-seconds') if solverStatistics else None,
             solverStatistics.get('moves-per-second') if solverStatistics else None,
             solverStatistics.get('consultations-per-move') if solverStatistics else None,
             solverStatistics.get('commitments-per-move') if solverStatistics else None))

def main():
    parser = argparse.ArgumentParser(
        description = 'Puts integration test results into a database',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--db', '--database', dest = 'database', default = 'results.db', help = 'Define results database')
    parser.add_argument('--model-override', dest = 'modelOverride', default = '', help = 'Replace the models from the result files with the given model')
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
                importResults(args, file, cursor)
                conn.commit()

main()
