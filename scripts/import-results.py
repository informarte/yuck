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
import sqlite3
from urllib.request import pathname2url


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
        'flatzinc_model_md5sum TEXT, '\
        'parser_runtime_in_seconds DOUBLE CONSTRAINT result_parser_runtime_in_seconds_constraint CHECK (parser_runtime_in_seconds >= 0), '\
        'compiler_runtime_in_seconds DOUBLE CONSTRAINT result_compiler_runtime_in_seconds_constraint CHECK (compiler_runtime_in_seconds >= 0), '\
        'domain_initializer_runtime_in_seconds DOUBLE CONSTRAINT result_domain_initializer_runtime_in_seconds_constraint CHECK (domain_initializer_runtime_in_seconds >= 0), '\
        'variable_factory_runtime_in_seconds DOUBLE CONSTRAINT result_variable_factory_runtime_in_seconds_constraint CHECK (variable_factory_runtime_in_seconds >= 0), '\
        'variable_classifier_runtime_in_seconds DOUBLE CONSTRAINT result_variable_classifier_runtime_in_seconds_constraint CHECK (variable_classifier_runtime_in_seconds >= 0), '\
        'constraint_factory_runtime_in_seconds DOUBLE CONSTRAINT result_constraint_factory_runtime_in_seconds_constraint CHECK (constraint_factory_runtime_in_seconds >= 0), '\
        'objective_factory_runtime_in_seconds DOUBLE CONSTRAINT result_objective_factory_runtime_in_seconds_constraint CHECK (objective_factory_runtime_in_seconds >= 0), '\
        'presolver_runtime_in_seconds DOUBLE CONSTRAINT result_presolver_runtime_in_seconds_constraint CHECK (presolver_runtime_in_seconds >= 0), '\
        'neighbourhood_factory_runtime_in_seconds DOUBLE CONSTRAINT result_neighbourhood_factory_runtime_in_seconds_constraint CHECK (neighbourhood_factory_runtime_in_seconds >= 0), '\
        'constraint_network_pruner_runtime_in_seconds DOUBLE CONSTRAINT result_constraint_network_pruner_runtime_in_seconds_constraint CHECK (constraint_network_pruner_runtime_in_seconds >= 0), '\
        'array_access_optimizer_runtime_in_seconds DOUBLE CONSTRAINT result_array_access_optimizer_runtime_in_seconds_constraint CHECK (array_access_optimizer_runtime_in_seconds >= 0), '\
        'warm_start_annotation_parser_runtime DOUBLE CONSTRAINT result_warm_start_annotation_parser_runtime_constraint CHECK (warm_start_annotation_parser_runtime >= 0), '\
        'number_of_variables INT CONSTRAINT result_number_of_variables_constraint CHECK (number_of_variables >= 0), '\
        'number_of_search_variables INT CONSTRAINT result_number_of_search_variables_constraint CHECK (number_of_search_variables >= 0), '\
        'number_of_implicitly_constrained_search_variables INT CONSTRAINT result_number_of_implicitly_constrained_search_variables_constraint CHECK (number_of_implicitly_constrained_search_variables >= 0), '\
        'number_of_channel_variables INT CONSTRAINT result_number_of_channel_variables_constraint CHECK (number_of_channel_variables >= 0), '\
        'number_of_constraints INT CONSTRAINT result_number_of_constraints_constraint CHECK (number_of_constraints >= 0), '\
        'number_of_implicit_constraints INT CONSTRAINT result_number_of_implicit_constraints_constraint CHECK (number_of_implicit_constraints >= 0), '\
        'number_of_layers INT CONSTRAINT result_number_of_layers_constraint CHECK (number_of_layers >= 0), '\
        'search_runtime_to_first_solution_in_seconds DOUBLE CONSTRAINT result_search_runtime_to_first_solution_in_seconds_constraint CHECK (search_runtime_to_first_solution_in_seconds >= 0), '\
        'search_runtime_to_best_solution_in_seconds DOUBLE CONSTRAINT result_search_runtime_to_best_solution_in_seconds_constraint CHECK (search_runtime_to_best_solution_in_seconds >= 0), '\
        'search_runtime_in_seconds DOUBLE CONSTRAINT result_search_runtime_in_seconds_constraint CHECK (search_runtime_in_seconds >= 0), '\
        'moves_per_second DOUBLE CONSTRAINT result_moves_per_second_constraint CHECK (moves_per_second >= 0), '\
        'consultations_per_move DOUBLE CONSTRAINT result_consultations_per_move_constraint CHECK (consultations_per_move >= 0), '\
        'commitments_per_move DOUBLE CONSTRAINT result_commitments_per_move_constraint CHECK (commitments_per_move >= 0), '\
        'number_of_perturbations DOUBLE CONSTRAINT result_number_of_perturbations_constraint CHECK (number_of_perturbations >= 0), '\
        'area DOUBLE CONSTRAINT result_area_constraint CHECK (area >= 0), '\
        'solved INT NOT NULL CONSTRAINT result_solved_constraint CHECK (solved in (0, 1)), '\
        'violation INT CONSTRAINT result_violation_constraint CHECK (violation >= 0), '\
        'objective_value INT, '\
        'CONSTRAINT result_unique_constraint UNIQUE (run, solver, solver_version, problem, model, instance) ON CONFLICT IGNORE)')
    cursor.execute('CREATE INDEX IF NOT EXISTS result_index ON result(run, solver, solver_version, problem, model, instance)')

def importResults(args, file, cursor):
    data = json.load(file)
    task = data.get('task')
    flatZincModelMetrics = data.get('flatzinc-model-metrics', data.get('flatzinc-model-statistics'))
    yuckModelMetrics = data.get('yuck-model-metrics', data.get('yuck-model-statistics'))
    result = data.get('result')
    solver = data.get('solver')
    parserMetrics = data.get('parser-metrics', data.get('parser-statistics'))
    compilerMetrics = data.get('compiler-metrics', data.get('compiler-statistics'))
    searchMetrics = data.get('search-metrics', data.get('search-statistics', data.get('solver-statistics')))
    if not task:
         print("No task (MiniZinc compiler error?)")
    elif 'env' in data and 'yuck' in data['env'] and not yuckModelMetrics:
        print("No model metrics (FlatZinc compiler error?)")
    else:
        cursor.execute(
            'INSERT INTO result VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
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
             flatZincModelMetrics.get('md5sum') if flatZincModelMetrics else None,
             parserMetrics['runtime-in-seconds'] if parserMetrics else None,
             compilerMetrics['runtime-in-seconds'] if compilerMetrics else None,
             compilerMetrics.get('domain-initializer-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('variable-factory-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('variable-classifier-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('constraint-factory-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('objective-factory-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('presolver-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('neighbourhood-factory-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('constraint-network-pruner-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('array-access-optimizer-runtime-in-seconds') if compilerMetrics else None,
             compilerMetrics.get('warm-start-annotation-parser-runtime') if compilerMetrics else None,
             yuckModelMetrics['number-of-search-variables'] + yuckModelMetrics['number-of-channel-variables'] if yuckModelMetrics else None,
             yuckModelMetrics['number-of-search-variables'] if yuckModelMetrics else None,
             yuckModelMetrics['number-of-implicitly-constrained-search-variables'] if yuckModelMetrics else None,
             yuckModelMetrics['number-of-channel-variables'] if yuckModelMetrics else None,
             yuckModelMetrics['number-of-constraints'] if yuckModelMetrics else None,
             yuckModelMetrics['number-of-implicit-constraints'] if yuckModelMetrics else None,
             yuckModelMetrics.get('number-of-layers') if yuckModelMetrics else None,
             searchMetrics.get('runtime-to-first-solution-in-seconds') if searchMetrics else None,
             searchMetrics.get('runtime-to-best-solution-in-seconds') if searchMetrics else None,
             searchMetrics.get('runtime-in-seconds') if searchMetrics else None,
             searchMetrics.get('moves-per-second') if searchMetrics else None,
             searchMetrics.get('consultations-per-move') if searchMetrics else None,
             searchMetrics.get('commitments-per-move') if searchMetrics else None,
             searchMetrics.get('number-of-perturbations') if searchMetrics else None,
             searchMetrics.get('area') if searchMetrics else None,
             result['solved'] if result and 'solved' in result else False,
             result.get('violation') if result else None,
             result.get('objective-value', result.get('quality')) if result else None))

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
