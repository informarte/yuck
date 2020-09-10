#! /usr/bin/python3

# This script translates VRP instance data from TSPLIB format to DataZinc.
#
# It was tested on instances from http://akira.ruc.dk/~keld/research/LKH-3/
# and supports some keywords introduced by LKH-3.

import argparse
import math
import re
import sys

def readProblem(source):
    problem = {}
    lines = source.readlines()
    propertyPattern = re.compile('(.*?):(.*)')
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        match = propertyPattern.match(line)
        if match:
            key = match.group(1).strip()
            value = match.group(2).strip()
            if key == 'NAME':
                problem['name'] = value
                namePattern = re.compile('.*-k(\\d+)')
                match = namePattern.match(value)
                if match:
                    problem['k'] = int(match.group(1))
            elif key == 'CAPACITY':
                problem['capacity'] = int(value)
            elif key == 'COMMENT':
                problem['comment'] = value
            elif key == 'DIMENSION':
                problem['n'] = int(value)
            elif key == 'EDGE_WEIGHT_FORMAT':
                if value != 'FULL_MATRIX':
                    raise ValueError('Unsupported edge-weight format {}'.format(key))
            elif key == 'EDGE_WEIGHT_TYPE':
                if not value in ['EUC_2D', 'EXACT_2D', 'EXPLICIT', 'FLOOR_2D']:
                    raise ValueError('Unsupported edge-weight type {}'.format(value))
                problem['edge-weight-type'] = value
                if value == 'EXACT_2D':
                    problem['scale'] = 1000
            elif key == 'SCALE':
                problem['scale'] = int(value)
            elif key == 'SERVICE_TIME':
                problem['service-time'] = int(value)
            elif key == 'TYPE':
                if not value in ['CVRP', 'TSPTW', 'CVRPTW']:
                    raise ValueError('Unsupported problem type {}'.format(value))
                problem['type'] = value
            elif key == 'VEHICLES':
                problem['k'] = int(value)
            else:
                raise ValueError('Unsupported key {}'.format(key))
        elif line == 'DEPOT_SECTION':
            depots = []
            while True:
                i += 1
                depot = int(lines[i])
                if depot == -1:
                    break
                else:
                    depots.append(depot)
            if len(depots) != 1:
                raise ValueError('Unsupported number of depots')
            if depots[0] != 1:
                raise ValueError('Unexpected depot index')
            problem['depots'] = depots
        elif line == 'DEMAND_SECTION':
            if not 'n' in problem:
                raise ValueError('Dimension is unknown')
            demands = []
            for j in range(problem['n']):
                i += 1
                k, demand = [int(token) for token in lines[i].split()]
                if k != j + 1:
                    raise ValueError('Malformed demand section')
                demands.append(demand)
            if len(demands) != problem['n']:
                raise ValueError('Not the right number of demands')
            if demands[0] != 0:
                raise ValueError('Depot has non-zero demand')
            problem['demands'] = demands
        elif line == 'EDGE_WEIGHT_SECTION':
            if not 'n' in problem:
                raise ValueError('Dimension is unknown')
            edgeWeights = []
            for j in range(problem['n']):
                i += 1
                tmp = [int(token) for token in lines[i].split()]
                if len(tmp) != problem['n']:
                    raise ValueError('Malformed edge-weight matrix')
                edgeWeights.append(tmp)
            problem['edge-weights'] = edgeWeights
        elif line == 'EOF':
            break
        elif line == 'NODE_COORD_SECTION':
            if not 'n' in problem:
                raise ValueError('Dimension is unknown')
            coordinates = []
            for j in range(problem['n']):
                i += 1
                k, x, y = [int(token) for token in lines[i].split()]
                if k != j + 1:
                    raise ValueError('Malformed coordinates section')
                coordinates.append((x, y))
            problem['coordinates'] = coordinates
        elif line == 'TIME_WINDOW_SECTION':
            if not 'n' in problem:
                raise ValueError('Dimension is unknown')
            timeWindows = []
            for j in range(problem['n']):
                i += 1
                k, start, end = [int(token) for token in lines[i].split()]
                if k != j + 1:
                    raise ValueError('Malformed time-window section')
                timeWindows.append([start, end])
            problem['time-windows'] = timeWindows
        else:
            raise ValueError('Unsupported keyword {}'.format(line))
        i += 1
    return problem

def writeMatrix(matrix, sink):
    m = len(matrix)
    n = len(matrix[0])
    colWidths = []
    for j in range(n):
        colWidth = 0
        for row in matrix:
            colWidth = max(colWidth, len(str(row[j])))
        colWidths += [colWidth]
    for i in range(m):
        row = matrix[i]
        formattedRow = [' ' * (colWidths[j] - len(str(row[j]))) + str(row[j]) for j in range(n)]
        sink.write('  ')
        sink.write(', '.join(formattedRow))
        sink.write(' |\n' if i < m - 1 else '\n')

def writeProblem(args, problem, sink):
    n = problem['n']
    sink.write('N = {};\n'.format(n - 1))
    if problem['type'] in ['CVRP', 'CVRPTW']:
        sink.write('MinK = {};\n'.format(problem['k'] if 'k' in problem else 1))
        sink.write('Capacity = {};\n'.format(problem['capacity']))
    if problem['edge-weight-type'] in ['EUC_2D', 'EXACT_2D', 'FLOOR_2D']:
        sink.write('\n')
        sink.write('Scale = {};\n'.format(problem['scale'] if 'scale' in problem else 1))
        sink.write('DistanceRoundingMode = {};\n'.format(
            'Floor' if problem['edge-weight-type'] == 'FLOOR_2D' else 'NearestInteger'))
        sink.write('X = array1d(0..N, {});\n'.format([x for (x, y) in problem['coordinates']]))
        sink.write('Y = array1d(0..N, {});\n'.format([y for (x, y) in problem['coordinates']]))
    elif problem['edge-weight-type'] == 'EXPLICIT':
        sink.write('\n')
        sink.write('TravelTimes = array2d(0..N, 0..N, [|\n')
        writeMatrix(problem['edge-weights'], sink)
        sink.write('|]);\n')
    if problem.get('demands'):
        sink.write('\n')
        sink.write('Demands = array1d(1..N, {});\n'.format(problem['demands'][1:]))
    if problem.get('service-time'):
        sink.write('\n')
        sink.write('ServiceTimes = array1d(1..N, {});\n'.format([problem['service-time'] for i in range(n - 1)]))
    if problem.get('time-windows'):
        sink.write('\n')
        sink.write('TimeWindows = array2d(1..2, 0..N, [|\n')
        writeMatrix([[problem['time-windows'][i][j] for i in range(n)] for j in range(2)], sink)
        sink.write('|]);\n')

def main():
    parser = argparse.ArgumentParser(
        description = 'Translates CVRP instance data from TSPLIB format to DataZinc',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('filenames', metavar = 'original-instance-data-file', nargs = 1)
    args = parser.parse_args()
    pattern = re.compile('(.*)\.(.*)')
    match = pattern.match(args.filenames[0])
    stem = match.group(1)
    with open(args.filenames[0], 'r') as source:
        try:
            problem = readProblem(source)
            with open(stem + '.dzn', 'w') as sink:
                writeProblem(args, problem, sink)
        except ValueError as e:
            print(e, file = sys.stderr)

main()
