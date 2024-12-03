#! /usr/bin/python3

# This script visualizes space performance metrics.

import argparse
import json
import matplotlib.pyplot as plt
import sys

def plot(args, results):
    byConstraint = results['by-constraint']
    consultationOverhead = results['consultation-effort-in-seconds'] - sum([cpm['consultation-effort-in-seconds'] for cpm in byConstraint.values()])
    consultationOverheadPerCall = consultationOverhead / sum([cpm['number-of-consultations'] for cpm in byConstraint.values()])
    commitmentOverhead = results['commitment-effort-in-seconds'] - sum([cpm['commitment-effort-in-seconds'] for cpm in byConstraint.values()])
    commitmentOverheadPerCall = commitmentOverhead / sum([cpm['number-of-commitments'] for cpm in byConstraint.values()])
    if args.subcommand in ['by-constraint', 'drill']:
        if args.subcommand == 'drill':
            byConstraint = results['by-goal'][args.goal]
        categories = [constraint for constraint in byConstraint]
        consultationEfforts = [cpm['consultation-effort-in-seconds'] for cpm in byConstraint.values()]
        consultationOverheads = [cpm['number-of-consultations'] * consultationOverheadPerCall for cpm in byConstraint.values()]
        commitmentEfforts = [cpm['commitment-effort-in-seconds'] for cpm in byConstraint.values()]
        commitmentOverheads = [cpm['number-of-commitments'] * commitmentOverheadPerCall for cpm in byConstraint.values()]
    else:
        byGoal = results['by-goal']
        categories = [goal for goal in byGoal]
        consultationEfforts = [sum([cpm['consultation-effort-in-seconds'] for cpm in gpm.values()]) for gpm in byGoal.values()]
        consultationOverheads = [sum([cpm['number-of-consultations'] for cpm in gpm.values()]) * consultationOverheadPerCall for gpm in byGoal.values()]
        commitmentEfforts = [sum([cpm['commitment-effort-in-seconds'] for cpm in gpm.values()]) for gpm in byGoal.values()]
        commitmentOverheads = [sum([cpm['number-of-commitments'] for cpm in gpm.values()]) * commitmentOverheadPerCall for gpm in byGoal.values()]
    efforts = [sum(t) for t in zip(consultationEfforts, consultationOverheads, commitmentEfforts, commitmentOverheads)]
    efforts, categories, consultationEfforts, consultationOverheads, commitmentEfforts, commitmentOverheads = \
        map(list, zip(*sorted(zip(efforts, categories, consultationEfforts, consultationOverheads, commitmentEfforts, commitmentOverheads), reverse = True))) # weird but works
    totalEffort = sum(efforts)
    consultationEfforts = [t / totalEffort * 100 for t in consultationEfforts]
    consultationOverheads = [t / totalEffort * 100 for t in consultationOverheads]
    commitmentEfforts = [t / totalEffort * 100 for t in commitmentEfforts]
    commitmentOverheads = [t / totalEffort * 100 for t in commitmentOverheads]
    p1 = plt.bar(categories, consultationEfforts)
    p2 = plt.bar(categories, commitmentEfforts, bottom = consultationEfforts)
    p3 = plt.bar(categories, consultationOverheads, bottom = [sum(t) for t in zip(consultationEfforts, commitmentEfforts)])
    p4 = plt.bar(categories, commitmentOverheads, bottom = [sum(t) for t in zip(consultationEfforts, commitmentEfforts, consultationOverheads)])
    plt.ylabel('% of {} seconds'.format(round(totalEffort, 2)))
    if args.subcommand == 'drill':
        plt.title('Performance metrics ({})'.format(args.goal))
    else:
        plt.title('Performance metrics')
    plt.xticks(categories, categories, rotation = 90)
    plt.subplots_adjust(bottom = 0.25) # make room for constraint names
    plt.legend((p1[0], p2[0], p3[0], p4[0]), ('consult', 'commit', 'consult overhead', 'commit overhead'))
    plt.gcf().canvas.manager.set_window_title(args.filename)
    plt.show()

def main():
    parser = argparse.ArgumentParser(
        description = 'Visualizes performance metrics',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    subparsers = parser.add_subparsers(title='Subcommands', dest='subcommand')
    subparsers.add_parser('by-constraint', help = 'Visualize efforts by constraint')
    subparsers.add_parser('by-goal', help = 'Visualize efforts by goal')
    drillParser = subparsers.add_parser('drill', help = 'Visualize efforts by constraints involved in the given goal')
    drillParser.add_argument('goal', metavar = 'goal')
    parser.add_argument('filename', metavar = 'json-result-file')
    args = parser.parse_args()
    with open(args.filename) as file:
        data = json.load(file)
        if not 'space-performance-metrics' in data:
            print("Error: No performance metrics, run Yuck with --constraint-profiling or --goal-profiling", file = sys.stderr)
            exit(1)
        results = data['space-performance-metrics']
        if not 'by-constraint' in results:
            print("Error: No constraint performance metrics", file = sys.stderr)
            exit(1)
        if args.subcommand in ['by-goal', 'drill']:
            if not 'by-goal' in results:
                print("Error: No goal performance metrics, run Yuck with --goal-profiling", file = sys.stderr)
                exit(1)
            if not results['by-goal']:
                print("Error: No goal performance metrics, add goal annotations to your model", file = sys.stderr)
                exit(1)
            if args.subcommand == 'drill' and not args.goal in results['by-goal']:
                print("Error: No performance metrics for given goal", file = sys.stderr)
                exit(1)
        plot(args, results)

main()
