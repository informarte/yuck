import matplotlib.pyplot as plt
import numpy
import statistics

# result: task -> value
def analyzeResult(result, bins = 10, range = None):
    values = [result[task] for task in result]
    if values:
        quartiles = numpy.percentile(values, [25, 50, 75])
        q1 = quartiles[0]
        q2 = quartiles[1]
        q3 = quartiles[2]
        iqr = q3 - q1
        return {
            'min': min(values),
            'q1': q1,
            'q2': q2,
            'q3': q3,
            'max': max(values),
            'mean': statistics.mean(values),
            'pstdev': statistics.pstdev(values),
            'histogram': numpy.histogram(values, 'auto')[0].tolist(),
            'extreme-values': {
                task[0] + "/" + task[1]: result[task]
                for task in result
                if result[task] < q1 - 1.5 * iqr or result[task] > q3 + 1.5 * iqr
            }
        }
    else:
        return {
            'count': 0
        }

# getValues: run -> [value]
def plotDiagrams(runs, getValues, title, xlabel, legendLocation):
    fig, (ax1, ax2, ax3) = plt.subplots(ncols = 1, nrows = 3, sharex = True, constrained_layout=True)
    fig.suptitle(title, fontsize = 'x-large')
    reverselySortedRuns = sorted(runs, reverse = True)
    values = [getValues(run) for run in reverselySortedRuns]
    ax1.hist(values, bins = 25, histtype = 'step', cumulative = False, label = reverselySortedRuns)
    ax2.hist(values, bins = 100, histtype = 'step', cumulative = True, label = reverselySortedRuns)
    ax3.boxplot([getValues(run) for run in reverselySortedRuns], vert = False, labels = reverselySortedRuns, showmeans = True)
    ax1.grid(True)
    ax1.legend(loc=legendLocation, fontsize = 'medium')
    ax1.set_ylabel('Number of results')
    ax2.grid(True)
    ax2.set_ylabel('Cumulative number of results')
    ax3.grid(True)
    ax3.set_xlabel(xlabel)
    plt.show()
