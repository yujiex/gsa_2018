import pandas as pd
import os
import glob
import pylab as P
import matplotlib.pyplot as plt
import seaborn as sns

def fill_between_lines(x, y1, y2, c1, c2, output_filename, xtickplace=None, xticklabel=None, xlimit=None):
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    ax = plt.axes()
    line1, = ax.plot(x, y1, c=c1, ls='-', lw=2, marker='o')
    line2, = ax.plot(x, y2, c=c2, ls='-', lw=2, marker='o')
    ax.fill_between(x, y1, y2, where=y1 <= y2,
                    facecolor='lime', alpha=0.5, interpolate=True)
    ax.fill_between(x, y1, y2, where=y1 > y2, facecolor='red',
                    alpha=0.5, interpolate=True)
    ax.set_ylabel("kBtu per square foot per month", fontsize=10)
    # ax.legend([line1, line2],
    #           ['Actual {1} use in {0}'.format(time_label(timerange_post), lb.title_dict[theme]),
    #            '\n'.join(tw.wrap('{1} use given {2} habits but {0} weather'.format(time_label(timerange_post),
    #                                                                                lb.title_dict[theme],
    #                                                                                time_label(timerange_pre)),
    #                              wrapwidth))], loc=location)
                                # following is for region 9 ppt one building
                                # wrapwidth))], loc=location, fontsize='xx-small')
    ax.figure.set_size_inches(8, 4)
    if (not(xtickplace is None)):
        plt.xticks(xtickplace, xticklabel)
    if (not (xlimit is None)):
        plt.xlim(xlimit)
    plt.ylim((0, max(max(y1), max(y2)) * 1.1))
    # ax.grid(linewidth=0.5)
    # plt.show()
    P.savefig(os.getcwd() + '/images/fill_between/{}'.format(output_filename), dpi = 150)
    plt.close()

def saving_fill_plot_bymonth():
    xtickplace = range(1, 13)
    xticklabel = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    files = glob.glob(os.getcwd() + "/to_python/*.csv")
    for f in files:
        print(f)
        df = pd.read_csv(f)
        filename = f[f.rfind("/") + 1:]
        print(filename)
        tokens = filename.split("_")
        building = tokens[0]
        plotType = tokens[1]
        if plotType == 'gas':
            c1 = 'brown'
            c2 = 'lightsalmon'
            # location = 'lower left'
            location = 'upper center'
            wrapwidth = 30
        elif plotType == "elec":
            c1 = 'navy'
            c2 = 'lightskyblue'
            # location = 'upper left'
            location = 'lower center'
            wrapwidth = 99
        fill_between_lines(df['month'], df['actual'], df['baseline'], c1, c2, filename.replace('.csv', '.png'),
                           xtickplace, xticklabel, xlimit=(1, 12))

saving_fill_plot_bymonth()
