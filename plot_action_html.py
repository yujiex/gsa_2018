import os
import glob
import pandas as pd
import numpy as np
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
import textwrap as tw
import datetime

import lean_temperature_monthly as ltm
import util
import label as lb
import util_seq as useq
import lean_dd as ld
import util_io as uo
import get_building_set as gbs

homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/' 
weatherdir = os.getcwd() + '/csv_FY/weather/'
outputdir = os.getcwd() + '/plot_FY_weather/html/'

def create_index():
    df_ecm = pd.read_csv(master_dir + 'ECM/EUAS_ecm.csv')
    df = df_ecm.copy()
    df = df[['high_level_ECM', 'detail_level_ECM']]
    df2 = df.groupby(['high_level_ECM', 'detail_level_ECM']).count()
    df2.drop('high_level_ECM', axis=1, inplace=True)
    df2.rename(columns={'detail_level_ECM': 'building count'},
               inplace=True)
    df2.reset_index(inplace=True)
    df2.replace({'detail_level_ECM': {'GSALink': ''}}, inplace=True)
    with open(outputdir + 'summary_ecm_count.html', 'w+') as wt:
        df2.to_html(wt, index=False, justify='left')
    with open(outputdir + 'summary_ecm_count.html', 'r') as rd:
        lines = rd.readlines()

    high_ecm = df['high_level_ECM'].unique()
    high_ecm = [x for x in high_ecm if not (type(x) == float and (np.isnan(x)))]
    # high_ecm.remove('GSALink')
    print high_ecm
    high_count = [len([x for x in lines if "{0}</td>".format(h) in x]) for h in high_ecm]
    count_dict = dict(zip(high_ecm, high_count))
    print count_dict
    for h in high_ecm:
        first = True
        for i in range(len(lines)):
            if ("{0}</td>".format(h) in lines[i]):
                if first:
                    lines[i] = lines[i].replace("<td>", "<td rowspan={0}>".format(count_dict[h]))
                    lines[i] = lines[i].replace("{0}</td>".format(h), "<a href={1}.html>{0}</a></td>".format(h, h.replace(" ", "_")))
                    first = False
                else:
                    lines[i] = "\n"
    with open(outputdir + 'summary_ecm_count_mergerow.html', 'w+') as wt:
        table = ''.join(lines)
        wt.write(table)
    with open(outputdir + 'index_template.html', 'r') as rd:
        rw_lines = rd.readlines()
    rw_lines = [x.replace("  <!-- insert table here -->", table) for x in rw_lines]
    with open(outputdir + 'index.html', 'w+') as wt:
        wt.write(''.join(rw_lines))
    with open(outputdir + 'template_action.html', 'r') as rd:
        lines = rd.readlines()
    df_ecm = df_ecm[['Building Number', 'high_level_ECM', 'Substantial Completion Date']]
    df_ecm.drop_duplicates(inplace=True)
    print high_ecm
    for h in high_ecm:
        newlines = [x.replace("action", h) for x in lines]
        print h
        i = useq.idx_substr(newlines, "<!-- insert list here -->")
        assert(i > -1)
        bd_str = process_high_level_links(df_ecm, h)
        newlines[i] = newlines[i].replace("<!-- insert list here -->",
                                          bd_str)
        with open(outputdir + '{0}.html'.format(h.replace(" ", "_")),
                  'w+') as wt:
            wt.write(''.join(newlines))
    return

def process_high_level_links(df_ecm, h):
    df = df_ecm.copy()
    df = df[df['high_level_ECM'] == h]
    buildings = df['Building Number'].tolist()
    buildings = sorted(buildings)
    building_str = '\n'.join(['<li><a href=single_building/{0}.html>{0}</a></li>'.format(x) for x in buildings])
    return building_str

# result_pre[0]: d_gas, result_pre[1]: d_elec
def plot_saving_fromdb(b, s, result_pre, result_post):
    print 'creating saving plot ...'
    pre_args = {}
    post_args = {}
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    plotlist = []
    d_save = {}
    d_cvrmse = {}
    if result_pre[0] == None or result_post[0] == None:
        print 'no gas data for saving plot'
        d_save['gas_percent'] = None
        d_save['gas_before'] = None
        d_save['gas_after'] = None
        d_cvrmse['gas'] = None
    else:
        plotlist.append('gas')
        d_gas_pre = result_pre[0]
        pre_args['timerange_pre'] = result_pre[-1]['timerange']
        pre_args['base_gas_pre'] = d_gas_pre['base_gas']
        pre_args['breakpoint_gas_pre'] = d_gas_pre['breakpoint']
        pre_args['df_gas_pre'] = d_gas_pre['df']
        d_gas_post = result_post[0]
        post_args['base_gas_post'] = d_gas_post['base_gas']
        post_args['timerange_post'] = result_post[-1]['timerange']
        post_args['breakpoint_gas_post'] = d_gas_post['breakpoint']
        d_gas_post['df']['eui_gas_hat'] = d_gas_pre['fun'](np.array(d_gas_post['df']['ave'].tolist()), *d_gas_pre['regression_par'])
        post_args['df_gas_post'] = d_gas_post['df']
    if result_pre[1] == None or result_post[1] == None:
        print 'no elec data for saving plot'
        d_save['elec_percent'] = None
        d_save['elec_before'] = None
        d_save['elec_after'] = None
        d_cvrmse['elec'] = None
    else:
        plotlist.append('elec')
        d_elec_pre = result_pre[1]
        pre_args['timerange_pre'] = result_pre[-1]['timerange']
        pre_args['base_elec_pre'] = d_elec_pre['base_elec']
        pre_args['breakpoint_elec_pre'] = d_elec_pre['breakpoint']
        d_elec_post = result_post[1]
        pre_args['df_elec_pre'] = d_elec_pre['df']
        post_args['base_elec_post'] = d_elec_pre['base_elec']
        post_args['timerange_post'] = result_post[-1]['timerange']
        post_args['breakpoint_elec_post'] = d_elec_post['breakpoint']
        d_elec_post['df']['eui_elec_hat'] = d_elec_pre['fun'](np.array(d_elec_post['df']['ave'].tolist()), *d_elec_pre['regression_par'])
        post_args['df_elec_post'] = d_elec_post['df']
    if len(plotlist) == 2:
        fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True,
                                         sharey=True)
        d_save['elec_percent'], d_save['elec_before'], d_save['elec_after'] = \
            plot_saving_aggyear(post_args['df_elec_post'],
                                pre_args['timerange_pre'],
                                post_args['timerange_post'],
                                'eui_elec', ax_1,
                                d_elec_pre['CV(RMSE)'])
        d_cvrmse['elec'] = round(d_elec_pre['CV(RMSE)'], 3)
        ax_1.set_xlim([1, 12])
        d_save['gas_percent'], d_save['gas_before'], d_save['gas_after'] = \
            plot_saving_aggyear(post_args['df_gas_post'],
                                pre_args['timerange_pre'],
                                post_args['timerange_post'],
                                'eui_gas', ax_2,
                                d_gas_pre['CV(RMSE)'])
        d_cvrmse['gas'] = round(d_gas_pre['CV(RMSE)'], 3)
        ax_2.set_xlim([1, 12])
    elif len(plotlist) == 1:
        if 'gas' in plotlist:
            ax = plt.axes()
            d_save['gas_percent'], d_save['gas_before'], d_save['gas_after'] = \
                plot_saving_aggyear(post_args['df_gas_post'],
                                    pre_args['timerange_pre'],
                                    post_args['timerange_post'],
                                    'eui_gas', ax,
                                    d_gas_pre['CV(RMSE)'])
            d_cvrmse['gas'] = d_gas_pre['CV(RMSE)']
            d_save['elec_percent'] = None
            d_save['elec_before'] = None
            d_save['elec_after'] = None
            d_cvrmse['elec'] = None
            ax.set_ylim([0, 7])
            ax.set_xlim([1, 12])
        elif 'elec' in plotlist:
            ax = plt.axes()
            d_save['elec_percent'], d_save['elec_before'], d_save['elec_after'] = \
                plot_saving_aggyear(post_args['df_elec_post'],
                                    pre_args['timerange_pre'],
                                    post_args['timerange_post'],
                                    'eui_elec', ax,
                                    d_elec_pre['CV(RMSE)'])
            d_cvrmse['elec'] = d_elec_pre['CV(RMSE)']
            d_save['gas_percent'] = None
            d_save['gas_before'] = None
            d_save['gas_after'] = None
            d_cvrmse['gas'] = None
            ax.set_ylim([0, 7])
            ax.set_xlim([1, 12])
    else:
        return d_save, d_cvrmse
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/savings/{0}_{1}_{2}_agg.png'.format(b, s, post_args['timerange_post']), dpi = 300)
    plt.close()
    return d_save, d_cvrmse

def plot_saving(b, s, result_pre, result_post):
    print 'creating saving plot ...'
    pre_args = {}
    post_args = {}
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    plotlist = []
    if result_pre[0] == None or result_post[0] == None:
        print 'no gas data for saving plot'
    else:
        plotlist.append('gas')
        d_gas_pre = result_pre[0]
        pre_args['timerange_pre'] = result_pre[-1]['timerange']
        pre_args['base_gas_pre'] = d_gas_pre['base_gas']
        pre_args['breakpoint_gas_pre'] = d_gas_pre['breakpoint']
        pre_args['df_gas_pre'] = d_gas_pre['df']
        d_gas_post = result_post[0]
        post_args['base_gas_post'] = d_gas_post['base_gas']
        post_args['timerange_post'] = result_post[-1]['timerange']
        post_args['breakpoint_gas_post'] = d_gas_post['breakpoint']
        d_gas_post['df']['eui_gas_hat'] = d_gas_pre['fun'](np.array(d_gas_post['df']['ave'].tolist()), *d_gas_pre['regression_par'])
        post_args['df_gas_post'] = d_gas_post['df']
    if result_pre[1] == None or result_post[1] == None:
        print 'no elec data for saving plot'
    else:
        plotlist.append('elec')
        d_elec_pre = result_pre[1]
        pre_args['timerange_pre'] = result_pre[-1]['timerange']
        pre_args['base_elec_pre'] = d_elec_pre['base_elec']
        pre_args['breakpoint_elec_pre'] = d_elec_pre['breakpoint']
        d_elec_post = result_post[1]
        pre_args['df_elec_pre'] = d_elec_pre['df']
        post_args['base_elec_post'] = d_elec_pre['base_elec']
        post_args['timerange_post'] = result_post[-1]['timerange']
        post_args['breakpoint_elec_post'] = d_elec_post['breakpoint']
        d_elec_post['df']['eui_elec_hat'] = d_elec_pre['fun'](np.array(d_elec_post['df']['ave'].tolist()), *d_elec_pre['regression_par'])
        post_args['df_elec_post'] = d_elec_post['df']
    d_save = {}
    d_cvrmse = {}
    if len(plotlist) == 2:
        fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True,
                                         sharey=True)
        d_save['elec'] = \
            plot_saving_aggyear(post_args['df_elec_post'],
                                pre_args['timerange_pre'],
                                post_args['timerange_post'],
                                'eui_elec', ax_1,
                                d_elec_pre['CV(RMSE)'])
        d_cvrmse['elec'] = round(d_elec_pre['CV(RMSE)'], 3)
        ax_1.set_xlim([1, 12])
        d_save['gas'] = plot_saving_aggyear(post_args['df_gas_post'],
                                            pre_args['timerange_pre'],
                                            post_args['timerange_post'],
                                            'eui_gas', ax_2,
                                            d_gas_pre['CV(RMSE)'])
        d_cvrmse['gas'] = round(d_gas_pre['CV(RMSE)'], 3)
        ax_2.set_xlim([1, 12])
    elif len(plotlist) == 1:
        if 'gas' in plotlist:
            ax = plt.axes()
            d_save['gas'] = \
                plot_saving_aggyear(post_args['df_gas_post'],
                                    pre_args['timerange_pre'],
                                    post_args['timerange_post'],
                                    'eui_gas', ax,
                                    d_gas_pre['CV(RMSE)'])
            d_cvrmse['gas'] = d_gas_pre['CV(RMSE)']
            d_save['elec'] = None
            d_cvrmse['elec'] = None
            ax.set_ylim([0, 7])
            ax.set_xlim([1, 12])
        elif 'elec' in plotlist:
            ax = plt.axes()
            d_save['elec'] = \
                plot_saving_aggyear(post_args['df_elec_post'],
                                    pre_args['timerange_pre'],
                                    post_args['timerange_post'],
                                    'eui_elec', ax,
                                    d_elec_pre['CV(RMSE)'])
            d_cvrmse['elec'] = d_elec_pre['CV(RMSE)']
            d_save['gas'] = None
            d_cvrmse['gas'] = None
            ax.set_ylim([0, 7])
            ax.set_xlim([1, 12])
    else:
        return None
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/savings/{0}_{1}_{2}_agg.png'.format(b, s, post_args['timerange_post']), dpi = 300)
    plt.close()
    return d_save, d_cvrmse

def plot_saving_aggyear(df, timerange_pre,
                        timerange_post, theme, ax, cvrmse):
    yearcol, timefilter = util.get_time_filter(timerange_post)
    df['in_range'] = df[yearcol].map(timefilter)
    df = df[df['in_range']]
    if theme == 'eui_gas':
        c1 = 'brown'
        c2 = 'lightsalmon'
        location = 'upper center'
        wrapwidth = 30
    else:
        c1 = 'navy'
        c2 = 'lightskyblue'
        location = 'lower center'
        wrapwidth = 99
    energy = df.groupby(['month']).mean()
    energy.reset_index(inplace=True)
    x = np.array(energy['month'])
    y = np.array(energy[theme])
    y_hat = np.array(energy[theme + '_hat'])
    save_percent = round((sum(y_hat) - sum(y)) / sum(y_hat) * 100, 1)
    after = sum(y)
    before = sum(y_hat)
    print save_percent, after, before, '################'
    def time_label(timerange):
        if 'before' in timerange or 'after' in timerange:
            spaceloc = timerange.find(' ')
            return timerange[spaceloc + 1:spaceloc + 5]
        else:
            spaceloc = timerange.rfind(' ')
            return "{} -- {}".format(timerange[:4], timerange[spaceloc + 1: spaceloc + 5])
    line1, = ax.plot(x, y, c=c1, ls='-', lw=2, marker='o')
    line2, = ax.plot(x, y_hat, c=c2, ls='-', lw=2, marker='o')
    ax.fill_between(x, y, y_hat, where=y_hat >= y,
                    facecolor='lime', alpha=0.5, interpolate=True)
    ax.fill_between(x, y, y_hat, where=y_hat < y, facecolor='red',
                    alpha=0.5, interpolate=True)
    ax.set_ylabel("kBtu per square foot per month", fontsize=10)
    ax.legend([line1, line2],
              ['Actual {1} use in {0}'.format(time_label(timerange_post), lb.title_dict[theme]),
               '\n'.join(tw.wrap('{1} use given {2} habits but {0} weather'.format(time_label(timerange_post),
                                                                                   lb.title_dict[theme],
                                                                                   time_label(timerange_pre)),
                                 wrapwidth))], loc=location)
    plt.xticks(range(1, 13), ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'])
    if save_percent > 0:
        ax.set_title('{2} after ({0}) vs before ({4}), {1}% less, CVRMSE: {3}'.format(time_label(timerange_post), abs(save_percent), lb.title_dict[theme], round(cvrmse, 2), time_label(timerange_pre)), fontsize=12)
    else:
        ax.set_title('{2} after ({0}) vs before ({4}), {1}% more, CVRMSE: {3}'.format(time_label(timerange_post), abs(save_percent), lb.title_dict[theme], round(cvrmse, 2), time_label(timerange_pre)), fontsize=12)
    ax.grid(linewidth=0.5)
    return save_percent, before, after

def plot_saving_year(df, year, pre_year, theme, ax, cvrmse):
    df = df[df['year'] == year]
    if theme == 'eui_gas':
        c1 = 'brown'
        c2 = 'lightsalmon'
        location = 'upper center'
        wrapwidth = 30
    else:
        c1 = 'navy'
        c2 = 'lightskyblue'
        location = 'lower center'
        wrapwidth = 99
    x = df['month']
    y = df[theme]
    y_hat = df[theme + '_hat']
    save_percent = round((sum(y_hat) - sum(y)) / sum(y_hat) * 100, 1)
    line1, = ax.plot(x, y, c=c1, ls='-', lw=2, marker='o')
    line2, = ax.plot(x, y_hat, c=c2, ls='-', lw=2, marker='o')
    ax.fill_between(x, y, y_hat, where=y_hat >= y,
                    facecolor='lime', alpha=0.5,
                    interpolate=True)
    ax.fill_between(x, y, y_hat, where=y_hat < y, facecolor='red',
                    alpha=0.5, interpolate=True)
    ax.legend([line1, line2],
              ['Actual {1} use in {0}'.format(year, lb.title_dict[theme]), '\n'.join(tw.wrap('{1} use given before {2} habits but {0} weather'.format(year, lb.title_dict[theme], pre_year), wrapwidth))], loc=location)
    if save_percent > 0:
        ax.set_title('{2} Savings {0} vs before {4}, {1}% less, CVRMSE: {3}'.format(year, save_percent, lb.title_dict[theme], round(cvrmse, 2), pre_year))
    else:
        ax.set_title('{2} Savings {0} vs before {4}, {1}% more, CVRMSE: {3}'.format(year, abs(save_percent), lb.title_dict[theme], round(cvrmse, 2), pre_year))

def process_html_lean_saving(b, s, action, pre_start, pre_end,
                             post_start, post_end, pre_args,
                             post_args, years):
    with open(os.getcwd() + '/plot_FY_weather/html/savings.html', 'r') as rd:
        saving_lines = rd.readlines()
    saving_new = []
    # saving_lines = ['{0}'.format(x) for x in saving_lines]
    for year in years:
        new_lines = [x.replace("2012", str(year)) for x in saving_lines]
        saving_new += new_lines
        print new_lines
    saving_new_str = '\n'.join(saving_new)
    saving_new_str = saving_new_str.replace('OK0063ZZ_KTUL', '{0}_{1}'.format(b, s))

    with open(os.getcwd() + '/plot_FY_weather/html/template_lean_sv.html', 'r') as rd:
        lines = rd.readlines()
    print (pre_start[-4:])
    print int(pre_end[-4:]) - 1
    print int(post_start[-4:]) + 1
    print (post_end[-4:])
    tokens = action.split("--")
    # pre_args = [base_gas_pre, base_elec_pre, breakpoint_gas_pre,
    #             breakpoint_elec_pre]
    inside_savings = False
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("OK0063ZZ", b)
        lines[i] = lines[i].replace("KTUL", s)
        lines[i] = lines[i].replace("before CY2011 and after CY2007.png", "before {0} and after {1}.png".format(pre_end, pre_start))
        lines[i] = lines[i].replace("before CY2014 and after CY2011.png", "before {0} and after {1}.png".format(post_end, post_start))
        lines[i] = lines[i].replace("pre_start -- pre_end", "{0} -- {1}".format(int(pre_start[-4:]), int(pre_end[-4:]) - 1))
        lines[i] = lines[i].replace("retrofit year",
                                    str(int(pre_end[-4:])))
        lines[i] = lines[i].replace("post_start -- post_end", "{0} -- {1}".format(int(post_start[-4:]) + 1, int(post_end[-4:])))
        if not "Building OK0063ZZ did" in lines[i]:
            lines[i] = lines[i].replace("action", '<br>'.join(tokens))
        else:
            lines[i] = lines[i].replace("action", action)
        for (args, status) in zip([pre_args, post_args], ['pre retrofit', 'post retrofit']):
            lines[i] = lines[i].replace("{0} Base electric load: <br>".format(status), "{0} Base electric load: {1}<br>".format(status, round(args[1], 2)))
            lines[i] = lines[i].replace("{0} Base gas load: ".format(status), "{0} Base gas load: {1}".format(status, round(args[0], 2)))
            lines[i] = lines[i].replace("{0} Start cooling at: ".format(status), "{0} Start cooling at: {1}".format(status, args[3]))
            lines[i] = lines[i].replace("{0} Start heating at: ".format(status), "{0} Start heating at: {1}".format(status, args[2]))
            lines[i] = lines[i].replace("<!-- savings -->", saving_new_str)
    with open(os.getcwd() +
              '/plot_FY_weather/html/single_building/{0}.html'.format(b),
              'w+') as wt:
        wt.write(''.join(lines))

def plot_trend_per_dd_fromdb(b, s, breakpoints):
    conn = uo.connect('all')
    with conn:
        df_all = pd.read_sql('SELECT * FROM EUAS_monthly_weather WHERE Building_Number = \'{0}\''.format(b), conn)
    conn.close()
    df_all.sort(['year', 'month'], inplace=True)
    df_all = df_all[['year', 'month', 'eui_elec', 'eui_gas', 'hdd65', 'cdd65']]
    df_all = df_all[(df_all['year'] < 2016) & (df_all['year'] > 2002)]
    df_agg = df_all.groupby('year').sum()
    df_agg.drop('month', axis=1, inplace=True)
    df_agg.reset_index(inplace=True)
    df_agg['Date'] = df_agg.apply(lambda r: datetime.datetime(int(r['year']), 1, 1) if not np.isnan(r['year']) else np.nan, axis=1)
    df_agg['eui_gas_perdd'] = df_agg.apply(lambda r: r['eui_gas'] / r['hdd65'] if r['hdd65'] > 0 else np.nan, axis=1)
    df_agg['eui_elec_perdd'] = df_agg.apply(lambda r: r['eui_elec'] / r['cdd65'] if r['cdd65'] > 0 else np.nan, axis=1)
    df_agg.set_index('Date', inplace=True)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    bx = plt.axes()
    gas_line_color = '#DE4A50'
    elec_line_color = '#429CD5'
    ylimit = max(df_agg['eui_gas_perdd'].max(),
                 df_agg['eui_elec_perdd'].max()) * 1.1
    line1, = plt.plot(df_agg.index, df_agg['eui_gas_perdd'], ls='-',
                      lw=2, marker='o', color=gas_line_color)
    line2, = plt.plot(df_agg.index, df_agg['eui_elec_perdd'], ls='-',
                      lw=2, marker='o', color=elec_line_color)
    hdd = df_agg['hdd65'].tolist()
    hdd = [int(round(x, 0)) for x in hdd]
    cdd = df_agg['cdd65'].tolist()
    cdd = [int(round(x, 0)) for x in cdd]
    for m, n, d in zip(df_agg.index, df_agg['eui_gas_perdd'], hdd):
        bx.annotate('HDD\n{0}'.format(d), xy=(m, n))
    for m, n, d in zip(df_agg.index, df_agg['eui_elec_perdd'], cdd):
        bx.annotate('CDD\n{0}'.format(d), xy=(m, n))
    for bp in breakpoints:
        x = pd.to_datetime([pd.to_datetime(bp)] * 2)
        plt.plot(x, [0, ylimit], ls='--', lw=2, color='gray')
    plt.legend([line1, line2], ['Gas', 'Electric'],
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.ylim((0, ylimit))
    plt.title("Electric EUI per degree day (65F) and Gas EUI per degree day (65F) Trend")
    plt.ylabel("[kBtu/(sq.ft*year*degree day(65F)]")
    plt.xlabel("Calendar Year")
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_year_perdd.png'.format(b, s), dpi = 70)
    plt.close()
    return
    

def plot_trend_per_dd(b, s, df_energy, breakpoints):
    df_hdd = pd.read_csv(weatherdir + \
                         'station_dd/{0}_HDD.csv'.format(s))
    # df_hdd.sort(['year', 'month'], inplace=True)
    df_hdd = df_hdd[['year', 'month', '65F']]
    df_cdd = pd.read_csv(weatherdir + \
                         'station_dd/{0}_CDD.csv'.format(s))
    df_cdd = df_cdd[['year', 'month', '65F']]
    df_dd = pd.merge(df_hdd, df_cdd, on=['year', 'month'], how='inner', suffixes=['_hdd', '_cdd'])
    df_energy = df_energy[df_energy['year'] < 2016]
    df_energy = df_energy.groupby(['year']).filter(lambda x: len(x) >
                                                   11)
    if len(df_energy) < 12:
        print 'not enough data for trend plot'
        return
    df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1) if not np.isnan(r['year']) else np.nan, axis=1)
    df_all = pd.merge(df_energy, df_dd, on=['year', 'month'],
                      how='inner')
    df_all.sort(['year', 'month'], inplace=True)
    df_all = df_all[['year', 'month', 'eui_elec', 'eui_gas', '65F_hdd', '65F_cdd']]
    df_agg = df_all.groupby('year').sum()
    df_agg.drop('month', axis=1, inplace=True)
    df_agg.reset_index(inplace=True)
    df_agg['Date'] = df_agg.apply(lambda r: datetime.datetime(int(r['year']), 1, 1) if not np.isnan(r['year']) else np.nan, axis=1)
    df_agg['eui_gas_perdd'] = df_agg.apply(lambda r: r['eui_gas'] / r['65F_hdd'] if r['65F_hdd'] > 0 else np.nan, axis=1)
    df_agg['eui_elec_perdd'] = df_agg.apply(lambda r: r['eui_elec'] / r['65F_cdd'] if r['65F_cdd'] > 0 else np.nan, axis=1)
    df_agg.set_index('Date', inplace=True)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    bx = plt.axes()
    gas_line_color = '#DE4A50'
    elec_line_color = '#429CD5'
    ylimit = max(df_agg['eui_gas_perdd'].max(),
                 df_agg['eui_elec_perdd'].max()) * 1.1
    line1, = plt.plot(df_agg.index, df_agg['eui_gas_perdd'], ls='-',
                      lw=2, marker='o', color=gas_line_color)
    line2, = plt.plot(df_agg.index, df_agg['eui_elec_perdd'], ls='-',
                      lw=2, marker='o', color=elec_line_color)
    hdd = df_agg['65F_hdd'].tolist()
    hdd = [int(round(x, 0)) for x in hdd]
    cdd = df_agg['65F_cdd'].tolist()
    cdd = [int(round(x, 0)) for x in cdd]
    for m, n, d in zip(df_agg.index, df_agg['eui_gas_perdd'], hdd):
        bx.annotate('HDD\n{0}'.format(d), xy=(m, n))
    for m, n, d in zip(df_agg.index, df_agg['eui_elec_perdd'], cdd):
        bx.annotate('CDD\n{0}'.format(d), xy=(m, n))
    for bp in breakpoints:
        x = pd.to_datetime([pd.to_datetime(bp)] * 2)
        y = bx.get_ylim()
        plt.plot(x, y, ls='--', lw=2, color='gray')
    plt.legend([line1, line2], ['Gas', 'Electric'],
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.ylim((0, ylimit))
    plt.title("Electric EUI per degree day (65F) and Gas EUI per degree day (65F) Trend")
    plt.ylabel("[kBtu/(sq.ft*year*degree day(65F)]")
    plt.xlabel("Calendar Year")
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_year_perdd.png'.format(b, s), dpi = 70)
    plt.close()
    return
    
def plot_trend_fromdb(b, s, breakpoints):
    conn = uo.connect('all')
    with conn:
        df_all = pd.read_sql('SELECT * FROM EUAS_monthly_weather WHERE Building_Number = \'{0}\''.format(b), conn)
    conn.close()
    df_all.sort(['year', 'month'], inplace=True)
    df_all = df_all[['year', 'month', 'eui_elec', 'eui_gas', 'hdd65', 'cdd65']]
    df_all = df_all[(df_all['year'] < 2016) & (df_all['year'] > 2002)]
    print type(df_all['eui_gas'].tolist()[0])
    byyear = df_all.groupby(['year']).sum()
    byyear.index = byyear.index.map(lambda x: datetime.datetime(int(x), 1, 1))
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    bx = plt.axes()
    gas_line_color = '#DE4A50'
    elec_line_color = '#429CD5'
    hdd = byyear['hdd65'].tolist()
    hdd = [int(round(x, 0)) for x in hdd]
    cdd = byyear['cdd65'].tolist()
    cdd = [int(round(x, 0)) for x in cdd]
    bx = plt.axes()
    line1, = plt.plot(byyear.index, byyear['eui_gas'], ls='-', lw=2,
                      marker='o', color=gas_line_color)
    line2, = plt.plot(byyear.index, byyear['eui_elec'], ls='-', lw=2,
                      marker='o', color=elec_line_color)
    for m, n, d in zip(byyear.index, byyear['eui_gas'], hdd):
        bx.annotate('HDD\n{0}'.format(d), xy=(m, n))
    for m, n, d in zip(byyear.index, byyear['eui_elec'], cdd):
        bx.annotate('CDD\n{0}'.format(d), xy=(m, n))
    for bp in breakpoints:
        x = pd.to_datetime([pd.to_datetime(bp)] * 2)
        y = bx.get_ylim()
        plt.plot(x, y, ls='--', lw=2, color='gray')
    plt.legend([line1, line2], ['Gas', 'Electric'],
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.title("Electric EUI and Gas EUI Trend")
    plt.ylabel("[kBtu/sq.ft/year]")
    plt.xlabel("Calendar Year")
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_year.png'.format(b, s), dpi = 70)
    plt.close()
    return

def plot_trend(b, s, df_energy, breakpoints):
    df_hdd = pd.read_csv(weatherdir + \
                         'station_dd/{0}_HDD.csv'.format(s))
    # df_hdd.sort(['year', 'month'], inplace=True)
    df_hdd = df_hdd[['year', 'month', '65F']]
    df_cdd = pd.read_csv(weatherdir + \
                         'station_dd/{0}_CDD.csv'.format(s))
    df_cdd = df_cdd[['year', 'month', '65F']]
    df_dd = pd.merge(df_hdd, df_cdd, on=['year', 'month'], how='inner', suffixes=['_hdd', '_cdd'])
    df_energy = df_energy[df_energy['year'] < 2016]
    df_energy = df_energy.groupby(['year']).filter(lambda x: len(x) >
                                                   11)
    if len(df_energy) < 12:
        print 'not enough data for trend plot'
        return
    df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1) if not np.isnan(r['year']) else np.nan, axis=1)
    df_all = pd.merge(df_energy, df_dd, on=['year', 'month'],
                      how='inner')
    df_all.sort(['year', 'month'], inplace=True)
    byyear = df_all.groupby(['year']).sum()
    byyear.index = byyear.index.map(lambda x: datetime.datetime(int(x), 1, 1))
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    bx = plt.axes()
    gas_line_color = '#DE4A50'
    elec_line_color = '#429CD5'
    bx.plot(byyear.index, byyear['65F_hdd'], ls='-', lw=2, marker='o',
            color=gas_line_color)
    # print type(byyear.index[0])
    ylimit = bx.get_ylim()
    plt.title('Total HDD (65F) Trend')
    plt.xlabel("Calendar Year")
    plt.gca().set_ylim(bottom=0)
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_hdd.png'.format(b, s), dpi = 300)
    plt.close()
    plt.plot(byyear.index, byyear['65F_cdd'], ls='-', lw=2,
             marker='o', color=elec_line_color)
    plt.title('Total CDD (65F) Trend')
    plt.xlabel("Calendar Year")
    plt.gca().set_ylim(bottom=0)
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_cdd.png'.format(b, s), dpi = 300)
    hdd = byyear['65F_hdd'].tolist()
    hdd = [int(round(x, 0)) for x in hdd]
    cdd = byyear['65F_cdd'].tolist()
    cdd = [int(round(x, 0)) for x in cdd]
    df = df_energy.set_index('Date')
    line1, = plt.plot(df.index, df['eui_gas'], ls='-', lw=2,
                      marker='o', color=gas_line_color)
    line2, = plt.plot(df.index, df['eui_elec'], ls='-', lw=2,
                      marker='o', color=elec_line_color)
    plt.legend([line1, line2], ['Gas', 'Electric'],
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.title("Electric and Gas kBtu/sq.ft. Trend")
    plt.ylabel("[kBtu/sq.ft/month]")
    plt.xlabel("Calendar Year")
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_month.png'.format(b, s), dpi = 300)
    plt.close()
    bx = plt.axes()
    df2 = df_energy.groupby('year').sum()
    df2.reset_index(inplace=True)
    df2['Date'] = df2.apply(lambda r: datetime.datetime(int(r['year']), 1, 1) if not np.isnan(r['year']) else np.nan, axis=1)
    df2.set_index('Date', inplace=True)
    line1, = plt.plot(df2.index, df2['eui_gas'], ls='-', lw=2,
                      marker='o', color=gas_line_color)
    line2, = plt.plot(df2.index, df2['eui_elec'], ls='-', lw=2,
                      marker='o', color=elec_line_color)
    for m, n, d in zip(df2.index, df2['eui_gas'], hdd):
        bx.annotate('HDD\n{0}'.format(d), xy=(m, n))
    for m, n, d in zip(df2.index, df2['eui_elec'], cdd):
        bx.annotate('CDD\n{0}'.format(d), xy=(m, n))
    for bp in breakpoints:
        x = pd.to_datetime([pd.to_datetime(bp)] * 2)
        y = bx.get_ylim()
        plt.plot(x, y, ls='--', lw=2, color='gray')
    plt.legend([line1, line2], ['Gas', 'Electric'],
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.title("Electric EUI and Gas EUI Trend")
    plt.ylabel("[kBtu/sq.ft/year]")
    plt.xlabel("Calendar Year")
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/trend/{0}_{1}_year.png'.format(b, s), dpi = 300)
    plt.close()
    return

def plot_action_fromdb():
    conn = uo.connect('all')
    with conn:
        df_action = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
        df_pro = pd.read_sql('SELECT * FROM EUAS_ecm_program', conn)
        df_bs = pd.read_sql('SELECT * FROM EUAS_monthly_weather', conn)
    bs_dict = dict(zip(df_bs['Building_Number'], df_bs['ICAO']))
    df_action = df_action[['Building_Number', 'high_level_ECM', 'detail_level_ECM', 'Substantial_Completion_Date']]
    df_act = df_action.copy()
    df_act = df_act.dropna()
    df_act['action'] = df_act.apply(lambda r: r['high_level_ECM'] + ' -- ' + r['detail_level_ECM'], axis=1)
    df_act.drop(['high_level_ECM', 'detail_level_ECM'], axis=1,
                inplace=True)
    gr = df_act.groupby('Building_Number')
    names = list(gr.groups)
    # print names.index('WA0120BN')
    lines = ['Building_Number,Time,Action,Electric_Saving,Gas_Saving,Electric_Before,Electric_After,Gas_Before,Gas_After,Electric_CVRMSE,Gas_CVRMSE']
    # FIXME: PA0060ZZ has None in eui_gas
    del names[153]
    names = ['CA0154ZZ']
    # names = ['CA0306ZZ']
    for i, name in enumerate(names):
        print i, name, '222222222222222222222222222222'
        group = gr.get_group(name)
        df_temp = group.groupby(['Substantial_Completion_Date'])['action'].apply(lambda x: '\n'.join(x))
        df_show = df_temp.to_frame('ECM action')
        # df_show['Building Number'] = 'CT0013ZZ'
        df_show.reset_index(inplace=True)
        df_show['Substantial_Completion_Date'] = pd.to_datetime(df_show['Substantial_Completion_Date'])
        df_show.sort_values(by=['Substantial_Completion_Date'], inplace=True)
        days_diff = useq.dist_between_adjacent(df_show['Substantial_Completion_Date'].tolist())
        pair = zip(df_show['Substantial_Completion_Date'].tolist(),
                   df_show['ECM action'].tolist())
        str_pair = [('{0}-{1}-{2}'.format(x[0].year, x[0].month, x[0].day), x[1]) for x in pair]
        breakpoints = [x[0] for x in str_pair]
        print breakpoints
        assert(len(breakpoints) > 0)
        ranges = ['before {0}'.format(breakpoints[0])] + useq.merge_adjacent(breakpoints, lambda x, y: '{0} -- {1}'.format(x, y)) + ['after {0}'.format(breakpoints[-1])]
        actionpoints = [x[1] for x in str_pair]
        actions = ['pre {0}'.format(actionpoints[0])] + useq.merge_adjacent(actionpoints, lambda x, y: 'post {0} pre {1}'.format(x, y)) + ['post {0}'.format(actionpoints[-1])]
        b = name
        # df_eng = gr_energy.get_group(b)
        # df_eng.reset_index(inplace=True)
        s = bs_dict[b]
        ar_pair = zip(actions, ranges)
        # un-comment if need to plot energy and dd trend side by side
        # plot_trend_fromdb(b, s, breakpoints)

        # print 'plot trend per dd'
        # plot_trend_per_dd_fromdb(b, s, breakpoints)

        results = []
        for a, r in zip(actions, ranges):
            result = ltm.lean_temperature_fromdb(b, s, 2, r, action=a)
            if result == None:
                result = (None, None, None)
            d = {'building': b, 'station': s, 'timerange': r, 'action': a}
            result += tuple([d])
            results.append(result)
        length = len(results)
        if length == 0:
            print 'no lean plot generated'
            continue
        def merge_action(string):
            string = string.replace('\n', ';')
            string = string.replace('pre ', '')
            return string
        def concat(string):
            return string.replace('\n', ';')
        for i in range(len(results) - 1):
            d_save, d_cvrmse = plot_saving_fromdb(b, s, results[i], results[i + 1])
            lines.append(','.join(map(str, [b, breakpoints[i],
                                            # (merge_action(actions[i])),
                                            concat(actionpoints[i]),
                                            d_save['elec_percent'],
                                            d_save['gas_percent'],
                                            d_save['elec_before'],
                                            d_save['elec_after'],
                                            d_save['gas_before'],
                                            d_save['gas_after'],
                                            d_cvrmse['elec'],d_cvrmse['gas']])))
        process_html(b, s, results, breakpoints)
    with open (os.getcwd() + '/plot_FY_weather/html/table/action_saving.csv', 'w+') as wt:
        wt.write('\n'.join(lines))

def plot_action():
    df = pd.read_csv(homedir + 'master_table/ECM/EUAS_ecm.csv')
    df = df[['Building Number', 'high_level_ECM', 'detail_level_ECM', 'Substantial Completion Date']]
    df_bs = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    df_bs = df_bs[df_bs['Valid Weather Data'] == 1]
    bs_dict = dict(zip(df_bs['Building Number'], df_bs['ICAO']))
    print len(df)
    df.dropna(inplace=True)
    print len(df)
    df['action'] = df.apply(lambda r: r['high_level_ECM'] + ' -- ' + r['detail_level_ECM'], axis=1)
    df.drop(['high_level_ECM', 'detail_level_ECM'], axis=1,
            inplace=True)
    gr = df.groupby('Building Number')
    names = list(gr.groups)
    # print names.index('WA0120BN')
    # names = ['CA0168ZZ']
    for i, name in enumerate(names[64:]):
        print i, name, '222222222222222222222222222222'
        group = gr.get_group(name)
        df_temp = group.groupby(['Substantial Completion Date'])['action'].apply(lambda x: '\n'.join(x))
        df_show = df_temp.to_frame('ECM action')
        # df_show['Building Number'] = 'CT0013ZZ'
        df_show.reset_index(inplace=True)
        df_show['Substantial Completion Date'] = pd.to_datetime(df_show['Substantial Completion Date'])
        df_show.sort('Substantial Completion Date', inplace=True)
        days_diff = useq.dist_between_adjacent(df_show['Substantial Completion Date'].tolist())
        pair = zip(df_show['Substantial Completion Date'].tolist(),
                   df_show['ECM action'].tolist())
        str_pair = [('{0}-{1}-{2}'.format(x[0].year, x[0].month, x[0].day), x[1]) for x in pair]
        breakpoints = [x[0] for x in str_pair]
        print breakpoints
        assert(len(breakpoints) > 0)
        ranges = ['before {0}'.format(breakpoints[0])] + useq.merge_adjacent(breakpoints, lambda x, y: '{0} -- {1}'.format(x, y)) + ['after {0}'.format(breakpoints[-1])]
        actionpoints = [x[1] for x in str_pair]
        actions = ['pre {0}'.format(actionpoints[0])] + useq.merge_adjacent(actionpoints, lambda x, y: 'post {0} pre {1}'.format(x, y)) + ['post {0}'.format(actionpoints[-1])]
        b = name
        if b in bs_dict:
            s = bs_dict[b]
        else:
            print 'no weather data'
            continue
        ar_pair = zip(actions, ranges)
        # FIXME populate energy and weather data
        df_eng = pd.read_csv(weatherdir + \
                             'energy_temp/{0}_{1}.csv'.format(b, s))
        # un-comment if need to plot energy and dd trend side by side
        plot_trend(b, s, df_eng, breakpoints)

        print 'plot trend per dd'
        plot_trend_per_dd(b, s, df_eng, breakpoints)

        results = []
        for a, r in zip(actions, ranges):
            result = ltm.lean_temperature(b, s, 2, r, action=a)
            if result == None:
                result = (None, None, None)
            d = {'building': b, 'station': s, 'timerange': r, 'action': a}
            result += tuple([d])
            results.append(result)
        length = len(results)
        if length == 0:
            print 'no lean plot generated'
            continue
        for i in range(len(results) - 1):
            plot_saving(b, s, results[i], results[i + 1])
        process_html(b, s, results)

        # # Degree day version
        # results = []
        # for a, r in zip(actions, ranges):
        #     result = ld.plot_lean_saving_one(b, s, r)
        #     results.append(result)
        # length = len(results)
        # if length == 0:
        #     print 'no lean plot generated'
        #     continue
        # for i in range(len(results) - 1):
        #     ld.plot_saving(b, s, results[i], results[i + 1])
        # print result

def process_html(b, s, results, breakpoints):
    with open(os.getcwd() + \
              '/plot_FY_weather/html/single_building/template_singlebuilding.html', 'r') as rd:
        lines = rd.readlines()
    def format_action(action, time):
        action_list = action.split('\n')
        action_pair_list = [x.split(' -- ') for x in action_list]
        action_sort = sorted(action_pair_list, key=lambda x: (x[0],
                                                              x[1]))
        action_init = action_sort[0][0]
        string = '{0}\n\n'.format(time)
        string += action_init
        for i in range(len(action_sort)):
            action_now = action_sort[i][0]
            if action_now == action_init:
                string += ('\n    ' + action_sort[i][1])
            else:
                string += ('\n' + action_sort[i][0])
                string += ('\n    ' + action_sort[i][1])
                action_init = action_now
        string = string.replace('GSALink\n    GSALink', 'GSALink')
        return string
    def substitute_data(results, j, i, end):
        if results[j][0] == None:
            base_gas = '****'
            bpoint_gas = '****'
        else:
            base_gas = str(round(results[j][0]['base_gas'], 2))
            bpoint_gas = str(int(results[j][0]['breakpoint']))
        if results[j][1] == None:
            base_elec = '****'
            bpoint_elec = '****'
        else:
            base_elec = str(round(results[j][1]['base_elec'], 2))
            bpoint_elec = str(int(results[j][1]['breakpoint']))
        data_lines = lines[i + 1: i + end]
        data = ''.join(data_lines)
        data = data.replace('base_elec', base_elec)
        data = data.replace('base_gas', base_gas)
        data = data.replace('bpoint_elec', bpoint_elec, 2)
        data = data.replace('bpoint_gas', bpoint_gas, 2)
        return data
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("IA0112ZZ", b)
        lines[i] = lines[i].replace("KMXO", s)
        if "substitute period" in lines[i]:
            newlines = []
            for j in range(len(results) - 1):
                action = results[j][-1]['action']
                action = action[action.find('pre') + 4:]
                action = format_action(action, breakpoints[j])
                newlines.append(lines[i + 1].replace("action", action))
                newlines.append(lines[i + 2].replace("building_station_period", "{0}_{1}_{2}".format(b, s, results[j + 1][-1]['timerange'])))
            str_newlines = ''.join(newlines)
            lines[i + 1] = ''
            lines[i + 2] = str_newlines
        if "substitute lean period" in lines[i]:
            newlines = []
            for j in range(len(results) - 1):
                newlines.append(lines[i + 1].replace("building_station_combined_period", "{0}_{1}_combined_{2}".format(b, s, results[j][-1]['timerange'])))
                action = results[j][-1]['action']
                action = action[action.find('pre') + 4:]
                action = format_action(action, breakpoints[j])
                newlines.append(lines[i + 2].replace("action", action))
            newlines.append(lines[i + 1].replace("building_station_combined_period", "{0}_{1}_combined_{2}".format(b, s, results[-1][-1]['timerange'])))
            str_newlines = ''.join(newlines)
            lines[i + 1] = str_newlines
            lines[i + 2] = ''
        if "substitute lean data" in lines[i]:
            newlines = []
            for j in range(len(results) - 1):
                # if results[j][0] == None or results[j][1] == None:
                #     return 'No data'
                data = substitute_data(results, j, i, 8)
                newlines.append(data)
            data_last = substitute_data(results, -1, i, 7)
            newlines.append(data_last)
            str_newlines = ''.join(newlines)
            lines[i + 1] = str_newlines
            lines[i + 2: i + 9] = ''
    with open(os.getcwd() + \
              '/plot_FY_weather/html/single_building/{0}.html'.format(b), 'w+') as wt:
        print 'write to html: {0}.html ...'.format(b)
        wt.write(''.join(lines))
    return

def modify_index():
    actions = ['Advanced_Metering', 'Building_Envelope',
               'Building_Tuneup_or_Utility_Improvements', 'HVAC',
               'Lighting', 'GSALink']
    pages = [os.getcwd() + '/plot_FY_weather/html/{0}.html'.format(x)
             for x in actions]
    htmls = glob.glob(os.getcwd() + \
                      '/plot_FY_weather/html/single_building/*.html')
    buildings = [x[x.rfind('/') + 1: -5] for x in htmls]
    for p in pages:
        with open (p, 'r') as rd:
            lines = rd.readlines()
        for i, line in enumerate(lines):
            idx = line.find('/') + 1
            b = line[idx: idx + 8]
            if ('.html' in line) and not (b in buildings):
                lines[i] = lines[i].replace("</a>", " ... data coming"+
                    " soon ...</a>")
        with open (p, 'w+') as wt:
            wt.write(''.join(lines))

def table_for_robust_set():
    conn = uo.connect('all')
    study_set = gbs.get_energy_set('eui').intersection(gbs.get_cat_set(['A', 'I'], conn))
    df = pd.read_csv(os.getcwd() + '/plot_FY_weather/html/table/action_saving.csv')
    df = df[df['Building_Number'].isin(study_set)]
    df.sort('Building_Number', inplace=True)
    df.to_csv(os.getcwd() + '/plot_FY_weather/html/table/action_saving_robustset.csv', index=False)
    return

def main():
    # create_index()
    # modify_index()
    # plot_action_alone()
    # plot_action()
    plot_action_fromdb()
    # table_for_robust_set()
    return
    
main()
