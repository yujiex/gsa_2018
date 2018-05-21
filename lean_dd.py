import os
import glob
import pandas as pd
import numpy as np
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
from scipy import stats
import textwrap as tw
import util
import label as lb

homedir = os.getcwd() + '/csv_FY/'
weatherdir = os.getcwd() + '/csv_FY/weather/'

def join_dd_temp_energy(b, s, kind):
    print (b, s)
    try:
        df_eng_temp = pd.read_csv(weatherdir +
                                  'energy_temp/{0}_{1}.csv'.format(b,
                                                                   s))
    except IOError:
        print '{0}_{1}.csv does not exist'.format(b, s)
        return
    df_dd = pd.read_csv(weatherdir + 
                        'station_dd/{0}_{1}.csv'.format(s, kind))
    df_all = pd.merge(df_eng_temp, df_dd, on=['year', 'month',
                                              'timestamp'],
                      how='inner')
    df_all.to_csv(weatherdir + '/dd_temp_eng/{2}_{0}_{1}.csv'.format(b, s, kind), index=False)
    return

def rejoin():
    df_bs = pd.read_csv(homedir + \
                        'master_table/indicator_wECM_weather.csv')
    df_bs = df_bs[df_bs['Valid Weather Data'] == 1]
    bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
    for (b, s) in bs_pair:
        join_dd_temp_energy(b, s, 'CDD')
        join_dd_temp_energy(b, s, 'HDD')
    return

def plot_dd_fit(df_all, slope, intercept, r, p, xF, theme, kind, b, s,
                timerange):
    x = df_all[xF]
    y = df_all[theme]
    xd = [0, x.max()]
    yd = [intercept, slope * x.max() + intercept]
    sns.set_style("white")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1.5)
    bx = plt.axes()
    bx.annotate('y = {0} x + {1}\nR^2: {2}, p_value: {3}'.format(round(slope, 3), round(intercept, 3), round(r * r, 3), round(p, 3)),
                xy = (x.max() * 0.1, y.max() * 0.9),
                xytext = (x.max() * 0.05, y.max() * 0.95), fontsize=20)
    bx.plot(x, y, 'o', xd, yd, '-')
    plt.ylim((0, y.max() * 1.1))
    plt.title('{0} - {1} Plot, Base: {2}, {3} 2013'.format(lb.title_dict[theme], kind, xF, timerange))
    plt.suptitle('Building {0}, Station {1}'.format(b, s))
    plt.xlabel('{0} Deg F'.format(kind))
    plt.ylabel(lb.ylabel_dict[theme])
    # plt.show()
    # P.savefig(os.getcwd() + '/plot_FY_weather/dd_energy/{2}/{0}_{1}_{3}.png'.format(b, s, theme, timerange), dpi = 150)
    # plt.close()

# kind: CDD, HDD, theme: 'eui_elec', 'eui_gas', 'eui_heat'
def opt_lireg(b, s, df_all, kind, theme, timerange):
    dd_list = ['{0}F'.format(x) for x in range(40, 81)]
    results = []
    for col in dd_list:
        lean_x = df_all[col]
        lean_y = df_all[theme]
        slope, intercept, r_value, p_value, std_err = \
            stats.linregress(lean_x, lean_y)
        results.append([slope, intercept, r_value, p_value, col])
    # sort by r squared
    ordered_result = sorted(results, key=lambda x: x[2]*x[2],
                            reverse=True)
    slope_opt, intercept_opt, r_opt, p_opt, col_opt = ordered_result[0]
    # plot_dd_fit(df_all, slope_opt, intercept_opt, r_opt, p_opt,
    #             col_opt, theme, kind, b, s, timerange)
    return ordered_result[0]

def dd2temp(dd, t_base, kind):
    if kind == 'HDD':
        return t_base - dd/30.0
    else:
        return t_base + dd/30.0
            
def temp2dd(temp, t_base, kind):
    if kind == 'HDD':
        return (t_base - temp) * 30.0
    else:
        return (temp - t_base) * 30.0

def lean_one(b, s, k_hdd, k_cdd, base_gas, base_elec, t_base_hdd,
             t_base_cdd, r2_hdd, r2_cdd, timerange, theme_h, theme_c,
             side):
    y_upperlim = 15
    x_leftlim = 40
    x_rightlim = 85
    if side == 'combined':
        base_elec = max(base_elec, 0)
        base_gas = max(base_gas, 0)
    elif side == 'heating':
        base_elec = 0
    elif side == 'cooling':
        base_gas = 0
    df_hdd = pd.read_csv(weatherdir +
                         'dd_temp_eng/HDD_{0}_{1}.csv'.format(b, s))
    df_cdd = pd.read_csv(weatherdir +
                         'dd_temp_eng/CDD_{0}_{1}.csv'.format(b, s))
    yearcol, timefilter = util.get_time_filter(timerange)
    hdd_base_header = '{0}F'.format(t_base_hdd)
    cdd_base_header = '{0}F'.format(t_base_cdd)
    df_hdd = df_hdd[['year', 'month', 'timestamp', theme_h, s,
                     hdd_base_header]]
    df_cdd = df_cdd[['year', 'month', 'timestamp', theme_c,
                     cdd_base_header]]
    df_hdd['timestamp'] = pd.to_datetime(df_hdd['timestamp'])
    df_hdd['in_range'] = df_hdd[yearcol].map(timefilter)
    df_hdd = df_hdd[df_hdd['in_range']]
    df_cdd['timestamp'] = pd.to_datetime(df_cdd['timestamp'])
    df_cdd['in_range'] = df_cdd[yearcol].map(timefilter)
    df_cdd = df_cdd[df_cdd['in_range']]
    df_hdd['cvt_temp_hdd'] = df_hdd[hdd_base_header].map(lambda x: dd2temp(x, t_base_hdd, 'HDD'))
    df_cdd['cvt_temp_cdd'] = df_cdd[cdd_base_header].map(lambda x: dd2temp(x, t_base_cdd, 'CDD'))
    df_hdd.rename(columns={hdd_base_header: hdd_base_header + '_HDD'},
                  inplace=True)
    df_cdd.rename(columns={cdd_base_header: cdd_base_header + '_CDD'},
                  inplace=True)
    df_plot = pd.merge(df_hdd, df_cdd, on=['year', 'month'],
                       how='inner')
    df_plot[theme_h + '_hat'] = df_plot[hdd_base_header + '_HDD'].map(lambda x: k_hdd * x + base_gas)
    df_plot[theme_c + '_hat'] = df_plot[cdd_base_header + '_CDD'].map(lambda x: k_cdd * x + base_elec)
    df_plot[theme_h + '_offset'] = \
        df_plot.apply(lambda r: r[theme_h] + base_elec if
                    r['cvt_temp_hdd'] < t_base_hdd else np.nan,
                    axis=1)
    df_plot[theme_c + '_offset'] = \
        df_plot.apply(lambda r: r[theme_c] + base_gas if
                    r['cvt_temp_cdd'] > t_base_cdd else np.nan,
                    axis=1)
    df_plot[theme_h + '_hat_offset'] = \
        df_plot.apply(lambda r: r[theme_h + '_hat'] + base_elec if
                    r['cvt_temp_hdd'] < t_base_hdd else np.nan,
                    axis=1)
    df_plot[theme_c + '_hat_offset'] = \
        df_plot.apply(lambda r: r[theme_c + '_hat'] + base_gas if
                    r['cvt_temp_cdd'] > t_base_cdd else np.nan,
                    axis=1)
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("paper", font_scale=1)
    bx = plt.axes()
    x1 = df_plot['cvt_temp_hdd']
    y1 = df_plot[theme_h + '_offset']
    y1_hat = df_plot[theme_h + '_hat_offset']
    sorted_x1y1hat = sorted(zip(x1, y1_hat), key=lambda x: x[0])
    sorted_x1 = [p[0] for p in sorted_x1y1hat]
    sorted_y1_hat = [p[1] for p in sorted_x1y1hat]
    x2 = df_plot['cvt_temp_cdd']
    y2 = df_plot[theme_c + '_offset']
    y2_hat = df_plot[theme_c + '_hat_offset']
    sorted_x2y2hat = sorted(zip(x2, y2_hat), key=lambda x: x[0])
    sorted_x2 = [p[0] for p in sorted_x2y2hat]
    sorted_y2_hat = [p[1] for p in sorted_x2y2hat]
    if side == 'heating':
        xmin = x1.min()
        xmax = x1.max()
    elif side == 'cooling':
        xmin = x2.min()
        xmax = x2.max()
    else:
        xmin = min(x1.min(), x2.min())
        xmax = max(x1.max(), x2.max())
    gas_line_color = '#DE4A50'
    gas_mk_color = '#DE4A50'
    elec_line_color = '#429CD5'
    elec_mk_color = '#429CD5'
    base_gas_color = 'orange'
    base_elec_color = 'yellow'
    base_elec_text_color = 'goldenrod'
    marker_size = 3
    marker_style = 'o'
    alpha = 0.5
    font_family = 'sans-serif'
    heating_note_color = '#A02225'
    cooling_note_color = '#4D7FBC'
    plt.figure(figsize=(5, 5), dpi=300, facecolor='w', edgecolor='k')
    bx = plt.axes()
    if side == 'heating' or side == 'combined':
        plt.plot(x1, y1, marker_style, markerfacecolor=gas_mk_color, ms=marker_size)
        # bx.annotate('HEATING', xy = (xmin + 1, base_elec + base_gas + \
        #                              0.2), fontsize=8,
        #             color=heating_note_color,
        #             weight='semibold',
        #             family=font_family)
        plt.plot(sorted_x1, sorted_y1_hat, '-', color=gas_line_color)
        bx.fill_between(sorted_x1, base_elec + base_gas, sorted_y1_hat, facecolor=gas_line_color, alpha=alpha)
    if side == 'cooling' or side == 'combined':
        plt.plot(x2, y2, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
        # bx.annotate('COOLING', xy = (xmax - 9, (base_elec + base_gas)
        #                              + 0.2), fontsize=8,
        #             color=cooling_note_color,
        #             weight='semibold',
        #             family=font_family)
        plt.plot(sorted_x2, sorted_y2_hat, '-', color=elec_line_color)
        bx.fill_between(sorted_x2, base_elec + base_gas, sorted_y2_hat, facecolor=elec_line_color, alpha=alpha)
        # bx.annotate('BASE ELECTRIC', xy = ((xmin + xmax)/2 - 8,
        #                                    base_elec/2), fontsize=8,
        #             color=base_elec_text_color,
        #             weight='semibold',
        #             family=font_family)
    plt.plot([xmin, xmax], [base_elec] * 2, color=base_elec_color)
    bx.fill_between([xmin, xmax], 0, [base_elec] * 2, facecolor=base_elec_color, alpha=alpha)
    plt.plot([xmin, xmax], [base_elec + base_gas] * 2, color=base_gas_color)
    bx.fill_between([xmin, xmax], base_elec, [base_elec + base_gas] * 2, facecolor=base_gas_color, alpha=alpha)
    print (round(r2_hdd, 2), round(r2_cdd, 2))
    if side == 'heating':
        plt.title('Building {0}, {1}\nHDD base {2}F ({4}{3})'.format(b, timerange, t_base_hdd, round(r2_hdd, 2), r'$R^2=$'))
    elif side == 'cooling':
        plt.title('Building {0}, {1}\nCDD base {2}F ({4}{3})'.format(b, timerange, t_base_cdd, round(r2_cdd, 2), r'$R^2=$'))
    else:
        plt.title('Building {0}, {1}\nHDD base {2}F ({6}{4}), CDD base {3}F({6}{5})'.format(b, timerange, t_base_hdd, t_base_cdd, round(r2_hdd, 2), round(r2_cdd, 2), r'$R^2=$'))
    plt.xlabel('Temperature Represented Degree Day [F]')
    plt.ylabel('Monthly [kBtu/sq.ft.]')
    plt.ylim((0, y_upperlim))
    plt.xlim((x_leftlim, x_rightlim))
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/lean_dd/{0}_{1}_{2}.png'.format(b, s, timerange, side), dpi = 150)
    plt.close()
    return (xmin, xmax, y1.max(), y2.max())

def plot_lean_saving_one(b, s, timerange):
    yearcol, timefilter = util.get_time_filter(timerange)
    df_gas = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'HDD', b, s))
    df_gas['timestamp'] = pd.to_datetime(df_gas['timestamp'])
    df_gas['in_range'] = df_gas[yearcol].map(timefilter)
    df_gas = df_gas[df_gas['in_range']]
    if len(df_gas) == 0:
        print 'no energy data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    df_elec = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'CDD', b, s))
    df_elec['timestamp'] = pd.to_datetime(df_elec['timestamp'])
    df_elec['in_range'] = df_elec[yearcol].map(timefilter)
    df_elec = df_elec[df_elec['in_range']]
    if len(df_elec) == 0:
        print 'no elec data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    slope_elec, intercept_elec, r_value_elec, p_value_elec, basetemp_elec = opt_lireg(b, s, df_elec, 'CDD', 'eui_elec', timerange)
    slope_gas, intercept_gas, r_value_gas, p_value_gas, basetemp_gas = opt_lireg(b, s, df_gas, 'HDD', 'eui_gas', timerange)
    d = {}
    r2_elec = (r_value_elec) ** 2
    r2_gas = (r_value_gas) ** 2
    d['base_elec'] = intercept_elec
    d['break_elec'] = basetemp_elec
    d['slope_elec'] = slope_elec
    d['base_gas'] = intercept_gas
    d['break_gas'] = basetemp_gas
    d['slope_gas'] = slope_elec
    d['timerange'] = timerange
    d['df_gas'] = df_gas
    d['df_elec'] = df_elec
    d['r2_gas'] = r2_gas
    d['r2_elec'] = r2_elec
    lean_one(b, s, slope_gas, slope_elec, intercept_gas,
             intercept_elec, int(basetemp_gas[:-1]),
             int(basetemp_elec[:-1]), r2_gas, r2_elec, timerange,
             'eui_gas', 'eui_elec', 'combined')
    return d

def calculate_saving_dd(b, s, result_pre, result_post):
    print 'creating saving plot ...'
    if result_pre == None or result_post == None:
        print 'not enough data for saving plot'
        return
    timerange_pre = result_pre['timerange']
    base_gas_pre = result_pre['base_gas']
    base_elec_pre = result_pre['base_elec']
    breakpoint_gas_pre = result_pre['break_gas']
    breakpoint_elec_pre = result_pre['break_elec']
    slope_gas_pre = result_pre['slope_gas']
    slope_elec_pre = result_pre['slope_elec']
    pre_args = [base_gas_pre, base_elec_pre, breakpoint_gas_pre,
                breakpoint_elec_pre]
    timerange_post = result_post['timerange']
    base_gas_post = result_post['base_gas']
    base_elec_post = result_post['base_elec']
    breakpoint_gas_post = result_post['break_gas']
    breakpoint_elec_post = result_post['break_elec']
    post_args = [base_gas_post, base_elec_post, breakpoint_gas_post,
                 breakpoint_elec_post]
    df_gas_post = result_post['df_gas']
    def CVRMSE(y, y_hat, n_par):
        n = len(y)
        return np.sqrt((np.subtract(y_hat, y) ** 2).sum() / (n - n_par))/np.array(y).mean()
    df_gas_post['eui_gas_hat'] = \
        df_gas_post.apply(lambda r: slope_gas_pre *
                          r[breakpoint_gas_pre] + base_gas_pre if
                          r[breakpoint_gas_pre] > 0 else r['eui_gas'],
                          axis=1)
    gas_cvrmse = CVRMSE(df_gas_post['eui_gas'], df_gas_post['eui_gas_hat'], 1)
    df_gas_post = df_gas_post[['year', 'month', 'timestamp', 'eui_gas',
                               'eui_gas_hat']]
    df_elec_post = result_post['df_elec']
    df_elec_post['eui_elec_hat'] = \
        df_elec_post.apply(lambda r: slope_elec_pre *
                           r[breakpoint_elec_pre] + base_elec_pre if
                           r[breakpoint_elec_pre] > 0 else
                           r['eui_elec'], axis=1)
    elec_cvrmse = CVRMSE(df_elec_post['eui_elec'], df_elec_post['eui_elec_hat'], 1)
    df_elec_post = df_elec_post[['year', 'month', 'timestamp',
                                 'eui_elec', 'eui_elec_hat']]
    df_post = pd.merge(df_gas_post, df_elec_post, on=['year', 'month',
                                                      'timestamp'],
                       how='inner')
    return df_post, timerange_post, timerange_pre, gas_cvrmse, elec_cvrmse

def plot_saving_aggyear(df, timerange_post, timerange_pre, theme, ax,
                        r2, cvrmse):
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
    x = energy['month']
    y = energy[theme]
    y_hat = energy[theme + '_hat']
    save_percent = round((sum(y_hat) - sum(y)) / sum(y_hat) * 100, 1)
    line1, = ax.plot(x, y, c=c1, ls='-', lw=2, marker='o')
    line2, = ax.plot(x, y_hat, c=c2, ls='-', lw=2, marker='o')
    # print x, '1111111111111'
    # print y, '2222222222222'
    # print y_hat, '333333333333'
    ax.fill_between(x, y, y_hat, where=y_hat >= y,
                    facecolor='lime', alpha=0.5, interpolate=True)
    ax.fill_between(x, y, y_hat, where=y_hat < y, facecolor='red',
                    alpha=0.5, interpolate=True)
    ax.legend([line1, line2], 
              ['Actual {1} use in {0}'.format(timerange_post, lb.title_dict[theme]), '\n'.join(tw.wrap('{1} use given {2} habits but {0} weather'.format(timerange_post, lb.title_dict[theme], timerange_pre), wrapwidth))], loc=location)
    def time_label(timerange):
        if 'before' in timerange or 'after' in timerange:
            return timerange[timerange.find(' ') + 1:]
        else:
            return timerange
    if save_percent > 0: 
        ax.set_title('{2} after ({0}) vs before ({4}), {1}% less, {5}{3}, CVRMSE={6}'.format(time_label(timerange_post), save_percent, lb.title_dict[theme], round(r2, 2), time_label(timerange_pre), r'$R^2=$', round(cvrmse, 2)))
    else:
        ax.set_title('{2} after ({0}) vs before ({4}), {1}% more, {5}{3}, CVRMSE={6}'.format(time_label(timerange_post), save_percent, lb.title_dict[theme], round(r2, 2), time_label(timerange_pre), r'$R^2=$', round(cvrmse, 2)))

def plot_saving(b, s, result_pre, result_post):
    df, timepost, timepre, gas_err, elec_err = \
        calculate_saving_dd(b, s, result_pre, result_post)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True, sharey=True)
    plot_saving_aggyear(df, timepost, timepre, 'eui_elec', ax_1,
                        float(result_pre['r2_elec']), elec_err)
    ax_1.set_xlim([1, 12])
    plot_saving_aggyear(df, timepost, timepre, 'eui_gas', ax_2,
                        float(result_pre['r2_gas']), gas_err)
    ax_2.set_xlim([1, 12])
    P.savefig(os.getcwd() + '/plot_FY_weather/html/single_building/saving_dd/{0}_{1}_{2}.png'.format(b, s, timepost), dpi = 70)
    return
