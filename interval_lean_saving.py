import pandas as pd
import numpy as np
import os
import glob
import seaborn as sns
import sqlite3
from datetime import datetime
import time
import shutil
import matplotlib.pyplot as plt
import seaborn as sns
import pylab as P
import json

import lean_temperature_monthly as ltm
import util_io as uo
import util
import label as lb

homedir = os.getcwd() + '/csv_FY/'
my_dpi = 100

def compute_piecewise(measure_type, df_all, b, s):
    npar = 2
    if measure_type == 'gas':
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format(s), 'eui': 'eui_gas', 'Timestamp': 'timestamp'})
        d = ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, None, df_reg)
    elif measure_type == 'electric':
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format(s), 'eui': 'eui_elec', 'Timestamp': 'timestamp'})
        d = ltm.piecewise_reg_one(b, s, npar, 'eui_elec', False, None, df_reg)
    return d

def join_interval(offset, b, s, area, col, m, measure_type, conn, year=None, season=None):
    with conn:
        df_w = pd.read_sql('SELECT * FROM {0}'.format(s), conn)
        df_minute = pd.read_sql('SELECT Timestamp, [{0}] FROM {1} WHERE Building_Number = \'{2}\''.format(col, measure_type, b), conn)
    df_minute['h'] = df_minute['Timestamp'].map(lambda x: x[:-5] + '00:00')
    df_e = df_minute.groupby('h').sum()
    df_e.reset_index(inplace=True)
    df_e.rename(columns={'h': 'Timestamp'}, inplace=True)
    df_e['eui'] = df_e[col] * m / area
    local = pd.to_datetime(df_w['Timestamp']).map(lambda x: x + np.timedelta64(offset, 's'))
    local_str = local.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
    df_w['Timestamp'] = local_str
    df_all = pd.merge(df_w, df_e, on='Timestamp', how='inner')
    df_all['hour'] = df_all['Timestamp'].map(lambda x: x[11:13]) 
    df_all['month'] = df_all['Timestamp'].map(lambda x: x[5:7]) 
    df_all['year'] = df_all['Timestamp'].map(lambda x: x[:4]) 
    df_all['day'] = df_all['Timestamp'].map(lambda x: datetime.strptime(x, '%Y-%m-%d %H:%M:%S').strftime('%a')) 
    # remove the outliers
    # df_all = df_all[df_all['eui'] >= 0]
    # up = df_all['eui'].quantile(0.99)
    # df_all = df_all[df_all['eui'] < up]
    if not year is None:
        df_all = df_all[df_all['year'] == str(year)]
    if not season is None:
        month_list = util.month_of_season(season)
        month_list = [str(x).zfill(2) for x in month_list]
        df_all = df_all[df_all['month'].isin(month_list)]
    return df_all

def build_energy_temperature(measure_type):
    conn = uo.connect('interval_ion')
    with conn:
        df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
        df_area = pd.read_sql('SELECT * FROM area', conn)
        df_tz = pd.read_sql('SELECT Building_Number, rawOffset FROM EUAS_timezone', conn)
    df_tz.set_index('Building_Number', inplace=True)
    df_area.set_index('Building_Number', inplace=True)
    bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    multiplier_dict = {'electric':  3.412, 'gas': 1.026}
    col = value_lb_dict[measure_type]
    m = multiplier_dict[measure_type]
    dfs = []
    # bs_pair = [x for x in bs_pair if x[0] == 'OR0033PE']
    for b, s in bs_pair:
        print b, s
        try:
            area = df_area.ix[b, 'Gross_Sq.Ft']
        except KeyError:
            print 'No area found'
            continue
        offset = df_tz.loc[b, 'rawOffset']
        df = join_interval(offset, b, s, area, col, m, measure_type, conn)
        if len(df) == 0:
            continue
        dfs.append(df)
        df['Building_Number'] = b
    df_all = pd.concat(dfs)
    with conn:
        df_all.to_sql('{0}_wtemp'.format(measure_type),
                      conn, if_exists='replace')
    conn.close()
    print 'end'

# source: http://stackoverflow.com/questions/22354094/pythonic-way-of-detecting-outliers-in-one-dimensional-observation-data
def mad_based_outlier(points, thresh=5):
    if len(points.shape) == 1:
        points = points[:,None]
    median = np.median(points, axis=0)
    diff = np.sum((points - median)**2, axis=-1)
    print diff[:5]
    diff = np.sqrt(diff)
    med_abs_deviation = np.median(diff)
    print med_abs_deviation

    modified_z_score = 0.6745 * diff / med_abs_deviation

    return modified_z_score > thresh

# def pos_p60_based_outlier(points, threshold=50):
#     positive = [x for x in points if x > 0]
#     p60 = np.percentile(positive, 80, axis=0)
#     pos_median = np.median([x for x in points if x > p60], axis=0)
#     return [(x > threshold * pos_median) or (x < 0) for x in points]

def pos_p60_based_outlier(points, threshold=5):
    positive = [x for x in points if x > 0]
    if len(positive) > 0:
        p60 = np.percentile(positive, 90, axis=0)
    else:
        p60 = np.percentile(points, 90, axis=0)
    pos_median = np.median([x for x in points if x > p60], axis=0)
    return [(x > threshold * pos_median) or (x < 0) for x in points]

def pos_median_based_outlier(points, threshold=50):
    positive = [x for x in points if x > 0]
    median = np.median(positive, axis=0)
    pos_median = np.median([x for x in points if x > median], axis=0)
    return [(x > threshold * pos_median) for x in points]

def neighbor(points, i, nb_size):
    if len(points) < nb_size:
        return points
    else:
        start = max(i - nb_size/2, 0)
        return points[start: start + nb_size]
        
def min_max_box(points, mildness):
    q1 = np.percentile(points, 25)
    q2 = np.percentile(points, 50)
    q3 = np.percentile(points, 75)
    if mildness == 'mild':
        return q1 - 1.5 * (q2 - q1), q3 + 1.5 * (q3 - q2)
    elif mildness == 'extreme':
        return q1 - 3 * (q2 - q1), q3 + 3 * (q3 - q2)
# source: 
# http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm
def box_based_roll_outlier(points, nb_size=1000):
    length = len(points)
    outlier_mild = []
    outlier_extreme = []
    for i in range(length):
        nb = neighbor(points, i, nb_size)
        # print i
        lower, upper = min_max_box(nb, 'mild')
        indicator = points[i] < lower or points[i] > upper
        outlier_mild.append(indicator)
        lower, upper = min_max_box(nb, 'extreme')
        indicator = points[i] < lower or points[i] > upper
        outlier_extreme.append(indicator)
    return outlier_mild, outlier_extreme

def pos_p60_based_roll_outlier(points, nb_size=1000):
    length = len(points)
    outlier = []
    print length
    for i in range(length):
        nb = neighbor(points, i, nb_size)
        # print i
        lower = 0
        positive = [x for x in nb if x > 0]
        p60 = np.percentile(positive, 90, axis=0)
        pos_median = np.median([x for x in nb if x > p60], axis=0)
        upper = 1.5 * pos_median
        indicator = points[i] < lower or points[i] > upper
        outlier.append(indicator)
    return outlier

def median_based_outlier(points, threshold=50):
    median = np.median(points, axis=0)
    return [(x > threshold * median) for x in points]

def percentile_based_outlier(data, threshold=99.99):
    diff = (100 - threshold) / 2.0
    minval, maxval = np.percentile(data, [diff, 100 - diff])
    return (data < minval) | (data > maxval)

def oneside_percentile_based_outlier(data, threshold=95):
    maxval = np.percentile(data, [threshold])
    return (data > maxval)

def plot(x):
    fig, axes = plt.subplots(nrows=2)
    for ax, func in zip(axes, [percentile_based_outlier, mad_based_outlier]):
        sns.distplot(x, ax=ax, rug=True, hist=False)
        outliers = x[func(x)]
        ax.plot(outliers, np.zeros_like(outliers), 'ro', clip_on=False)

    kwargs = dict(y=0.95, x=0.05, ha='left', va='top')
    axes[0].set_title('Percentile-based Outliers', **kwargs)
    axes[1].set_title('MAD-based Outliers', **kwargs)
    fig.suptitle('Comparing Outlier Tests with n={}'.format(len(x)), size=14)
    plt.show()

def show_outlier(points, b, method, measure_type, threshold, plot=False):
    if method == 'box':
        outliers_mild, outliers_extreme = box_based_roll_outlier(points, nb_size=500)
        return outliers_mild, outliers_extreme
    elif method == 'pos_roll':
        outliers = pos_p60_based_roll_outlier(points, nb_size=200)
    elif method == 'upper':
        outliers = pos_p60_based_outlier(points, threshold)
    elif method == 'percentile':
        outliers = percentile_based_outlier(points, threshold=99.9)
    elif method == 'oneside_percentile':
        outliers = oneside_percentile_based_outlier(points,
                                                    threshold=99.9)
    elif method == 'mad':
        outliers = mad_based_outlier(points)
    elif method == 'median':
        outliers = median_based_outlier(points)
    outlier_plot = [0 if x else np.nan for x in outliers]
    if plot:
        plt.plot(range(len(points)), (outlier_plot), 'ro', clip_on=False)
        plt.plot(range(len(points)), points)
        n_removed = len([x for x in outliers if x])
        n_total = len(points)
        print 'remove: {0}, total{1}'.format(n_removed, n_total)
        plt.title('# remove {0}, # total {1} ({2:.2%})'.format(n_removed, n_total, 1.0 * n_removed/n_total))
        path = os.getcwd() + '/input/FY/interval/ion_0627/outlier/{0}_{1}.png'.format(b, measure_type)
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()
    # print outliers[:5]
    return outliers

def plot_piece(gr, ax, title, color, measure_type, b, s, scatter=True,
               annote=False, jsondir=None, csvdir=None, season=None):
    # print season, '1111111111111111'
    group = gr.get_group(title)
    temp = group.reset_index()
    d = compute_piecewise(measure_type, temp, b, s)
    if d is None:
        return None
    x0 = d['x_range'][0]
    x1 = d['x_range'][1]
    if type(d['breakpoint']) == tuple:
        b0 = d['breakpoint'][0]
        b1 = d['breakpoint'][1]
        x = np.array([x0, b0, b1, x1])
    else:
        x = np.array([x0, d['breakpoint'], x1])
    if scatter:    
        ax.plot(d['x'], d['y'], 'o', c=color)
    y = d['fun'](x, *d['regression_par'])
    ax.plot(x, y, c='salmon')
    if annote:
        if measure_type == 'electric':
            ax.annotate(b, xy=(x[-1], y[-1]))
        else:
            ax.annotate(b, xy=(x[0], y[0]))
    ax.set_ylabel(lb.ylabel_dict[measure_type])
    if not jsondir is None:
        d_plot = {}
        d_plot['name'] = b
        x = map(lambda m: round(m, 4), x)
        y = map(lambda m: round(m, 4), y)
        d_plot['data'] = map(list, zip(x, y)) 
        if season is None:
            path = '{0}{1}_{2}.json'.format(jsondir, b, measure_type)
        else:
            path = '{0}{1}_{2}_{3}.json'.format(jsondir, b, measure_type, season)
        with open (path, 'w+') as wt:
            json.dump(d_plot, wt)
    if not csvdir is None:
        x = map(lambda m: round(m, 2), x)
        y = map(lambda m: round(m, 2), y)
        df = pd.DataFrame({'x': x, 'y': y})
        df['id'] = b
        df.to_csv('{0}{1}_{2}.csv'.format(csvdir, b, measure_type), index=False)
    return d
    
# saving of d0 under d1 condition
def compute_saving(d_active, d_rest, sum_other):
    if d_active is None or d_rest is None:
        return 0
    y_active = d_active['y']
    y_rest = d_rest['y']
    y_rest_hat = d_active['fun'](d_rest['x'], *d_active['regression_par'])
    s = 1 - (sum(y_rest) + sum(y_active) + sum_other)/(sum(y_rest_hat) + sum(y_active) + sum_other)
    return s 

def compute_saving_setback(d_active, d_rest, sum_other):
    if d_active is None or d_rest is None:
        return 0
    y_active = d_active['y']
    y_rest = d_rest['y']
    y40_rest = d_rest['fun'](np.array([40.0]), *d_rest['regression_par'])[0]
    y40_active = d_active['fun'](np.array([40.0]), *d_active['regression_par'])[0]
    offset = y40_active - y40_rest
    y_rest_hat = d_active['fun'](d_rest['x'], *d_active['regression_par'])
    # print 'offset', offset
    # print 'y_rest_hat[0] before', y_rest_hat[0]
    y_rest_hat = y_rest_hat - offset
    # print 'y_rest_hat[0] after', y_rest_hat[0]
    s = 1 - (sum(y_rest) + sum(y_active) + sum_other)/(sum(y_rest_hat) + sum(y_active) + sum_other)
    return offset, s 

def plot_outlier(measure_type):
    conn = uo.connect('interval_ion')
    with conn:
        df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
    bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    col = value_lb_dict[measure_type]
    print len(bs_pair)
    for b, s in bs_pair[:1]:
        print b, s
        with conn:
            df = pd.read_sql('SELECT * FROM {0} WHERE Building_Number = \'{1}\''.format(measure_type, b), conn)
        df = df[df[col] >= 0]
        points = df[col]
        outliers_mild, outliers_extreme = box_based_roll_outlier(points, nb_size=1000)
        maxi = max(points)
        df['outlier_mild'] = map(lambda x: maxi * 0.5 if x else np.nan, outliers_mild)
        df['outlier_extreme'] = map(lambda x: maxi * 0.75 if x else np.nan, outliers_extreme)
        df.to_csv(homedir + 'temp/{0}.csv'.format(b), index=False)
    return

def remove_outliers(measure_type):
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    col = value_lb_dict[measure_type]
    conn = uo.connect('interval_ion')
    dfs = []
    with conn:
        # df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
        df_id = pd.read_sql('SELECT * FROM {0}_id'.format(measure_type), conn)
    # bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    # bs_pair = [x for x in bs_pair if x[0] == 'AL0039AB']
    ids = df_id['id']
    for i, b in enumerate(ids):
        print i, b
        with conn:
            df = pd.read_sql('SELECT * FROM {0} WHERE Building_Number = \'{1}\''.format(measure_type, b), conn)
        print df.head()
        # df = df.head(n = 5000)
        # df.info()
        points = df[col]
        outliers = show_outlier(points, b, 'upper', measure_type, 1.5)
        # outliers = show_outlier(points, b, 'pos_roll', measure_type, 1.5)
        # mild, outliers = show_outlier(points, b, 'box', measure_type, 1.5)
        df['outlier'] = outliers
        print len([x for x in outliers if x])
        dfs.append(df)
    df_all = pd.concat(dfs, ignore_index=True)
    print df_all.head()
    with conn:
        df_all.to_sql('{0}_outlier_tag'.format(measure_type),
                      conn, if_exists='replace')
    return

def fit_time(measure_type, occtime, season=None):
    conn = uo.connect('interval_ion')
    with conn:
        df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
        df_area = pd.read_sql('SELECT * FROM area', conn)
        df_tz = pd.read_sql('SELECT Building_Number, rawOffset FROM EUAS_timezone', conn)
    df_tz.set_index('Building_Number', inplace=True)
    df_area.set_index('Building_Number', inplace=True)
    bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    multiplier_dict = {'electric':  3.412, 'gas': 1.026}
    col = value_lb_dict[measure_type]
    m = multiplier_dict[measure_type]
    ylabel = {'electric': 'electric (kBtu/sq.ft)', 'gas': 'gas kBtu/sq.ft'}
    print len(bs_pair)
    sns.set_style("whitegrid")
    # palette = sns.cubehelix_palette(len(bs_pair))
    palette = sns.color_palette('husl', len(bs_pair))
    sns.set_palette(palette)
    colors_rgb = [util.float2hex(x) for x in palette]
    sns.set_context("talk", font_scale=1)
    jsondir = os.getcwd() + '/input/FY/interval/ion_0627/piecewise_all/json_{0}/'.format(occtime)
    # csvdir = os.getcwd() + '/input/FY/interval/ion_0627/piecewise_all/csv/'
    for i, (b, s) in enumerate(bs_pair):
        print b, s
        try:
            area = df_area.ix[b, 'Gross_Sq.Ft']
        except KeyError:
            print 'No area found'
            continue
        offset = df_tz.loc[b, 'rawOffset']
        df = join_interval(offset, b, s, area, col, m, measure_type, conn, season=season)
        df.to_csv(homedir + 'temp/{0}.csv'.format(b))
        df = df[df[col] >= 0]
        points = df[col]
        outliers = show_outlier(points, b, 'upper', measure_type, 1.5)
        df['outlier'] = outliers
        df = df[~np.array(outliers)]
        df['status_week_day_night'] = \
            df.apply(lambda r: util.get_status(r['hour'], r['day']), axis=1)
        min_time = df['Timestamp'].min()
        max_time = df['Timestamp'].max()
        gr = df.groupby('status_week_day_night')
        bx = plt.axes()
        d0 = plot_piece(gr, bx, occtime, colors_rgb[i], measure_type, b, s, scatter=False, annote=True, jsondir=jsondir, season=season)
    plt.xlabel('Temperature_F')
    # plt.show()
    if season is None:
        path = os.getcwd() + '/input/FY/interval/ion_0627/piecewise_all/{0}_{1}.png'.format(measure_type, occtime)
    else:
        path = os.getcwd() + '/input/FY/interval/ion_0627/piecewise_all/{0}_{1}_{2}.png'.format(measure_type, occtime, season)
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    shutil.copy(path, path.replace('input/FY/interval/ion_0627/piecewise_all', 'plot_FY_weather/html/interval/lean/all'))
    plt.close()
    return
    
def fit(measure_type, year=None, season=None):
    conn = uo.connect('interval_ion')
    with conn:
        df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
        df_area = pd.read_sql('SELECT * FROM area', conn)
        df_tz = pd.read_sql('SELECT Building_Number, rawOffset FROM EUAS_timezone', conn)
    df_tz.set_index('Building_Number', inplace=True)
    df_area.set_index('Building_Number', inplace=True)
    bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 2)
    sns.set_context("talk", font_scale=1)
    # col_wrap_dict = {'hour': 6, 'month': 4, 'day': 5, 'status':2}
    # upper = {'electric': 600, 'gas': 2500}
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    multiplier_dict = {'electric':  3.412, 'gas': 1.026}
    col = value_lb_dict[measure_type]
    m = multiplier_dict[measure_type]
    ylabel = {'electric': 'electric (kBtu/sq.ft)', 'gas': 'gas kBtu/sq.ft'}
    # test = ['TN0088ZZ', 'TX0057ZZ', 'NY0281ZZ', 'NY0304ZZ', 'MO0106ZZ']
    # test = ['FL0067ZZ']
    # bs_pair = [x for x in bs_pair if x[0] in test]
    lines = ['Building_Number,week night save%,weekend day save%,weekend night save%,aggregate save%,CVRMSE week day,CVRMSE week night,CVRMSE weekend day,CVRMSE weekend night']
    # bs_pair = bs_pair[:1]
    print len(bs_pair)
    for b, s in bs_pair:
        print b, s
        try:
            area = df_area.ix[b, 'Gross_Sq.Ft']
        except KeyError:
            print 'No area found'
            continue
        offset = df_tz.loc[b, 'rawOffset']
        df = join_interval(offset, b, s, area, col, m, measure_type, conn, year, season)
        if len(df) == 0:
            continue
        df.to_csv(homedir + 'temp/{0}.csv'.format(b))
        df = df[df[col] >= 0]
        points = df[col]
        # outliers = show_outlier(points, b, 'upper', measure_type, 5)
        outliers = show_outlier(points, b, 'upper', measure_type, 1.5)
        df['outlier'] = outliers
        df = df[~np.array(outliers)]
        df['status_week_day_night'] = \
            df.apply(lambda r: util.get_status(r['hour'], r['day']), axis=1)
        min_time = df['Timestamp'].min()
        max_time = df['Timestamp'].max()
        sns.set_style("whitegrid")
        colors = sns.color_palette('Paired', 16)
        colors_rgb = [util.float2hex(x) for x in colors]
        sns.set_context("talk", font_scale=1)
        gr = df.groupby('status_week_day_night')
        f, axarr = plt.subplots(2, 2, sharex=True, sharey=True)
        d0 = plot_piece(gr, axarr[0, 0], 'week day', colors_rgb[0], measure_type, b, s)
        if not d0 is None:
            axarr[0, 0].set_title('{0} ({3})\nbreak point {1}F, CV(RMSE): {2:.3f}'.format('week day', d0['breakpoint'], d0['CV(RMSE)'], b))
        d1 = plot_piece(gr, axarr[0, 1], 'week night', colors_rgb[1], measure_type, b, s)
        x0 = d0['x_range'][0]
        x1 = d0['x_range'][1]
        if type(d0['breakpoint']) == tuple:
            b0 = d['breakpoint'][0]
            b1 = d['breakpoint'][1]
            x = np.array([x0, b0, b1, x1])
        else:
            x = np.array([x0, d0['breakpoint'], x1])
        y = d0['fun'](x, *d0['regression_par'])
        d2 = plot_piece(gr, axarr[1, 0], 'weekend day', colors_rgb[2], measure_type, b, s)
        d3 = plot_piece(gr, axarr[1, 1], 'weekend night', colors_rgb[3], measure_type, b, s)
        axarr[0, 1].plot(x, y, ls='--', c='red')
        axarr[1, 0].plot(x, y, ls='--', c='red')
        axarr[1, 1].plot(x, y, ls='--', c='red')
        plt.ylim((0, 0.02))
        plt.xlim((0, 100))
        save, err = compute_saving_all(b, d0, d1, d2, d3, axarr)
        # save, err = compute_saving_all_setback(d0, d1, d2, d3, axarr)
        plt.suptitle('{0} -- {1}'.format(min_time, max_time))
        f.text(0.5, 0.04, 'Temperature_F', ha='center', va='center')
        if year is None:
            if season is None:
                path = os.getcwd() + '/input/FY/interval/ion_0627/piecewise/{1}/{0}_{1}.png'.format(b, measure_type)
            else:
                path = os.getcwd() + '/input/FY/interval/ion_0627/piecewise/{1}/{0}_{1}_{2}.png'.format(b, measure_type, season)
        else:
            path = os.getcwd() + '/input/FY/interval/ion_0627/piecewise/{1}/{0}_{1}_{2}.png'.format(b, measure_type, int(year))
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        shutil.copy(path, path.replace('input/FY/interval/ion_0627/piecewise', 'plot_FY_weather/html/interval/lean'))
        plt.close()
        lines.append(','.join([b] + save + err))
    if year is None:
        if season is None:
            table_path = os.getcwd() + '/input/FY/interval/ion_0627/table/{0}_save.csv'.format(measure_type)
        else:
            table_path = os.getcwd() + '/input/FY/interval/ion_0627/table/{0}_save_{1}.csv'.format(measure_type, season)
    else:
        table_path = os.getcwd() + '/input/FY/interval/ion_0627/table/{0}_save_{1}.csv'.format(measure_type, int(year))
    with open(table_path, 'w+') as wt:
        wt.write('\n'.join(lines))
    return
   
def compute_saving_all_setback(d0, d1, d2, d3, axarr):
    if None in [d0, d1, d2, d3]:
        return
    save = []
    err = []
    err.append('{0:.3f}'.format(d0['CV(RMSE)']))
    o1, save_percent = compute_saving_setback(d0, d1, sum(d2['y']) + sum(d3['y']))
    axarr[0, 1].set_title('{0}\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('week night', d1['breakpoint'], d1['CV(RMSE)'], save_percent))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d1['CV(RMSE)']))
    o2, save_percent = compute_saving_setback(d0, d2, sum(d1['y']) + sum(d3['y']))
    axarr[1, 0].set_title('{0}\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('weekend day', d2['breakpoint'], d2['CV(RMSE)'], save_percent))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d2['CV(RMSE)']))
    o3, save_percent = compute_saving_setback(d0, d3, sum(d1['y']) + sum(d2['y']))
    axarr[1, 1].set_title('{0}\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('weekend night', d3['breakpoint'], d3['CV(RMSE)'], save_percent))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d3['CV(RMSE)']))
    actual = sum([sum(d['y']) for d in [d0, d1, d2, d3]])
    est = sum([sum(d0['fun'](d['x'], *d0['regression_par']) - o) for
               o, d in zip ([o1, o2, o3], [d1, d2, d3])]) + sum(d0['y'])
    print actual, est
    save_percent = 1 - actual/est
    save.append('{0:.2%}'.format(float(save_percent)))
    return save, err

def compute_saving_all(b, d0, d1, d2, d3, axarr):
    if None in [d0, d1, d2, d3]:
        return
    save = []
    err = []
    err.append('{0:.3f}'.format(d0['CV(RMSE)']))
    save_percent = compute_saving(d0, d1, sum(d2['y']) + sum(d3['y']))
    axarr[0, 1].set_title('{0} ({4})\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('week night', d1['breakpoint'], d1['CV(RMSE)'], save_percent, b))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d1['CV(RMSE)']))
    save_percent = compute_saving(d0, d2, sum(d1['y']) + sum(d3['y']))
    axarr[1, 0].set_title('{0} ({4})\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('weekend day', d2['breakpoint'], d2['CV(RMSE)'], save_percent, b))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d2['CV(RMSE)']))
    save_percent = compute_saving(d0, d3, sum(d1['y']) + sum(d2['y']))
    axarr[1, 1].set_title('{0} ({4})\nbreak point {1}F, CV(RMSE): {2:.3f}, save {3:.2%}'.format('weekend night', d3['breakpoint'], d3['CV(RMSE)'], save_percent, b))
    save.append('{0:.2%}'.format(save_percent))
    err.append('{0:.3f}'.format(d3['CV(RMSE)']))
    actual = sum([sum(d['y']) for d in [d0, d1, d2, d3]])
    est = sum([sum(d0['fun'](d['x'], *d0['regression_par'])) for d in
               [d1, d2, d3]]) + sum(d0['y'])
    print actual, est
    save_percent = 1 - actual/est
    save.append('{0:.2%}'.format(float(save_percent)))
    return save, err

def temp():
    conn = uo.connect('interval_ion')
    with conn:
        df = pd.read_sql('SELECT * FROM area', conn)
    df.to_csv(homedir + 'temp/area.csv')
    
def read_interval_building(b):
    conn = uo.connect('interval_ion')
    with conn:
        df = pd.read_sql('SELECT * FROM electric WHERE Building_Number = \'{0}\''.format(b), conn)
    df.to_csv(homedir + 'temp/{0}_int.csv'.format(b))
    
# TODO: modify this for "electric.html" and "gas.html"
def process_html(measure_type, year=None):
    with open (os.getcwd() + '/plot_FY_weather/html/interval/lean/template.html', 'r') as rd:
        lines = rd.readlines()
    if year is None:
        measureTypeYear = measure_type
    else:
        measureTypeYear = '{0}_{1}'.format(measure_type, year)
    for i, line in enumerate(lines):
        if 'start' in line:
            start_id = i
        elif 'end' in line:
            end_id = i
        lines[i] = lines[i].replace('measure_type', measure_type)
        lines[i] = lines[i].replace('measureTypeYear', measureTypeYear)
    print start_id, end_id
    if year is None:
        files = glob.glob(os.getcwd() + '/plot_FY_weather/html/interval/lean/{0}/*_{0}.png'.format(measure_type))
    else:
        files = glob.glob(os.getcwd() + '/plot_FY_weather/html/interval/lean/{0}/*_{0}_{1}.png'.format(measure_type, year))
    to_replace = lines[start_id + 1: end_id]
    newlines = []
    for f in files:
        building = f[f.rfind('/') + 1:f.rfind('/') + 9]
        print building
        for x in to_replace:
            newlines.append(x.replace('WY0029ZZ', building))
            print x
            print x.replace('WY0029ZZ', building)
    final = lines[:start_id] + newlines + lines[end_id + 1:]
    if year is None:
        outfile = os.getcwd() + '/plot_FY_weather/html/interval/lean/{0}.html'.format(measure_type)
    else:
        outfile = os.getcwd() + '/plot_FY_weather/html/interval/lean/{0}_{1}.html'.format(measure_type, year)
    with open(outfile, 'w+') as wt:
        wt.write(''.join(final))
    return
    
def process_index_dygraph(measure_type, dirname, outname):
    with open (os.getcwd() + '/plot_FY_weather/html/interval/trend/template.html', 'r') as rd:
        lines = rd.readlines()
    for i, line in enumerate(lines):
        if 'href' in line:
            replace_idx = i
        lines[i] = lines[i].replace('measure_type', measure_type)
    to_replace = lines[replace_idx]
    files = glob.glob(os.getcwd() + '/{1}/{1}/*_{0}.html'.format(measure_type, dirname))
    newlines = []
    for f in files:
        building_name = f[f.rfind('/') + 1: f.rfind('_')]
        newlines.append(to_replace.replace('building', building_name))
    result = lines[:replace_idx] + newlines + lines[replace_idx + 1:]
    with open(os.getcwd() + '/plot_FY_weather/html/interval/{1}/{0}.html'.format(measure_type, outname), 'w+') as wt:
        wt.write(''.join(result))

def hourly_trend():
    # hourly trend dygraph 
    files = glob.glob(os.getcwd() + '/plot_interval_hour/plot_interval_hour/*.html')
    for f in files:
        shutil.copyfile(f,
                        f.replace('/plot_interval_hour/plot_interval_hour/',
                                  '/plot_FY_weather/html/interval/trend_hour/'))
    process_index_dygraph('gas', 'plot_interval_hour', 'trend_hour')
    process_index_dygraph('electric', 'plot_interval_hour', 'trend_hour')
    return

def cmp_euas():
    for measure_type in ['electric', 'gas']:
        uo.dir2html(os.getcwd() + \
                    '/input/FY/interval/ion_0627/cmp_euas/',
                    '*_{0}.png'.format(measure_type), 
                    'ION vs EUAS: {0}'.format(measure_type.title()),
                    '{0}_cmp.html'.format(measure_type), 
                    templatepath='/css_template/01-bootstrap-kickoff-template/index.html',
                    assetdir='../', style='width:45%',
                    withname=False)
    files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/cmp_euas/*')
    for f in files:
        shutil.copyfile(f, f.replace('/input/FY/interval/ion_0627/', '/plot_FY_weather/html/interval/'))

def plot_csv(dirname, measure_type):
    files = glob.glob(os.getcwd() + \
                      '/input/FY/interval/ion_0627/{0}/csv/*_{1}.csv'.format(dirname, measure_type))
    dfs = [pd.read_csv(f) for f in files]
    df_all = pd.concat(dfs, ignore_index=True)
    df_p = df_all.pivot(index='x', columns='id', values='y')
    csv_data = os.getcwd() + '/input/FY/interval/ion_0627/{0}/{1}.csv'.format(dirname, measure_type)
    df_p.to_csv(csv_data, index=True)
    with open (os.getcwd() + '/input/FY/interval/ion_0627/{0}/template-dy.html'.format(dirname, measure_type), 'r') as rd:
        lines = rd.readlines()
    for i in range(len(lines)):
        lines[i] = lines[i].replace('filename', '{0}.csv'.format(measure_type))
    with open (os.getcwd() + '/input/FY/interval/ion_0627/{0}/{1}-dy.html'.format(dirname, measure_type), 'w+') as wt:
        wt.write(''.join(lines))
    print 'end'

def plot_json(dirname, measure_type, occtime, season=None, subset=None):
    if season is None:
        files = glob.glob(os.getcwd() + \
                        '/input/FY/interval/ion_0627/{0}/json_{1}/*_{2}.json'.format(dirname, occtime, measure_type))
    else:
        files = glob.glob(os.getcwd() + \
                          '/input/FY/interval/ion_0627/{0}/json_{1}/*_{2}_{3}.json'.format(dirname, occtime, measure_type, season))
    def get_name(string):
        idx = string.rfind('/')
        return string[idx + 1: idx + 9]
    if not subset is None: 
        df = pd.read_csv(os.getcwd() + '/input/FY/interval/ion_0627/table/electric_save_lowerr.csv')
        study = set(df['Building_Number'].tolist())
        files = [x for x in files if get_name(x) in study]
    data = []
    for x in files:
        with open (x, 'r') as rd:
            j = json.load(rd)
        data.append(j)
    data_str = 'series: [{0}]'.format(','.join(map(str, data)))
    data_str = data_str.replace('u\'', '\'')
    with open (os.getcwd() + '/input/FY/interval/ion_0627/{0}/template.html'.format(dirname, measure_type), 'r') as rd:
        lines = rd.readlines()
    if season is None:
        mytitle = 'Hourly {0} (kBtu/sq.ft) vs Temperature (F), {1}'.format(measure_type, occtime)
    else:
        mytitle = '{2} Hourly {0} (kBtu/sq.ft) vs Temperature (F), {1}'.format(measure_type, occtime, season.title())
    for i in range(len(lines)):
        lines[i] = lines[i].replace('series: []', data_str)
        lines[i] = lines[i].replace('Mytitle', mytitle)
        lines[i] = lines[i].replace('Myylabel', 'kBtu/sq.ft'.format(measure_type, occtime))
    if season is None:
        f = os.getcwd() + '/input/FY/interval/ion_0627/{0}/{1}_{2}.html'.format(dirname, measure_type, occtime)
    else:
        if subset is None:
            f = os.getcwd() + '/input/FY/interval/ion_0627/{0}/{1}_{2}_{3}.html'.format(dirname, measure_type, occtime, season)
        else:
            f = os.getcwd() + '/input/FY/interval/ion_0627/{0}/{1}_{2}_{3}_{4}.html'.format(dirname, measure_type, occtime, season, subset)
    with open (f, 'w+') as wt:
        wt.write(''.join(lines))
    shutil.copyfile(f, f.replace('/input/FY/interval/ion_0627/', '/plot_FY_weather/html/interval/'))
    print 'end'
    return
    
def create_summary_daynightlean():
    files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/table/*.csv')
    for f in files:
        df = pd.read_csv(f)
        df['sortby'] = df['aggregate save%'].map(lambda x:
                                                 float(x[:-1]))
        df.sort('sortby', ascending=False, inplace=True)
        df_out = df.copy()
        df_out.drop('sortby', axis=1, inplace=True)
        df_out.to_csv(f, index=False)
    for f in files:
        uo.csv2html(f)
    files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/table/*.html')
    for f in files:
        shutil.copyfile(f, f.replace('/input/FY/interval/ion_0627/',
                                     '/plot_FY_weather/html/interval/'))
    return
    
def plot_saving_oneplot(occtime, season=None):
    for measure_type in ['electric', 'gas']:
        fit_time(measure_type, occtime, season=season)
        plot_json('piecewise_all', measure_type, occtime, season=season, subset='lowerr')
    
def get_low_err_lean(name=None):
    files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/table/*.csv')
    files = [f for f in files if name in f]
    for f in files:
        df = pd.read_csv(f)
        df = df[df['CVRMSE week day'] < 0.35]
        df.to_csv(f.replace('.csv', '_lowerr.csv'), index=False)
    return
    
def lean():
    # fit('gas')
    # fit('electric')
    # fit('gas', year=2014)
    # fit('gas', year=2015)
    # fit('electric', year=2014)
    # fit('electric', year=2015)
    # fit('electric', season='winter')
    # fit('electric', season='summer')
    # get_low_err_lean(name='electric_save.csv')
    # create_summary_daynightlean()
    # process_html('electric')
    # process_html('gas')
    # process_html('electric', year=2014)
    # process_html('gas', year=2014)
    # process_html('electric', year=2015)
    # process_html('gas', year=2015)
    return

def main():
    measure_type = 'electric'
    build_energy_temperature(measure_type)
    # lean()
    # remove_outliers('electric')
    # remove_outliers('gas')
    # plot_saving_oneplot('week day', season='winter')
    # plot_saving_oneplot('weekend day', season='winter')
    # plot_saving_oneplot('week day')
    # plot_saving_oneplot('week night')
    # plot_saving_oneplot('weekend day')
    # plot_saving_oneplot('weekend night')
    # plot_csv('piecewise_all', 'electric')
    # read_interval_building('NM0050ZZ')
    # read_interval_building('LA0085ZZ')
    # temp()
    # copy outlier files
    # fit('gas')
    # uo.dir2html('/media/yujiex/work/SEED/gitDir/SEEDproject/Code/merge/input/FY/interval/ion_0627/outlier/', '*_gas.png', 'Gas Outlier', 'gas_outlier.html')
    # files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/outlier/*')
    # for f in files:
    #     shutil.copyfile(f, f.replace('/input/FY/interval/ion_0627/', '/plot_FY_weather/html/interval/'))
    # fit('electric')
    # uo.dir2html('/media/yujiex/work/SEED/gitDir/SEEDproject/Code/merge/input/FY/interval/ion_0627/outlier/', '*_electric.png', 'Electric Outlier', 'electric_outlier.html')
    # print 'end'
    # process_html('electric')
    # process_html('gas')
    # process_index_dygraph('electric')
    # use R to plot dygraphs
    # files = glob.glob(os.getcwd() + '/plot_interval/plot_interval/*.html')
    # for f in files:
    #     shutil.copyfile(f, f.replace('/plot_interval/plot_interval/', '/plot_FY_weather/html/interval/trend/'))
    # process_index_dygraph('gas', 'plot_interval', 'trend')
    # process_index_dygraph('electric', 'plot_interval', 'trend')
    # hourly_trend()

    # cmp_euas()
    return

main()
