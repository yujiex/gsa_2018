# cross validation implementation: use two year of data to predict the other year
import pandas as pd
import os
import glob
import numpy as np
import util
import get_building_set as gbs
import lean_temperature_monthly as ltm
import lean_dd as ld
import lean_vv as vv
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
from datetime import datetime

weatherdir = os.getcwd() + '/csv_FY/weather/'
interval_daily = os.getcwd() + '/input/FY/interval/single/'
interval_dir = os.getcwd() + '/input/FY/interval/'
my_dpi = 150

def get_bs(filename):
    filename = filename[:-4]
    tokens = filename.split('_')
    return tokens[1], tokens[2]

def get_pair(prefix):
    files = glob.glob(weatherdir +
                      'dd_temp_eng/{0}*.csv'.format(prefix))
    pairs = [get_bs(util.get_filename(x)) for x in files] 
    return pairs

def get_partitions(nfold, df):
    length = len(df)
    df['id'] = range(length)
    step = length / nfold
    def partition(df, i, step):
        df1 = df[~df['id'].isin(range(i * step, (i + 1) * step))]
        df2 = df[df['id'].isin(range(i * step, (i + 1) * step))]
        return df1, df2
    df_pairs = [partition(df, i, step) for i in range(nfold)]
    return df_pairs

def cv(method, theme, kind):
    no_invest = gbs.get_no_invest_set()
    hdd_pair = get_pair('HDD')
    print hdd_pair[0]
    hdd_pair = [x for x in hdd_pair if x[0] in no_invest]
    print len(hdd_pair)
    n_par_temp = 2
    lines = []
    if theme == 'eui_elec':
        base_temp = '55F'
    else:
        base_temp = '65F'
    lines.append('Building Number,CVRMSE_{0}'.format(method))
    for p in hdd_pair:
        b = p[0]
        s = p[1]
        df = pd.read_csv(weatherdir + 'dd_temp_eng/{0}_{1}_{2}.csv'.format(kind, b, s))
        if len(df) < 36:
            continue
        df = df.tail(n=36)
        df_pairs = get_partitions(3, df)
        errs = []
        for x in df_pairs:
            df_train = x[0]
            if df_train[theme].sum() == 0:
                continue
            df_test = x[1]
            y = np.array(df_test[theme])
            if method == 'temperature':
                x = np.array(df_test[s])
                d = ltm.piecewise_reg_one(b, s, n_par_temp, theme, None, df_train)
                y_hat = d['fun'](x, *(d['regression_par']))
            elif method == 'dd':
                par_list = ld.opt_lireg(b, s, df_train, kind, theme, None)
                x = df_test[par_list[-1]]
                fun = lambda x: par_list[0] * x + par_list[1]
                y_hat = x.map(fun)
            elif method == 'vv':
                par_list = vv.lean(b, s, df_train, theme, base_temp)
                df_temp = df_test.copy()
                df_temp = df_temp[['month', base_temp]]
                df_perdd = par_list[0]
                df_perdd.reset_index(inplace=True)
                df_mg = pd.merge(df_test, df_perdd, left_on='month', right_index=True)
                y = df_mg[theme]
                y_hat = df_mg[base_temp] * df_mg['y_per_dd'] + par_list[-1]
            cvrmse = util.CVRMSE(y, y_hat, n_par_temp)
            print cvrmse
            errs.append(cvrmse)
        line = '{0},{1}'.format(b, (np.array(errs)).mean())
        print line
        lines.append(line)
    with open(weatherdir + 'cv_{0}_{1}.csv'.format(method, theme), 'w+') as wt:
        wt.write('\n'.join(lines))
    return

def load_data_ion(b, s, npar, theme, timestep, timerange):
    if timestep == 'D':
        df_e = pd.read_csv(interval_daily + '{0}_{1}_D.csv'.format(b, theme))
    elif timestep == 'H':
        df_e = pd.read_csv(interval_dir + 'single_hourly/{0}.csv'.format(b))
    df_e.info()
    dates = pd.to_datetime(df_e['Date'])
    dayOfWeek = pd.DatetimeIndex(dates).dayofweek
    days = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    fun = lambda x: days[x]
    fun = np.vectorize(fun)
    dayOfWeek_str = fun(dayOfWeek)
    if timestep == 'D':
        df_e.set_index(dates, inplace=True)
    elif timestep == 'H':
        time_str = df_e['Timestamp'].map(lambda x: x[:18])
        time_idx = pd.to_datetime(time_str)
        df_e.set_index(time_idx, inplace=True)
    print len(df_e)
    # fixme, timerange
    # df_e = df_e[df_e['Date'] < np.datetime64('2013-09-01')]
    print len(df_e)
    minDate = dates.min()
    maxDate = dates.max()
    print minDate, maxDate
    minDate_str = minDate.strftime('%Y-%m-%d %H:%M:%S')
    maxDate_str = maxDate.strftime('%Y-%m-%d %H:%M:%S')
    print minDate_str, maxDate_str
    df_w = ltm.get_weather_data(s, minDate_str, maxDate_str, timestep)
    df = pd.merge(df_e, df_w, left_index=True, right_index=True, how='left')
    df['day'] = dayOfWeek_str
    df['hour'] = df['Timestamp'].map(lambda x: int(x[11:13]))
    df['year'] = df['Timestamp'].map(lambda x: int(x[:4]))
    df['month'] = df['Timestamp'].map(lambda x: int(x[5:7]))
    df_plot = df
    df_plot = df_plot[df_plot[theme + ' (kBtu)'] >= 0]
    if timestep == 'H':
        sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), data=df_plot,
                fit_reg=False)
        plt.ylim((0, 4000))
        # plt.gca().set_ylim(bottom=0)
        # plt.show()
        image_output_dir = os.getcwd() + '/plot_FY_weather/html/single_building/lean_interval/'
        P.savefig('{0}scatter_{1}_{2}_{3}_{4}_ori.png'.format(image_output_dir, b, s, theme, timestep), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()
        sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), hue='day',
                col='hour', col_wrap=6, size=3, data=df_plot,
                fit_reg=False)
        plt.ylim((0, 4000))
        # plt.gca().set_ylim(bottom=0)
        P.savefig('{0}scatter_{1}_{2}_{3}_{4}.png'.format(image_output_dir, b, s, theme, timestep), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()

        # for year in [2014, 2015]:
        #     df_plot = df[(df['day'] == 'Tue') & (df['year'] == year)]
        #     sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), hue='month',
        #             col='hour', col_wrap=6, size=3, data=df_plot, fit_reg=False)
        #     plt.gca().set_ylim(bottom=0)
        #     P.savefig('{0}scatter_{1}_{2}_{3}_{4}_{5}_Tue.png'.format(image_output_dir, b, s, theme, timestep, year), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        #     plt.close()

    if theme == 'Gas':
        df_reg = df.rename(columns={'{0} (kBtu)'.format(theme): 'eui_gas', 'Date': 'timestamp'})
        df_reg['day'] = dayOfWeek_str
        ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, timerange, df_reg)
    elif theme == 'Electric':
        df_reg = df.rename(columns={'{0} (kBtu)'.format(theme): 'eui_elec', 'Date': 'timestamp'})
        df_reg['day'] = dayOfWeek_str
        df_reg = df_reg[df_reg['day'] != 'Sat']
        df_reg = df_reg[df_reg['day'] != 'Sun']
        ltm.piecewise_reg_one(b, s, npar, 'eui_elec', False, timerange, df_reg)
    # # plt.show()
    return
def load_data(b, s, npar, theme, timestep, timerange):
    if timestep == 'D':
        df_e = pd.read_csv(interval_daily + '{0}_{1}_D.csv'.format(b, theme))
    elif timestep == 'H':
        df_e = pd.read_csv(interval_dir + 'single_hourly/{0}.csv'.format(b))
    df_e.info()
    dates = pd.to_datetime(df_e['Date'])
    dayOfWeek = pd.DatetimeIndex(dates).dayofweek
    days = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    fun = lambda x: days[x]
    fun = np.vectorize(fun)
    dayOfWeek_str = fun(dayOfWeek)
    if timestep == 'D':
        df_e.set_index(dates, inplace=True)
    elif timestep == 'H':
        time_str = df_e['Timestamp'].map(lambda x: x[:18])
        time_idx = pd.to_datetime(time_str)
        df_e.set_index(time_idx, inplace=True)
    print len(df_e)
    # fixme, timerange
    # df_e = df_e[df_e['Date'] < np.datetime64('2013-09-01')]
    print len(df_e)
    minDate = dates.min()
    maxDate = dates.max()
    print minDate, maxDate
    minDate_str = minDate.strftime('%Y-%m-%d %H:%M:%S')
    maxDate_str = maxDate.strftime('%Y-%m-%d %H:%M:%S')
    print minDate_str, maxDate_str
    df_w = ltm.get_weather_data(s, minDate_str, maxDate_str, timestep)
    df = pd.merge(df_e, df_w, left_index=True, right_index=True, how='left')
    df['day'] = dayOfWeek_str
    df['hour'] = df['Timestamp'].map(lambda x: int(x[11:13]))
    df['year'] = df['Timestamp'].map(lambda x: int(x[:4]))
    df['month'] = df['Timestamp'].map(lambda x: int(x[5:7]))
    df_plot = df
    df_plot = df_plot[df_plot[theme + ' (kBtu)'] >= 0]
    if timestep == 'H':
        sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), data=df_plot,
                fit_reg=False)
        plt.ylim((0, 4000))
        # plt.gca().set_ylim(bottom=0)
        # plt.show()
        image_output_dir = os.getcwd() + '/plot_FY_weather/html/single_building/lean_interval/'
        P.savefig('{0}scatter_{1}_{2}_{3}_{4}_ori.png'.format(image_output_dir, b, s, theme, timestep), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()
        sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), hue='day',
                col='hour', col_wrap=6, size=3, data=df_plot,
                fit_reg=False)
        plt.ylim((0, 4000))
        # plt.gca().set_ylim(bottom=0)
        P.savefig('{0}scatter_{1}_{2}_{3}_{4}.png'.format(image_output_dir, b, s, theme, timestep), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()

        # for year in [2014, 2015]:
        #     df_plot = df[(df['day'] == 'Tue') & (df['year'] == year)]
        #     sns.lmplot(x=s, y='{0} (kBtu)'.format(theme), hue='month',
        #             col='hour', col_wrap=6, size=3, data=df_plot, fit_reg=False)
        #     plt.gca().set_ylim(bottom=0)
        #     P.savefig('{0}scatter_{1}_{2}_{3}_{4}_{5}_Tue.png'.format(image_output_dir, b, s, theme, timestep, year), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        #     plt.close()

    if theme == 'Gas':
        df_reg = df.rename(columns={'{0} (kBtu)'.format(theme): 'eui_gas', 'Date': 'timestamp'})
        df_reg['day'] = dayOfWeek_str
        ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, timerange, df_reg)
    elif theme == 'Electric':
        df_reg = df.rename(columns={'{0} (kBtu)'.format(theme): 'eui_elec', 'Date': 'timestamp'})
        df_reg['day'] = dayOfWeek_str
        df_reg = df_reg[df_reg['day'] != 'Sat']
        df_reg = df_reg[df_reg['day'] != 'Sun']
        ltm.piecewise_reg_one(b, s, npar, 'eui_elec', False, timerange, df_reg)
    # # plt.show()
    return

# 128.2.108.158
def compute_cv():
    cv('temperature', 'eui_elec', 'CDD')
    cv('dd', 'eui_elec', 'CDD')
    cv('vv', 'eui_elec', 'CDD')

def merge_result():
    # theme = 'eui_elec'
    methods = ['temperature', 'dd', 'vv']
    for theme, kind in zip(['eui_elec', 'eui_gas'], ['CDD', 'HDD']):
    # for theme, kind in zip(['eui_gas'], ['HDD']):
        dfs = []
        print theme
        for method in methods:
            # cv(method, theme, kind)
            df = pd.read_csv(weatherdir + 'cv_{0}_{1}.csv'.format(method, theme))
            df['method'] = method
            df['CVRMSE'] = df['CVRMSE_' + method]
            dfs.append(df)
    # dfs = [pd.read_csv(weatherdir + 'cv_{0}_{1}.csv'.format(method, theme)) for method in methods]
        # df_all = reduce(lambda x, y: pd.merge(x, y, on='Building Number', how='inner'), dfs)
        df_all = pd.concat(dfs, ignore_index=True)
        print method
        print df_all.describe()
        # sns.boxplot(x='method', y='CVRMSE', data=df_all)
        # plt.title(theme)
        # plt.ylim((0, 10))
        # plt.show()
        df_all.to_csv(weatherdir + 'cv_{0}.csv'.format(theme), index=False)
    return
    
def filter(theme):
    df = pd.read_csv(weatherdir + 'cv_{0}.csv'.format(theme))
    df.info()
    df = df[df['CVRMSE_temperature'] != np.inf]
    df = df[df['CVRMSE_temperature'].notnull()]
    df = df[df['CVRMSE_temperature'] < 20]
    cols = list(df)
    cols.remove('Building Number')
    for c in cols:
        df[c] = df[c].map(lambda x: round(x, 2))
    df.to_csv(weatherdir + 'cv_{0}_noinf.csv'.format(theme), index=False)
    
def plot_interval():
    b = 'UT0017ZZ'
    s = 'KSLC'
    load_data(b, s, 3, 'Electric', 'D', 'after 2013-9-1')
    b = 'UT0032ZZ'
    s = 'KPVU'
    load_data(b, s, 2, 'Electric', 'D', 'after 2013-9-1')
    b = 'NE0531ZZ'
    s = 'KLNK'
    load_data(b, s, 2, 'Electric', 'D', 'after 2013-9-1')
    return

def compare_lean_timediff(timestep):
    buildings = ['KS0094ZZ', 'UT0017ZZ', 'NE0531ZZ', 'UT0032ZZ']
    ss = ['KMKC', 'KSLC', 'KLNK', 'KPVU']
    for b in buildings[:1]:
        # df1 = util.read_building_eui(b, timestep)
        df1 = pd.read_csv(interval_dir + 'single_monthly/{0}_Electric_M.csv'.format(b))
        df1.rename(columns={'Electric (kBtu)': 'Electric (kBtu) daily'}, inplace=True)
        df2 = util.read_building_eui(b, 'M')
        df_all = pd.merge(df1, df2, on=['year', 'month'], how='inner')
        print df_all.head()
        # df_w = ltm.
        # print len(df_e)
        # minDate = dates.min()
        # maxDate = dates.max()
        # print minDate, maxDate
        # minDate_str = minDate.strftime('%Y-%m-%d %H:%M:%S')
        # maxDate_str = maxDate.strftime('%Y-%m-%d %H:%M:%S')
        # print minDate_str, maxDate_str
        # df_w = ltm.get_weather_data(s, minDate_str, maxDate_str, timestep)

def main():
    # compare_lean_timediff('D')
    # compute_cv()
    # merge_result()
    # filter('eui_elec')
    # filter('eui_gas')
    # plot_interval()
    return

main()
