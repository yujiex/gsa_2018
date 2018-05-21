import pandas as pd
import numpy as np
import os
import glob
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
import sqlite3
from datetime import datetime
import time
import shutil

import util
import util_io as uo
import lean_temperature_monthly as ltm
import cluster as cl

homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
interval_dir = os.getcwd() + '/input/FY/interval/'
my_dpi = 150
steplabel = {'H': 'hourly', 'D': 'daily', 'M': 'monthly'}

def concat_consumption(suf):
    # files = glob.glob(os.getcwd() + '/input/FY/interval/*{0}.csv'.format(suf))
    # dfs = [pd.read_csv(f) for f in files]
    # df_all = pd.concat(dfs, ignore_index=False)
    # df_all['year'] = df_all['Timestamp'].map(lambda x: x[:4])
    # df_all.rename(columns=lambda x: x.replace("EL.Total Electrical Consumption (Int)", ""), inplace=True)
    # df_all.rename(columns=lambda x: x.replace("NG.Total Gas Consumption (Int)", ""), inplace=True)
    # df_all.rename(columns=lambda x: '"{0}"'.format(x) if "," in x else
    #               x, inplace=True)
    # df_all['Date'] = df_all['Timestamp'].map(lambda x: x[:10])
    # df_all['Date'] = pd.to_datetime(df_all['Date'])
    # cols = list(df_all)
    # cols.remove('Timestamp')
    # cols.remove('Date')
    # cols.remove('year')
    # for col in cols:
    #     print col
    #     df_all[col] = df_all.apply(lambda r: np.nan if type(r[col]) == float and np.isnan(r[col]) else r['Date'], axis=1)
    # df_all.to_csv(os.getcwd() + '/input/FY/interval/summary/concat_{0}.csv'.format(suf), index=False)
    # print 'write to concat_{0}.csv'.format(suf)

    df_all = pd.read_csv(os.getcwd() + '/input/FY/interval/summary/concat_{0}.csv'.format(suf)) 
    df_all['year'] = df_all['Timestamp'].map(lambda x: x[:4])
    cols = list(df_all)
    cols.remove('Timestamp')
    cols.remove('Date')
    cols.remove('year')
    outfile = os.getcwd() + '/input/FY/interval/summary/summary_{0}.csv'.format(suf)
    with open(outfile, 'w+') as wt:
        wt.write('Building,min date,max date\n')
        for col in cols:
            dates = set(df_all[col].tolist())
            dates = dates.difference(set([np.nan])) 
            wt.write('{0},{1},{2}\n'.format(col, min(dates), max(dates)))

    f = os.getcwd() + '/input/FY/interval/summary/summary_{0}.csv'.format(suf)
    df = pd.read_csv(f)
    df['start_year'] = df['min date'].map(lambda x: x[:4])
    df['value'] = 1
    df_p = df.pivot(index='Building', columns='start_year', values='value')
    outfile = os.getcwd() + '/input/FY/interval/summary/earliest_year_{0}.csv'.format(suf)
    df_p.to_csv(outfile, index=True)

    df['end_year'] = df['max date'].map(lambda x: x[:4])
    df['value'] = 1
    df_p = df.pivot(index='Building', columns='end_year', values='value')
    outfile = os.getcwd() + '/input/FY/interval/summary/latest_year_{0}.csv'.format(suf)
    df_p.to_csv(outfile, index=True)
    return
    
def compare_names():
    conn = sqlite3.connect(homedir + 'db/interval.db')
# b    Cu = conn.cursor()
    df1 = pd.read_csv(os.getcwd() + \
        '/input/FY/GSAlink 81 Buildings Updated 9_22_15.csv')
    df_e = pd.read_csv(os.getcwd() +
                      '/input/FY/interval/summary/summary_Total' +\
                      ' Electric Consumption.csv')
    df_g = pd.read_csv(os.getcwd() +
                      '/input/FY/interval/summary/summary_Total' +\
                      ' Gas Consumption.csv')
    df_e['Building Name'] = df_e['Building'].map(lambda x: x[:-1])
    df_e.drop('Building', axis=1, inplace=True)
    df_g['Building Name'] = df_g['Building'].map(lambda x: x[:-1])
    df_g.drop('Building', axis=1, inplace=True)
    df1 = df1[['Building ID', 'Building Name']]
    df = pd.merge(df1, df_e, on='Building Name', how='left')
    df_all = pd.merge(df, df_g, on='Building Name', how='left', suffixes=['_elec', '_gas'])
    df_all.rename(columns={'Building ID': 'Building Number'},
                  inplace=True)
    outfile = 'gsalink_interval_availability.csv'
    print 'write to ' + outfile
    df_all.to_csv(master_dir + outfile, index=False)
    df_all.to_sql('gsalink_interval_availability', conn,
                  if_exists='replace')
    df_euas = pd.read_csv(master_dir + 'EUAS_static_tidy.csv')
    df_euas = df_euas[['Building Number']]
    df_all2 = pd.merge(df_euas, df_all, on='Building Number',
                       how='left')
    outfile2 = outfile.replace('gsalink', 'EUAS')
    print 'write to ' + outfile2
    df_all2.to_csv(master_dir + outfile2, index=False)
    df_all2 = df_all2.dropna(subset=['min date_elec', 'max date_elec', 'min date_gas', 'max date_gas'], how='all')
    df_all2.to_csv(master_dir + outfile2.replace('.csv', 'nonan.csv'), index=False)
    # df_all.to_sql('EUAS_interval_availability', conn,
    #               if_exists='replace')
    cols = ['min date_gas']
    print len(df_all2)
    df_all3 = df_all2.dropna(subset=cols)
    print df_all3[['Building Number', 'min date_gas', 'max date_gas']]
    conn.close()
    return
    
def compare_month_daily_twomonth(theme):
    df_month = pd.read_csv(master_dir + 'energy_info_monthly.csv')
    df_month.rename(columns={'Electricity (kBtu)': 'Electric (kBtu)'}, inplace=True)
    df_month = df_month[['Building Number', 'year', 'month', theme + ' (kBtu)']]
    df_e = read_interval(theme)
    df_e.rename(columns={theme + ' (kBtu)': theme + ' (kBtu)_daily_aggregated'}, inplace=True)
    df_e_all = pd.merge(df_month, df_e, on=['Building Number', 'year', 'month'], how='inner')
    df_e_all.reset_index(inplace=True)
    df_e_all['Date'] = df_e_all.apply(lambda r: datetime.strptime('{0}-{1}-{2}'.format(int(r['year']), int(r['month']), 1), "%Y-%m-%d"), axis=1)
    gr = df_e_all.groupby('Building Number')
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 8)
    sns.set_context("talk", font_scale=1)
    for name, group in list(gr):
        print name
        group.reset_index(inplace=True)
        group.set_index(pd.DatetimeIndex(group['Date']), inplace=True)
        df_re = group.resample('2M', how='sum', convention='start')
        total = df_re.sum()
        ratio = round(total[theme + ' (kBtu)']/total[theme + ' (kBtu)_daily_aggregated'], 3)
        line1, = plt.plot(df_re.index, df_re[theme + ' (kBtu)']/1e3, marker="o")
        line2, = plt.plot(df_re.index, df_re[theme + ' (kBtu)_daily_aggregated']/1e3, marker="o")
        plt.title('Building: {0}, (total EUAS/total Skysparke) = {1}'.format(name, ratio))
        plt.legend([line1, line2], ['monthly', 'daily aggregated'], loc='center left', bbox_to_anchor=(1, 0.5), prop={'size':13})
        plt.ylabel('Monthly Total {0} (Million Btu)'.format(theme))
        plt.gca().set_ylim(bottom=0)
        path = os.getcwd() + '/input/FY/interval/plot_2month/{0}_{1}.png'.format(name, theme)
        # plt.show()
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()

def compare_month_daily(theme):
    df_month = pd.read_csv(master_dir + 'energy_info_monthly.csv')
    df_month.rename(columns={'Electricity (kBtu)': 'Electric (kBtu)'}, inplace=True)
    df_month = df_month[['Building Number', 'year', 'month', theme + ' (kBtu)']]
    df_e = read_interval(theme)
    df_e.rename(columns={theme + ' (kBtu)': theme + ' (kBtu)_daily_aggregated'}, inplace=True)
    df_e_all = pd.merge(df_month, df_e, on=['Building Number', 'year', 'month'], how='inner')
    df_e_all.reset_index(inplace=True)
    df_e_all['Date'] = df_e_all.apply(lambda r: datetime.strptime('{0}-{1}-{2}'.format(int(r['year']), int(r['month']), 1), "%Y-%m-%d"), axis=1)
    df_e_all.sort(columns=['Building Number', 'year', 'month'],
                  inplace=True)
    df_e_all.to_csv(master_dir +
                    'EUAS_interval_{0}.csv'.format(theme), index=False)
    gr = df_e_all.groupby('Building Number')
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 8)
    sns.set_context("talk", font_scale=1)
    for name, group in list(gr):
        print name
        group.reset_index(inplace=True)
        group.set_index(pd.to_datetime(group['Date']), inplace=True)
        total = group.sum()
        ratio = round(total[theme + ' (kBtu)']/total[theme + '(kBtu)_daily_aggregated'], 3)
        line1, = plt.plot(group.index, group[theme + ' (kBtu)']/1e3, marker="o")
        line2, = plt.plot(group.index, group[theme + ' (kBtu)_daily_aggregated']/1e3, marker="o")
        plt.title('Building: {0}, (total EUAS/total Skysparke) = {1}'.format(name, ratio))
        plt.legend([line1, line2], ['monthly', 'daily aggregated'], loc='center left', bbox_to_anchor=(1, 0.5), prop={'size':13})
        plt.ylabel('Monthly Total {0} (Million Btu)'.format(theme))
        plt.gca().set_ylim(bottom=0)
        path = os.getcwd() + '/input/FY/interval/plot/{0}_{1}.png'.format(name, theme)
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()
    print 'end'
    
def read_building_raw_energy(b):
    df = pd.read_csv(master_dir + 'energy_info_monthly.csv')
    df = df[df['Building Number'] == b]
    df.to_csv(master_dir + '{0}.csv'.format(b), index=False)
    return

def read_interval(theme):
    conn = sqlite3.connect(homedir + 'db/interval.db')
    c = conn.cursor()
    df = pd.read_csv(master_dir + 'EUAS_interval_availability.csv')
    if theme == 'Electric':
        df = df[['Building Number', 'Building Name', 'min date_elec']]
        df.dropna(subset=['min date_elec'], inplace=True)
        df['start_year'] = df['min date_elec'].map(lambda x: x[:4])
    elif theme == 'Gas':
        df = df[['Building Number', 'Building Name', 'min date_gas']]
        df.dropna(subset=['min date_gas'], inplace=True)
        df['start_year'] = df['min date_gas'].map(lambda x: x[:4])
    df.set_index('Building Name', inplace=True)
    buildings = df['Building Number'].tolist()
    years = range(2011, 2016)
    dfs = [pd.read_csv(os.getcwd() + '/input/FY/interval/{0} Total {1} Consumption.csv'.format(year, theme)) for year in years]
    df_all = pd.concat(dfs, ignore_index=True)
    cols = list(df_all)
    cols.remove('Timestamp')
    if theme == 'Electric':
        suf = 'EL.'
        e2float = lambda x: np.nan if type(x) == float else float(x[:-3]) * 3.412
    else:
        suf = 'NG.'
        e2float = lambda x: np.nan if type(x) == float else float(x[:-4]) * 1.026
    df_minuses = []
    dfs_resample = []
    for c in cols:
        df_temp = df_all.copy()
        df_temp = df_temp[['Timestamp', c]]
        name = c[:c.find(suf) - 1]
        try:
            bd_id = df.ix[name, 'Building Number']
        except KeyError:
            continue
        # print bd_id
        df_temp.rename(columns={c: theme}, inplace=True)
        df_temp[theme + ' (kBtu)'] = df_temp[theme].map(e2float)
        df_minus = df_temp[df_temp[theme + ' (kBtu)'] < 0]
        if len(df_minus) > 0:
            df_minus['Building Name'] = name
            df_minus['Building Number'] = bd_id
            df_minuses.append(df_minus)
        df_temp = df_temp[df_temp[theme + ' (kBtu)'] >= 0]
        df_temp.dropna(subset=[theme], inplace=True)
        df_temp['Date'] = df_temp['Timestamp'].map(lambda x:x[:10])
        df_temp.rename(columns={'Electric': 'Electric (kWh)', 'Gas':
                                'Gas (ft3)'}, inplace=True)
        df_temp.to_csv(os.getcwd() +
                       '/input/FY/interval/single/{0}_{1}_D.csv'.format(bd_id, theme), index=False)
        df_temp.drop('Timestamp', axis=1, inplace=True)
        df_temp['Date'] = pd.to_datetime(df_temp['Date'])
        df_dt = df_temp.set_index(pd.DatetimeIndex(df_temp['Date']))
        df_re = df_dt.resample('M', how='sum')
        df_re['year'] = df_re.index.map(lambda x: x.year)
        df_re['month'] = df_re.index.map(lambda x: x.month)
        df_re['Building Number'] = bd_id
        df_re.to_csv(os.getcwd() +
                     '/input/FY/interval/single_monthly/{0}_{1}_M.csv'.format(bd_id, theme), index=False)
        dfs_resample.append(df_re)
    df_m = pd.concat(df_minuses, ignore_index=True)
    df_m.to_csv(os.getcwd() +
                '/input/FY/interval/negative_{0}.csv'.format(theme),
                index=False)
    df_all = pd.concat(dfs_resample, ignore_index=True)
    conn.close()
    print 'end'
    return df_all
        
def read_interval_hour(timestep):
    conn = sqlite3.connect(homedir + 'db/interval.db')
    c = conn.cursor()
    print 'start reading {0} interval data ...'.format(steplabel[timestep])
    df_lookup = pd.read_csv(master_dir + 'EUAS_interval_availability.csv')
    df_lookup.set_index('Building Name', inplace=True)
    files = glob.glob(interval_dir + 'H/*.csv')
    dfs = []
    for f in files:
        df = pd.read_csv(f)
        df.drop(0, axis=0, inplace=True)
        cols = list(df)
        cols.remove('Timestamp')
        if 'Electric' in f:
            df.rename(columns={k: k[:k.find(' EL.')] for k in cols}, inplace=True)
            df['Energy Type'] = 'Electric'
        else:
            df.rename(columns={k: k[:k.find(' NG.')] for k in cols}, inplace=True)
            df['Energy Type'] = 'Gas'
        dfs.append(df)
    e2float_elec = lambda x: np.nan if type(x) == float else float(x[:-3]) * 3.412
    e2float_gas = lambda x: np.nan if type(x) == float else float(x[:-4]) * 1.026
    df_all = pd.concat(dfs)
    cols = list(df_all)
    cols.remove('Timestamp')
    cols.remove('Energy Type')
    cols.insert(0, 'Energy Type')
    cols.insert(0, 'Timestamp')
    df_all = df_all[cols]
    df_all.sort(['Energy Type', 'Timestamp'], inplace=True)
    reading_cols = list(df_all)[2:]
    df_all.dropna(subset=reading_cols, how='all', inplace=True)
    df_all.to_csv(interval_dir + '{0}_all.csv'.format(timestep), index=False)
    cols = list(df_all)[2:]
    dfs = []
    for col in cols:
        df_temp = df_all[['Timestamp', 'Energy Type', col]]
        df_p = df_temp.pivot(index='Timestamp', columns='Energy Type', values=col)
        df_p.rename(columns={'Electric': 'Electric (kWh)', 'Gas':
                             'Gas (ft3)'}, inplace=True)
        df_p['Electric (kBtu)'] = df_p['Electric (kWh)'].map(e2float_elec)
        df_p['Gas (kBtu)'] = df_p['Gas (ft3)'].map(e2float_gas)
        df_p['Date'] = df_p.index.map(lambda x:x[:10])
        try:
            bd_id = df_lookup.ix[col, 'Building Number']
        except KeyError:
            continue
        print bd_id
        df_p.to_csv(interval_dir + 'single_hourly/{0}.csv'.format(bd_id))
        df_p['Building Number'] = bd_id
        dfs.append(df_p)
    df_all = pd.concat(dfs, ignore_index=True)
    df_all.to_csv(interval_dir + '{0}_all_long.csv'.format(timestep), index=False)
    # df_sq = df_all.drop(['Gas (ft3)'], axis=1)
    # df_sq.to_sql('{0}_all_long'.format(timestep), conn, if_exists='replace')
    return

def compare_hourly(timestep):
    bds = ['NE0531ZZ', 'UT0017ZZ', 'UT0032ZZ']
    for b in bds:
        print b
        df = pd.read_csv(interval_dir +
                         'single_hourly/{0}.csv'.format(b))
        df_temp = df.copy()
        df_temp['Date'] = pd.to_datetime(df_temp['Date'])
        df_dt = df_temp.set_index(pd.DatetimeIndex(df_temp['Date']))
        df_re = df_dt.resample(timestep, how='sum')
        # df_re.reset_index(inplace=True)
        df_re.rename(columns={'Gas (kBtu)': 'Gas (kBtu) hourly',
                              'Electric (kBtu)': 'Electric (kBtu) ' +
                              'hourly'}, inplace=True)
        if timestep == 'D':
            df2 = pd.read_csv(interval_dir +
                            'single/{0}_Gas.csv'.format(b))
            df2.rename(columns={'Gas(kBtu)':'Gas (kBtu) daily'},
                    inplace=True)
            df2['Date'] = pd.to_datetime(df2['Date'])
        elif timestep == 'M':
            df1 = util.read_building_eui(b, 'M')
            df1.rename(columns={'Electricity (kBtu)': 'Electric (kBtu) monthly', 'Gas (kBtu)': 'Gas (kBtu) monthly'}, inplace=True)
            df1 = df1[['year', 'month', 'Electric (kBtu) monthly', 'Gas (kBtu) monthly']]
            df1['Date'] = pd.to_datetime(df1.apply(lambda r: '{0}-{1}-1'.format(int(r['year']), int(r['month'])), axis=1))
            df1.set_index(pd.DatetimeIndex(df1['Date']), inplace=True)
            df2 = df1.resample('M', how='mean')
            df2.drop(labels=['year', 'month'], axis=1, inplace=True)
        sns.set_style("whitegrid")
        sns.set_palette("Set2")
        sns.set_context("talk", font_scale=1)
        df_all = pd.merge(df_re, df2, how='inner', left_index=True, right_index=True)
        line1, = plt.plot(df_all.index, df_all['Gas (kBtu) monthly'], marker="o")
        line2, = plt.plot(df_all.index, df_all['Gas (kBtu) hourly'], marker="o")
        plt.legend([line1, line2], ['monthly', 'hourly aggregated'])
        plt.ylabel('kBtu')
        path = os.getcwd() + '/input/FY/interval/plot_cmp/{0}_Gas.png'.format(b)
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()
    
def read_by_region(indir, prefix, value_label, melt):
    files = glob.glob(indir + prefix)
    dfs = [pd.read_csv(f) for f in files]
    df_all = pd.concat(dfs, ignore_index=True)
    df_all.dropna(axis=1, how='all', inplace=True)
    # print len(df_all)
    cols = list(df_all)
    cols.remove('Timestamp')
    df_all.dropna(subset=cols, axis=0, how='all', inplace=True)
    def no_na_start(lst):
        length = len(lst)
        result = []
        for i in range(length - 1):
            # print i, lst[i], np.isnan(lst[i]), lst[i + 1], np.isnan(lst[i + 1])
            if (np.isnan(lst[i])) and (not np.isnan(lst[i + 1])):
                result.append(i + 1)
        return result
    df_all['Timestamp'] = df_all['Timestamp'].map(lambda x: datetime.strptime(x, '%m/%d/%Y %I:%M:%S %p').strftime('%Y-%m-%d %H:%M:%S'))
    df_all.sort('Timestamp', inplace=True)
    for col in cols[:1]:
        need_to_change = no_na_start(df_all[col].tolist())
        # print need_to_change
        df_all.reset_index(inplace=True)
        # print df_all[['Timestamp', col, 'index']].head(n = 7)
        df_all[col] = df_all.apply(lambda r: np.nan if r['index'] in
                                   need_to_change else r[col], axis=1)
        # print df_all[['Timestamp', col, 'index']].head(n = 7)
    if melt:
        df_melt = pd.melt(df_all, id_vars='Timestamp', value_vars=cols)
        df_melt.rename(columns={'variable': 'Building_Number', 'value':
                                value_label}, inplace=True)
        return df_melt
    else:
        return df_all

def read_ion(measure_type, prefix, value_label):
    indir = os.getcwd() + '/input/FY/interval/ion_0627/{0}/'.format(measure_type)
    df1 = read_by_region(indir, prefix + ' 1_2_3_4*', value_label,
                         True)
    # temp = pd.to_datetime(df_melt['Timestamp'][:5])
    df2 = read_by_region(indir, prefix + ' 5_6*', value_label, True)
    # print len(df2), '222'
    df3 = read_by_region(indir, prefix + ' 7_to_11*', value_label,
                         True)
    # print len(df3), '333'
    df_melt = pd.concat([df1, df2, df3], ignore_index=True)
    print len(df_melt)
    df_melt.dropna(subset=[value_label], inplace=True)
    ids = df_melt['Building_Number'].unique()
    df_id = pd.DataFrame({'id': ids})
    print len(df_melt)
    conn = uo.connect('interval_ion')
    with conn:
        c = conn.cursor()
        c.execute("DROP TABLE IF EXISTS {0}".format(measure_type))
        df_melt.to_sql(measure_type, conn, if_exist='replace')
        df_id.to_sql('{0}_id'.format(measure_type), conn,
                     if_exist='replace')
    conn.close()
    print 'end'
    
def read_ion_single(measure_type, prefix):
    indir = os.getcwd() + '/input/FY/interval/ion_0627/{0}/'.format(measure_type)
    df1 = read_by_region(indir, prefix + ' 1_2_3_4*', 'Electric_(KWH)', False)
    df1['dup'] = df1.duplicated(cols = 'Timestamp')
    df1[df1['dup']].to_csv(homedir + 'temp/dup1.csv', index=False)
    df1.drop('dup', axis=1, inplace=True)
    df1.info()

    df2 = read_by_region(indir, prefix + ' 5_6*', 'Electric_(KWH)',
                         False)
    df2['dup'] = df2.duplicated(cols = 'Timestamp')
    df2[df2['dup']].to_csv(homedir + 'temp/dup2.csv', index=False)
    df2.drop('dup', axis=1, inplace=True)
    df1.info()
    df_all = pd.merge(df1, df2, on='Timestamp', how='outer')
    df_all.dropna(axis=1, how='all', inplace=True)
    ori = time.time()
    df_all['Timestamp'] = df_all['Timestamp'].map(lambda x: datetime.strptime(x, '%m/%d/%Y %I:%M:%S %p').strftime('%Y-%m-%d %H:%M:%S'))
    # df_all.info()
    # df_all.drop_duplicates(cols='Timestamp', inplace=True)

    # util.timing(ori, time.time(), 'strptime')
    # conn = uo.connect('interval_ion_single')
    # with conn:
    #     c = conn.cursor()
    #     c.execute("DROP TABLE IF EXISTS {0}".format(measure_type))
    #     df_all.to_sql(measure_type, conn, if_exist='replace')
    # conn.close()
    print 'end'

def summary_long(measure_type):
    conn = uo.connect('interval_ion')
    with conn:
        df_id = pd.read_sql('SELECT * FROM {0}_id'.format(measure_type), conn)
    lines = []
    lines.append('Building_Number,neg_count,min,max,median,75percent,min_time,max_time,expect_count,actural_count,missing_count')
    ids = df_id['id'].tolist()
    for name in ids:
        with conn:
            df = pd.read_sql('SELECT * FROM {0} WHERE Building_Number = \'{1}\''.format(measure_type, name), conn)
        df['Timestamp'] = pd.to_datetime(df['Timestamp'])
        cols = list(df)
        cols.remove('Timestamp')
        cols.remove('Building_Number')
        c = cols[0]
        temp = df.dropna(subset=[c])
        neg = temp[c].tolist()
        neg = [x for x in neg if x < 0]
        min_time = temp['Timestamp'].min()
        min_time_str = min_time.strftime('%Y-%m-%d %H:%M:%S')
        max_time = temp['Timestamp'].max()
        max_time_str = max_time.strftime('%Y-%m-%d %H:%M:%S')
        expect_count = int(pd.to_timedelta((max_time - min_time)) / np.timedelta64(15,'m')) + 1
        actural_count = len(temp)
        missing_count = expect_count - actural_count
        mini = temp[c].min()
        maxi = temp[c].max()
        median = temp[c].median()
        q3 = temp[c].quantile(0.75)
        print (','.join([name, str(int(len(neg))), str(mini), str(maxi), str(median), str(q3), min_time_str, max_time_str, str(expect_count), str(actural_count), str(missing_count)]))
        lines.append(','.join([name, str(int(len(neg))), str(mini), str(maxi), str(median), str(q3), min_time_str, max_time_str, str(expect_count), str(actural_count), str(missing_count)]))
    indir = os.getcwd() + '/input/FY/interval/ion_0627/summary_long/'
    with open (indir + 'summary_{0}.csv'.format(measure_type), 'w+') as wt:
        wt.write('\n'.join(lines))
        
def create_index_interval():
    files = glob.glob(os.getcwd() +
                      '/input/FY/interval/ion_0627/summary_long/*.csv')
    def type_err(n_outlier, n_neg, percent_data):
        err_list = []
        if (n_outlier > n_neg) or (n_outlier == n_neg and n_neg > 0):
            err_list.append('Random - High Positive Values')
        if n_neg > 0:
            err_list.append('Negative Values')
        if float(percent_data[:-1]) < 99:
            err_list.append('Data Loss')
        if len(err_list) == 0:
            return 'NA'
        else:
            return ';'.join(err_list)
    def get_method(string):
        methods = []
        if 'Random - High Positive Values' in string:
            methods.append('95% cutoff')
        if 'Negative Values' in string:
            methods.append('0 cutoff')
        if len(methods) == 0:
            return 'NA'
        else:
            return ';'.join(methods)
    # method_dict = {'negative|extreme': '90% cutoff 0 cutoff',
    #                'extreme': '90% cutoff', 'negative': '0 cutoff'}
    df_err = pd.read_csv(homedir + 'temp/Updated error Table.csv')
    err_dict = dict(zip(df_err['Building_Number'], df_err['type_of_err']))
    clean_dict = dict(zip(df_err['Building_Number'], df_err['Cleaning Methods']))
    for f in files:
        df1 = pd.read_csv(f)
        measure_type = f[f.rfind('_') + 1: f.rfind('.')]
        print measure_type
        df1 = df1[['Building_Number', 'min_time', 'max_time', 'expect_count', 'missing_count', 'neg_count']]
        df2 = pd.read_csv(os.getcwd() +
                          '/input/FY/interval/ion_0627/summary_long/outlier/{0}_outlier.csv'.format(measure_type))
        df = pd.merge(df1, df2, on='Building_Number', how='left')
        df['max_time'] = pd.to_datetime(df['max_time'])
        df['min_time'] = pd.to_datetime(df['min_time'])
        df['data_time_span'] = df['max_time'] - df['min_time']
        df['percent_data_available'] = df.apply(lambda r: '{0:.2%}'.format(1 - float(r['missing_count'])/r['expect_count']), axis=1)
        df['before_cleaning'] = df['Building_Number'].map(lambda x: '<a href="trend/{0}_{1}.html">link</a>'.format(x, measure_type))
        df['type_of_err'] = df.apply(lambda r: type_err(r['n_outlier'], r['neg_count'], r['percent_data_available']), axis=1)
        df['after_cleaning'] = df['Building_Number'].map(lambda x: '<a href="trend_no/{0}_{1}.html">link</a>'.format(x, measure_type))
        df['Cleaning Method'] = df['type_of_err'].map(get_method)
        df = df[['Building_Number', 'data_time_span', 
                 'percent_data_available', 'before_cleaning', 'after_cleaning', 'type_of_err', 'Cleaning Method']]
        print df.head()
        outfile = f.replace('.csv', '.html')
        outfile = outfile.replace('/input/FY/interval/ion_0627/summary_long/', '/plot_FY_weather/html/interval/')
        with pd.option_context('max_colwidth', 80):
            df.to_html(outfile, index=False)
        with open(outfile, 'r') as rd:
            lines = rd.readlines()
        for i in range(len(lines)):
            lines[i] = lines[i].replace('&lt;', '<')
            lines[i] = lines[i].replace('&gt;', '>')
            lines[i] = lines[i].replace('...', '')
        with open(outfile, 'w') as wt:
            wt.write(''.join(lines))
    
def summary_wide(measure_type):
    conn = uo.connect('interval_ion_single')
    with conn:
        df = pd.read_sql('SELECT * FROM {0}'.format(measure_type),
                         conn)
    conn.close()
    df['Timestamp'] = pd.to_datetime(df['Timestamp'])
    cols = list(df)
    cols.remove('Timestamp')
    lines = []
    lines.append('Building_Number,neg_count,min,max,median,min_time,max_time,expect_count,actural_count,missing_count')
    for c in cols:
        temp = df[['Timestamp', c]].copy()
        temp.dropna(subset=[c], inplace=True)
        neg = temp[c].tolist()
        neg = [x for x in neg if x < 0]
        min_time = temp['Timestamp'].min()
        min_time_str = min_time.strftime('%Y-%m-%d %H:%M:%S')
        max_time = temp['Timestamp'].max()
        max_time_str = max_time.strftime('%Y-%m-%d %H:%M:%S')
        expect_count = int(pd.to_timedelta((max_time - min_time)) / np.timedelta64(15,'m')) + 1
        actural_count = len(temp)
        missing_count = expect_count - actural_count
        mini = temp[c].min()
        maxi = temp[c].max()
        median = temp[c].median()
        print (','.join([c, str(int(len(neg))), str(mini), str(maxi), str(median), min_time_str, max_time_str, str(expect_count), str(actural_count), str(missing_count)]))
        lines.append(','.join([c, str(int(len(neg))), str(mini), str(maxi), str(median), min_time_str, max_time_str, str(expect_count), str(actural_count), str(missing_count)]))
    indir = os.getcwd() + '/input/FY/interval/ion_0627/summary/'
    with open (indir + 'summary_{0}.csv'.format(measure_type), 'w+') as wt:
        wt.write('\n'.join(lines))

def add_temperature_long_gsalink():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, ICAO FROM gsalink_weather_station', conn)
    conn.close()
    # print df.head()
    conn = uo.connect('interval_ion')
    with conn:
        ids_elec = pd.read_sql('SELECT * FROM electric_id', conn)['id']
        ids_gas = pd.read_sql('SELECT * FROM gas_id', conn)['id']
    df_id_elec = df[df['Building_Number'].isin(ids_elec)]
    df_id_gas = df[df['Building_Number'].isin(ids_gas)]
    stations = np.union1d(df_id_elec['ICAO'].unique(), df_id_gas['ICAO'].unique())
    with conn:
        df_id_elec.to_sql('electric_id_station', conn, if_exists='replace')
        df_id_gas.to_sql('gas_id_station', conn, if_exists='replace')
    conn_w = uo.connect('gsalink_utc')
    print len(stations)
    for s in stations:
        with conn_w:
            ds = pd.read_sql('SELECT * FROM {0} WHERE Timestamp between \'2010-09-01\' and \'2016-06-28\''.format(s), conn_w)
            print s
        with conn:
            ds.to_sql(s, conn, if_exists='replace')
    conn_w.close()
    conn.close()

def add_temperature_long():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, ICAO FROM EUAS_monthly_weather', conn)
    conn.close()
    # print df.head()
    conn = uo.connect('interval_ion')
    with conn:
        ids_elec = pd.read_sql('SELECT * FROM electric_id', conn)['id']
        ids_gas = pd.read_sql('SELECT * FROM gas_id', conn)['id']
    df_id_elec = df[df['Building_Number'].isin(ids_elec)]
    df_id_gas = df[df['Building_Number'].isin(ids_gas)]
    stations = np.union1d(df_id_elec['ICAO'].unique(), df_id_gas['ICAO'].unique())
    with conn:
        df_id_elec.to_sql('electric_id_station', conn, if_exists='replace')
        df_id_gas.to_sql('gas_id_station', conn, if_exists='replace')
    conn_w = uo.connect('weather_hourly_utc')
    print len(stations)
    for s in stations:
        with conn_w:
            ds = pd.read_sql('SELECT * FROM {0} WHERE Timestamp between \'2011-09-01\' and \'2016-06-28\''.format(s), conn_w)
            print s
        with conn:
            ds.to_sql(s, conn, if_exists='replace')
    conn_w.close()
    conn.close()

def add_temperature_wide():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, ICAO FROM EUAS_monthly_weather', conn)
    conn.close()
    # print df.head()
    conn = uo.connect('interval_ion_single')
    with conn:
        df_elec = pd.read_sql('SELECT * FROM electric', conn)
        df_gas = pd.read_sql('SELECT * FROM gas', conn)
    ids_elec = list(df_elec)
    ids_gas = list(df_gas)
    ids_elec.remove('Timestamp')
    ids_gas.remove('Timestamp')
    df_id_elec = df[df['Building_Number'].isin(ids_elec)]
    df_id_gas = df[df['Building_Number'].isin(ids_gas)]
    stations = np.intersect1d(df_id_elec['ICAO'].unique(), df_id_elec['ICAO'].unique())
    with conn:
        df_id_elec.to_sql('electric_id_station', conn, if_exists='replace')
        df_id_gas.to_sql('gas_id_station', conn, if_exists='replace')
    conn_w = uo.connect('weather_hourly_utc')
    print len(stations)
    # for s in stations:
    #     with conn_w:
    #         ds = pd.read_sql('SELECT * FROM {0} WHERE Timestamp between \'2013-09-01\' and \'2016-06-28\''.format(s), conn_w)
    #         print s
    #     with conn:
    #         ds.to_sql(s, conn, if_exists='replace')
    # conn_w.close()
    # conn.close()
    
def add_area():
    conn = uo.connect('all')
    with conn:
        df_area = pd.read_sql('SELECT DISTINCT Building_Number, [Gross_Sq.Ft] FROM EUAS_area WHERE Fiscal_Year > 2010', conn)
    conn.close()
    conn = uo.connect('interval_ion')
    with conn:
        ids_elec = pd.read_sql('SELECT * FROM electric_id', conn)['id']
        ids_gas = pd.read_sql('SELECT * FROM gas_id', conn)['id']
    ids = np.union1d(ids_elec, ids_gas)
    df_area = df_area[df_area['Building_Number'].isin(ids)]
    print len(df_area)
    print df_area.head(n = 20)
    change_area = df_area.groupby('Building_Number').filter(lambda x: len(x) > 2)
    assert(len(change_area) == 0)
    with conn:
        df_area.to_sql('area', conn, if_exists='replace')
    conn.close()
    
def get_status(hour, day):
    def night(hour):
        return (float(hour) > 18)
    def morning(hour):
        return float(hour) < 6
    if (not night(hour)) and (not morning(hour)):
        if day in ['Sat', 'Sun']:
            return 'weekend day'
        else:
            return 'week day'
    elif night(hour):
        if day in ['Mon', 'Tue', 'Wed', 'Thu']:
            return 'week night'
        else:
            return 'weekend night'
    else:
        if day in ['Tue', 'Wed', 'Thu', 'Fri']:
            return 'week night'
        else:
            return 'weekend night'

def get_status_v0(hour, day):
    def night(hour):
        return (float(hour) > 18)
    def morning(hour):
        return float(hour) < 6
    if (night(hour)) or (morning(hour)):
        if day in ['Sat', 'Sun']:
            return 'weekend night'
        else:
            return 'week night'
    else:
        if day in ['Sat', 'Sun']:
            return 'weekend day'
        else:
            return 'week day'

def plot_scatter_long(hue_str, measure_type):
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
    sns.set_context("talk", font_scale=1.5)
    col_wrap_dict = {'hour': 6, 'month': 4, 'day': 5, 'status':2}
    upper = {'electric': 600, 'gas': 2500}
    value_lb_dict = {'electric': 'Electric_(KWH)', 'gas':
                     'Gas_(CubicFeet)'}
    multiplier_dict = {'electric':  3.412, 'gas': 1.026}
    ylabel = {'electric': 'electric (kBtu/sq.ft)', 'gas': 'gas kBtu/sq.ft'}
    # test = ['TX0057ZZ', 'NY0281ZZ', 'NY0304ZZ', 'MO0106ZZ']
    # test = ['TN0088ZZ']
    # bs_pair = [x for x in bs_pair if x[0] in test]
    # bs_pair = []
    for b, s in bs_pair:
        print b, s
        # df_w = pd.read_sql('SELECT * FROM {0} WHERE Timestamp between \'\2015-01-01\' and \'2016-01-01\''.format(s), conn)
        col = value_lb_dict[measure_type]
        m = multiplier_dict[measure_type]
        with conn:
            df_w = pd.read_sql('SELECT * FROM {0}'.format(s), conn)
            df_minute = pd.read_sql('SELECT Timestamp, [{0}] FROM {1} WHERE Building_Number = \'{2}\''.format(col, measure_type, b), conn)
        df_minute['h'] = df_minute['Timestamp'].map(lambda x: x[:-5] + '00:00')
        df_e = df_minute.groupby('h').sum()
        df_e.reset_index(inplace=True)
        df_e.rename(columns={'h': 'Timestamp'}, inplace=True)
        try:
            area = df_area.ix[b, 'Gross_Sq.Ft']
        except KeyError:
            print 'no area'
            continue
        df_e['eui'] = df_e[col] * m / area
        offset = df_tz.loc[b, 'rawOffset']
        local = pd.to_datetime(df_w['Timestamp']).map(lambda x: x + np.timedelta64(offset, 's'))
        local_str = local.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
        df_w['Timestamp'] = local_str
        df_all = pd.merge(df_w, df_e, on='Timestamp', how='inner')
        df_all['hour'] = df_all['Timestamp'].map(lambda x: x[11:13]) 
        df_all['month'] = df_all['Timestamp'].map(lambda x: x[5:7]) 
        df_all['year'] = df_all['Timestamp'].map(lambda x: x[:4]) 
        df_all['day'] = df_all['Timestamp'].map(lambda x: datetime.strptime(x, '%Y-%m-%d %H:%M:%S').strftime('%a')) 
        df_all['status'] = df_all.apply(lambda r: 'other' if float(r['hour']) < 6 or float(r['hour']) > 18 or r['day'] in ['Sat', 'Sun'] else 'work hour', axis=1)
        df_all['status_week_day_night'] = \
            df_all.apply(lambda r: get_status_v0(r['hour'], r['day']),
                         axis=1)
        df_all['status_week_day_night_nonflex'] = \
            df_all.apply(lambda r: get_status_v0(r['hour'], r['day']),
                         axis=1)
        df_all.to_csv('/home/yujiex/Public/{0}.csv'.format(b), index=False)
        df_all['method'] = 'non flex week'
        df_all2 = df_all.copy()
        df_all2['status_week_day_night'] = \
            df_all2.apply(lambda r: get_status(r['hour'], r['day']),
                          axis=1)
        df_all2['method'] = 'flex week'
        if hue_str is None:
            sns.regplot(x='Temperature_F', y='eui', data=df_all, fit_reg=False)
            plt.ylabel(ylabel[measure_type])
            plt.title('{0} vs Temperature (F): {1}'.format(ylabel[measure_type], b))
            # plt.ylim((0, upper[measure_type]))
        elif hue_str is 'status':
            g = sns.lmplot(x='Temperature_F', y='eui', hue=hue_str,
                           hue_order=['work hour', 'other'],
                           palette='husl', size=5, data=df_all,
                           fit_reg=False)
            plt.ylabel(ylabel[measure_type])
            g.set(ylabel=ylabel[measure_type])
            if measure_type == 'electric':
                plt.ylim((0, 0.003 * 4))
            elif measure_type == 'gas':
                plt.ylim((0, 0.008 * 4))
            min_time = df_all['Timestamp'].min()
            max_time = df_all['Timestamp'].max()
            print min_time, max_time
            plt.title('{0} {1} Setback'.format(b,
                                               measure_type.title()))
            plt.suptitle('{0} -- {1}'.format(min_time, max_time))
            plt.subplots_adjust(top=0.85)
        elif hue_str == 'status_week_day_night_nonflex':
            df_plot = df_all.groupby('year').filter(lambda x: len(x) > 7500)
            if len(df_plot) == 0:
                print 'not enough data points'
                continue
            g = sns.lmplot(x='Temperature_F', y='eui', hue=hue_str,
                           col='year',
                           hue_order=['week day', 'weekend day',
                                      'week night', 'weekend night'], 
                           palette='husl',
                           # palette='Paired',
                           size=5, aspect = 1.0, data=df_plot, 
                           fit_reg=False, scatter_kws={'s':8}, 
                           legend=False)
                           # lowess=True, scatter_kws={'s':4})
            plt.ylabel(ylabel[measure_type])
            g.set(ylabel=ylabel[measure_type])
            if measure_type == 'electric':
                plt.ylim((0, 0.003 * 4))
            elif measure_type == 'gas':
                plt.ylim((0, 0.008 * 4))
                # plt.ylim((0, 0.04))
            min_time = df_all['Timestamp'].min()
            max_time = df_all['Timestamp'].max()
            plt.legend(loc='center left', bbox_to_anchor=(1, 0.5),
                       markerscale=3)
            print min_time, max_time
            # plt.title('{0} {1} Setback'.format(b,
            #                                    measure_type.title()))
            # plt.suptitle('{0} -- {1}'.format(min_time, max_time))
            title = '{0} {1} Setback\n{2} -- {3}'.format(b, measure_type.title(), min_time, max_time)
            plt.suptitle(title)
            plt.subplots_adjust(top=0.85)
        elif hue_str == 'status_week_day_night':
            # check distribution of the shooting up ones
            # df_all = df_all[(df_all['Temperature_F'] < 50) & (df_all['Temperature_F'] > 45)]
            # df_all = df_all[df_all['status_week_day_night'] == 'week day']
            # print df_all.describe()
            # sns.distplot(df_all[df_all['eui'] < 40]['eui'])
            # print len(df_all[df_all['eui'] > 40])

            df_con = pd.concat([df_all, df_all2], ignore_index=True)
            g = sns.lmplot(x='Temperature_F', y='eui', hue=hue_str,
                           col='year', row='method',
                           hue_order=['week day', 'weekend day',
                                      'week night', 'weekend night'], 
                           palette='husl',
                           # palette='Paired',
                           size=5, aspect = 1.0, data=df_con, 
                           fit_reg=False)
                           # lowess=True, scatter_kws={'s':4})
            plt.ylabel(ylabel[measure_type])
            g.set(ylabel=ylabel[measure_type])
            if measure_type == 'electric':
                plt.ylim((0, 0.003 * 4))
            elif measure_type == 'gas':
                plt.ylim((0, 0.008 * 4))
                # plt.ylim((0, 0.04))
            min_time = df_all['Timestamp'].min()
            max_time = df_all['Timestamp'].max()

            print min_time, max_time
            # plt.title('{0} {1} Setback'.format(b,
            #                                    measure_type.title()))
            # plt.suptitle('{0} -- {1}'.format(min_time, max_time))
            title = '{0} {1} Setback\n{2} -- {3}'.format(b, measure_type.title(), min_time, max_time)
            plt.suptitle(title)
            plt.subplots_adjust(top=0.85)
        elif hue_str == 'cluster':
            cl.plot_cluster(df_all, b, s, measure_type, 0.18, 100)
            continue
        elif hue_str == 'piece':
            df_all = df_all[df_all[b] > 0]
            plot_piecewise(measure_type, df_all, b, s)
        else:
            g = sns.lmplot(x='Temperature_F', y=b, hue=hue_str, col=hue_str, col_wrap=col_wrap_dict[hue_str], size=3, data=df_all, fit_reg=False)
            g.set(ylim=(0, upper[measure_type]))
            g.set(ylabel=ylabel[measure_type])
        plt.gca().set_ylim(bottom=0)
        # plt.show()
        path = os.getcwd() + '/input/FY/interval/ion_0627/{0}_scatter/{1}_{2}_{3}'.format(measure_type, b, s, hue_str)
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()
    return

def plot_scatter(hue_str, measure_type):
    conn = uo.connect('interval_ion_single')
    with conn:
        df_bs = pd.read_sql('SELECT * FROM {0}_id_station'.format(measure_type), conn)
    bs_pair = zip(df_bs['Building_Number'], df_bs['ICAO'])
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 8)
    sns.set_context("talk", font_scale=1)
    col_wrap_dict = {'hour': 6, 'month': 4, 'day': 5}
    upper = {'electric': 600, 'gas': 2500}
    ylabel = {'electric': 'electric (kwh)'.title(), 'gas': 'gas (Cubic Feet ?)'.title()}
    for b, s in bs_pair:
        print b, s
        df_w = pd.read_sql('SELECT * FROM {0} WHERE Timestamp between \'\2015-01-01\' and \'2016-01-01\''.format(s), conn)
        with conn:
            df_w = pd.read_sql('SELECT * FROM {0}'.format(s), conn)
            df_e = pd.read_sql('SELECT Timestamp, {0} FROM {1}'.format(b, measure_type), conn)
            local = pd.to_datetime(df_w['Timestamp']).map(lambda x: x - np.timedelta64(5, 'h'))
            local_str = local.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
            df_w['Timestamp'] = local_str
        df_all = pd.merge(df_w, df_e, on='Timestamp', how='inner')
        df_all['hour'] = df_all['Timestamp'].map(lambda x: x[11:13]) 
        df_all['month'] = df_all['Timestamp'].map(lambda x: x[5:7]) 
        df_all['day'] = df_all['Timestamp'].map(lambda x: datetime.strptime(x, '%Y-%m-%d %H:%M:%S').strftime('%a')) 

        if hue_str is None:
            sns.regplot(x='Temperature_F', y=b, data=df_all, fit_reg=False)
            plt.ylabel(ylabel[measure_type])
            plt.title('{0} vs Temperature (F): {1}'.format(ylabel[measure_type], b))
            plt.ylim((0, upper[measure_type]))
        elif hue_str == 'piece':
            df_all = df_all[df_all[b] > 0]
            plot_piecewise(measure_type, df_all, b, s)
        else:
            g = sns.lmplot(x='Temperature_F', y=b, hue=hue_str, col=hue_str, col_wrap=col_wrap_dict[hue_str], size=3, data=df_all, fit_reg=False)
            g.set(ylim=(0, upper[measure_type]))
            g.set(ylabel=ylabel[measure_type])
        plt.gca().set_ylim(bottom=0)
        # plt.show()
        path = os.getcwd() + '/input/FY/interval/ion_0627/{0}_scatter/{1}_{2}_{3}'.format(measure_type, b, s, hue_str)
        P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()
    return
    
def plot_piecewise(measure_type, df_all, b, s):
    npar = 2
    if measure_type == 'gas':
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format(s), '{0}'.format(b): 'eui_gas', 'Timestamp': 'timestamp'})
        ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, None, df_reg)
    elif measure_type == 'electric':
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format(s), '{0}'.format(b): 'eui_elec', 'Timestamp': 'timestamp'})
        ltm.piecewise_reg_one(b, s, npar, 'eui_elec', False, None, df_reg)

def check_0705():
    # read_ion('electric', 'Regions', 'Electric_(KWH)')
    # read_ion('gas', 'Gas Regions', 'Gas_(CubicFeet)')
    # summary_long('electric')
    # summary_long('gas')
    # add_temperature_long()
    # add_area()
    # for measure_type in ['gas']:
    # for measure_type in ['electric']:
    for measure_type in ['electric', 'gas']:
        # plot_scatter_long('status', measure_type)
        plot_scatter_long('status_week_day_night', measure_type)
        # plot_scatter_long('cluster', measure_type)
        
def code_0712():
    # add_temperature_long_gsalink()
    for measure_type in ['electric', 'gas']:
        plot_scatter_long('status_week_day_night_nonflex', measure_type)
        # uo.dir2html(os.getcwd() + '/input/FY/interval/ion_0627/{0}_scatter/'.format(measure_type), '*_status_week_day_night_nonflex.png', 'ION {0} setback by year'.format(measure_type), '{0}_byyear.html'.format(measure_type))

        # files = glob.glob(os.getcwd() + '/input/FY/interval/ion_0627/{0}_scatter/*'.format(measure_type))
        # for f in files:
        #     shutil.copyfile(f, f.replace('/input/FY/interval/ion_0627/', '/plot_FY_weather/html/interval/scatter/'))
    print 'end'
    return
        
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

def main():
    # create_index_interval()
    code_0712()
    # check_0705()
    # for measure_type in ['electric', 'gas']:
    # for measure_type in ['electric']:
    #     plot_scatter('day', measure_type)
    #     plot_scatter('month', measure_type)
    #     plot_scatter('hour', measure_type)
    #     plot_scatter(None, measure_type)
        # plot_scatter('piece', measure_type)
        # plot_scatter('trend', measure_type)
    # add_temperature()
    # summary_wide('gas')
    # summary_wide('electric')
    # read_ion_single('electric', 'Regions')
    # read_ion_single('gas', 'Gas Regions')
    # compare_names()
    # compare_hourly('M')
    # read_interval_hour('H')
    # compare_names()
    # concat_consumption("Total Electric Consumption")
    # concat_consumption("Total Gas Consumption")
    # read_building_raw_energy('NE0036ZZ')
    # compare_month_daily('Electric')
    # compare_month_daily('Gas')
    # compare_month_daily_twomonth('Electric')
    # compare_month_daily_twomonth('Gas')
    # merge()
    return
    
main()
# import util_read as ur
# df = pd.read_csv('/home/yujiex/Desktop/LA0085ZZ_day.csv')
# hd = 'H BOGGS FED BLDG/COURTHOUSE NG.Total Gas Consumption (Int)'
# df.dropna(subset=[hd], inplace=True)
# df['Gas (ft3)'] = df[hd].map(lambda x: float(x[:-4]))
# df.to_csv('/home/yujiex/Desktop/LA0085ZZ_float.csv')
# ur.read_building_energy('LA0085ZZ', outpath='/home/yujiex/Desktop/')
