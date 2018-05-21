import os
import glob
import pandas as pd
import numpy as np
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
import matplotlib.dates as dates
from scipy import stats
from scipy import optimize
import pyqt_fit.nonparam_regression as smooth
from pyqt_fit import npr_methods
import calendar
import re
import textwrap as tw
from geopy.geocoders import Nominatim
# from geopy.distance import vincenty
import time
import geocoder
from vincenty import vincenty
import datetime
import json

import lean_temperature_monthly as ltm
import util
import label as lb

homedir = os.getcwd() + '/csv_FY/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
fldir = os.getcwd() + '/input/FL/'

# read weather data in a folder and convert them to csv
def excel2csv():
    print 'read excel file'
    # filename = os.getcwd() + '/input/FY/WeatherGSA.xlsx'
    filelist = glob.glob(os.getcwd() + '/input/FY/weather/*.xlsx')
    print filelist
    for f in filelist:
        filename = f[f.rfind('/') + 1: f.rfind('.')]
        df = pd.read_excel(f, sheetname=0)
        df.to_csv(homedir + 'weatherinput/{0}.csv'.format(filename))
        print 'output' + (homedir +
                          'weatherinput/{0}.csv'.format(filename))

# check and remove nan values for weather data
def check_data():
    filelist = glob.glob(homedir + 'weatherinput/*.csv')
    print filelist

    for f in filelist:
        df = pd.read_csv(f)
        # weather data is of the format:
        #                       KBOS
        #                       \\128.2.109.159\WeatherUnderground/KBOS/Temperature
        #
        #   2012-Sep-01 0:00:00 80.0999984741211
        #   2012-Sep-01 1:00:00 79.9899978637695
        #   2012-Sep-01 2:00:00 79

        # drop rows with empty time stamp
        df.dropna(subset=['Unnamed: 0'], inplace=True)
        cols = list(df)[1:]
        print 'number of columns before dropna: {0}'.format(len(cols))
        err_strings = []
        for col in cols:
            col_value = df[col].tolist()
            str_value = [x for x in col_value if is_string(x)]
            str_set = set(str_value)
            if len(str_set) != 0:
                err_strings += list(str_set)
        err_str_set = set(err_strings)
        print 'The set of error string: {0}'.format(set(err_str_set))

        df.replace(list(err_str_set), np.nan, inplace=True)
        print len(df)
        df.drop(len(df), axis=0, inplace=True)
        print len(df)
        df.dropna(axis=1, how='any', inplace=True)
        clean_cols = list(df)
        print 'number of columns after dropna: {0}'.format(len(clean_cols))
        assert('KDMH' not in df)
        df.to_csv(f.replace('.csv', '_nonan.csv'), index=False)
    return
    
# requires input files have the same time duration
def union_weatherinput():
    filelist = glob.glob(homedir + 'weatherinput/*_nonan.csv')
    print filelist
    df_base = pd.read_csv(filelist[0])
    existing_cols = list(df_base)
    print 'original number of stations: {0}'.format(len(existing_cols))
    length = len(df_base)
    other = filelist[1:]
    for f in filelist:
        df_i = pd.read_csv(f)
        print f[f.rfind('/') + 1:]
        assert(len(df_i) == length)
        col_to_use = list(df_i.columns - df_base.columns)
        col_to_use.append('Unnamed: 0')
        df_base = pd.merge(df_base, df_i[col_to_use], on='Unnamed: 0',
                           how='inner')
        print(len(df_base))
        existing_cols = list(df_base)
        print 'number of stations after merge: {0}'.format(len(existing_cols))
    cols = list(df_base)
    assert(len(cols) == len(set(cols)))
    df_base.to_csv(homedir + 'weatherData_nonan.csv', index=False)

def is_string(s):
    # float, with possible trailing e-0x
    pattern = re.compile('-?[0-9]{0,3}\.?[0-9]{0,20}(e-[0-9]{2})?$')
    if type(s) is str:
        return (not re.match(pattern, s))
    else:
        return False

def check_data_single(f):
    filename = f[f.rfind('/') + 1:]
    s = filename[:4]
    outfile = (weatherdir + 'weather_nostr/{0}'.format(filename))
    df = pd.read_csv(f)
    col_value = df[s].tolist()
    str_value = [x for x in col_value if is_string(x)]
    str_set = set(str_value)
    if len(str_set) > 0:
        print s, list(str_set)
        df.replace(list(str_set), np.nan, inplace=True)
    df.to_csv(outfile, index=False)

def get_mean_temp():
    outfile = (homedir + 'weatherData_meanTemp.csv')
    print 'write mean temperature to {0}'.format(outfile)
    df = pd.read_csv(homedir + 'weatherData_nonan.csv')
    df.set_index(pd.DatetimeIndex(df['Unnamed: 0']), inplace=True)
    df.resample('M', how = 'mean').to_csv(outfile, index=True)

def get_mean_temp_single(f):
    filename = f[f.rfind('/') + 1:]
    outfile = (weatherdir + 'station_temp/{0}'.format(filename))
    print 'write mean temperature to {0}'.format(filename)
    df = pd.read_csv(f)
    df.drop('Timestamp', axis=1, inplace=True)
    df.set_index(pd.DatetimeIndex(df['localTime']), inplace=True)
    df = df.resample('M', how = 'mean').to_csv(outfile, index=True)
    df2 = pd.read_csv(outfile)
    df2['year'] = df2['Unnamed: 0'].map(lambda x: x[:4])
    df2['month'] = df2['Unnamed: 0'].map(lambda x: int(x[5: 7]))
    df2.rename(columns={'Unnamed: 0': 'timestamp'}, inplace=True)
    cols = list(df2)
    cols.remove('year')
    cols.remove('month')
    cols.insert(1, 'month')
    cols.insert(1, 'year')
    df2 = df2[cols]
    df2.to_csv(outfile, index=False)

def get_DD_itg_single(f, baselist, theme):
    starttime = time.time()
    df = pd.read_csv(f)
    filename = f[f.rfind('/') + 1:]
    s = filename[:4]
    df.drop('Timestamp', axis=1, inplace=True)
    df.set_index(pd.DatetimeIndex(df['localTime']), inplace=True)
    df_hour = df.resample('H', how = 'mean')

    for base in baselist:
        base_hd = '{0}F'.format(base)
        if theme == 'HDD':
            df_hour[base_hd] = df_hour[s].map(lambda x: 0 if x >= base else base - x)
        else:
            df_hour[base_hd] = df_hour[s].map(lambda x: 0 if x <= base else x - base)
    df_day = df_hour.resample('D', how = 'mean')
    df_month = df_day.resample('M', how = 'sum')
    df_month.drop(s, axis=1, inplace=True)
    df_month.to_csv(weatherdir + 'station_dd/{0}_{1}.csv'.format(theme, s), index=True)
    df_month = pd.read_csv(weatherdir + \
                           'station_dd/{0}_{1}.csv'.format(theme, s))
    df_month.rename(columns={'Unnamed: 0': 'timestamp'}, inplace=True)
    df_month['year'] = df_month['timestamp'].map(lambda x: x[:4])
    df_month['month'] = df_month['timestamp'].map(lambda x: x[5:7])
    cols = list(df_month)
    cols.remove('year')
    cols.remove('month')
    cols.insert(1, 'year')
    cols.insert(1, 'month')
    df_month = df_month[cols]
    df_month.to_csv(weatherdir + 'station_dd/{0}_{1}.csv'.format(s, theme), index=False)
    print 'calculating {0} for {1} in {2} s'.format(theme, s, round(time.time() - starttime, 2))

def get_DD_itg(base, theme):
    df = pd.read_csv(homedir + 'weatherData_nonan.csv')
    df.set_index(pd.DatetimeIndex(df['Unnamed: 0']), inplace=True)
    df_hour = df.resample('H', how = 'mean')

    for col in df_hour:
        if theme == 'HDD':
            df_hour[col] = df_hour[col].map(lambda x: 0 if x >= base else base - x)
        else:
            df_hour[col] = df_hour[col].map(lambda x: 0 if x <= base else x - base)
    df_day = df_hour.resample('D', how = 'mean')
    print 'base temperature: {0}'.format(base)
    print df_day['KBOS'].head()
    df_day.to_csv(homedir + 'degreeday/Day{1}_itg_{0}F.csv'.format(int(base), theme))
    df_month = df_day.resample('M', how = 'sum').to_csv(homedir + 'degreeday/{1}_itg_{0}F.csv'.format(int(base), theme))

# read energy of building b
def read_energy(b):
    filelist = glob.glob(os.getcwd() + '/csv_FY/single_eui_cal/{0}*.csv'.format(b))
    if len(filelist) == 0:
        return pd.DataFrame({'year': [], 'month': []})
    dfs = [pd.read_csv(csv) for csv in filelist]
    df_all = pd.concat(dfs, ignore_index=True)
    df_all.sort(columns=['year', 'month'], inplace=True)
    return df_all

# read temperature record from Oct. 2012 to Sep. 2015
def read_temperature():
    df = pd.read_csv(homedir + 'weatherData_meanTemp.csv')
    df.drop(0, axis=0, inplace=True)
    return df

# read icao cade (4-alphabetical char) of weather station
def read_icao():
    names = ['Block Number', 'Station Number', 'ICAO Location', 'Indicator',
             'Place Name', 'State', 'Country Name', 'WMO Region',
             'Station Latitude', 'Station Longitude', 'Upper Air Latitude',
             'Upper Air Longitude', 'Station Elevation (Ha)',
             'Upper Air Elevation (Hp)', 'RBSN indicator']
    df = pd.read_csv(homedir + 'nsd_bbsss.txt', sep=';',
                     header=None, names=names)

    df['WMO ID'] = df.apply(lambda row: str(row['Block Number']).zfill(2) + str(row['Station Number']).zfill(3), axis=1)
    df = df[['WMO ID', 'ICAO Location']]
    return df

def read_ghcnd():
    names = ['ID', 'LATITUDE', 'LONGITUD', 'ELEVATION', 'STATE', 'NAME',
             'GSN FLAG', 'HCN/CRN FLAG', 'WMO ID']
    filename = homedir + 'ghcnd-stations.txt'
    outfile = homedir + 'ghcnd-stations-delim.txt'
    with open (filename, 'r') as rd:
        lines = rd.readlines()
    with open (outfile ,'w+') as wt:
        for line in lines:
            line_list = list(line)
            for i in [11, 20, 30, 37, 40, 71, 75, 79]:
                line_list[i] = ','
            new_line = ''.join(line_list)
            wt.write(new_line)
    df = pd.read_csv(homedir + 'ghcnd-stations-delim.txt',
                     header=None, names=names)
    df = df[['ID', 'WMO ID']]
    return df

# read climate normal
def read_ncdc():
    import calendar
    names = ['ID'] + [calendar.month_abbr[i] for i in range(1, 13)]
    df = pd.read_csv(homedir + 'mly-tavg-normal.txt',
                     delim_whitespace=True, header=None, names=names)
    # checked, no special value in the file
    for col in df:
        if col != 'ID':
            df[col] = df[col].map(lambda x: int(x[:-1])/10.0)
    return df

# return lookup table of ical to ghcnd
def read_temp_norm():
    df_icao = read_icao()
    df_ghcnd = read_ghcnd()
    #df_ghcnd.to_csv(homedir + 'ghcnd.csv', index=False)
    df_merge = pd.merge(df_icao, df_ghcnd, on='WMO ID', how='left')
    df_merge = df_merge[df_merge['ICAO Location'] != '----']
    #df_merge.to_csv(homedir + 'icao_ghcnd.csv',
    #                index=False)
    df_temp = read_ncdc()
    df_all = pd.merge(df_merge, df_temp, on='ID', how='left')
    #df_all.to_csv(homedir + 'icao_ghcnd_ncdc.csv',
    #              index=False)
    return df_all

def plot_energy_temp(df_energy, df_temp, theme, b, s):
    df = pd.DataFrame({'energy': df_energy[theme], 'temp': df_temp[s]})
    sns.regplot('temp', 'energy', data=df, fit_reg=False)
    P.savefig(os.getcwd() + '/plot_FY_weather/{2}/{0}_{1}.png'.format(b, s, theme), dpi = 150)
    plt.title('Temperature-{0} plot: {1}, {2}'.format(theme, b, s))
    plt.close()
    return

def plot_energy_temp_byyear_2015(theme):
    sns.set_palette(sns.color_palette('Set2', 27))
    sns.mpl.rc("figure", figsize=(10,5))
    cat_df = pd.read_csv(os.getcwd() + '/csv_FY/join_cal/join_2015.csv')
    cat_dict = dict(zip(cat_df['Building Number'].tolist(),
                        cat_df['Cat'].tolist()))
    filelist = glob.glob(os.getcwd() + '/csv_FY/energy_temperature_select/*_{0}.csv'.format(lb.title_dict[theme]))
    def getname(dirname):
        id1 = dirname.find('select') + len('select') + 1
        return dirname[id1: id1 + 8]
    buildings = [getname(f) for f in filelist]
    dfs = [pd.read_csv(csv) for csv in filelist]
    dfs = [df[df['Fiscal Year'] == 2015] for df in dfs]
    euis = [round(df[theme].sum(), 2) for df in dfs]
    sorted_bedf = sorted(zip(buildings, euis, dfs), key=lambda x: x[1], reverse=True)
    buildings = [x[0] for x in sorted_bedf]
    euis = [x[1] for x in sorted_bedf]
    dfs = [x[2] for x in sorted_bedf]
    lines = []
    for i in range(len(buildings)):
        df = dfs[i]
        df.sort(['temperature', theme], inplace=True)
        line, = plt.plot(df['temperature'], df[theme])
        lines.append(line)
    labels = ['{0}: {1} kBtu/sq.ft*year_{2}'.format(b, e, cat_dict[b]) for (b, e) in zip(buildings, euis)]
    plt.title('Temperature-{0} plot: 27 Building, Fiscal Year 2015'.format(lb.title_dict[theme]))
    plt.xlabel(xlabel_temp, fontsize=12)
    plt.ylabel(ylabel_dict[theme], fontsize=12)
    plt.legend(lines, labels, bbox_to_anchor=(0.2, 1), prop={'size':6})
    P.savefig(os.getcwd() + '/plot_FY_weather/27building_{0}_2015_trunc.png'.format(theme), dpi = 150)
    #P.savefig(os.getcwd() + '/plot_FY_weather/27building_{0}_2015.png'.format(theme), dpi = 150)
    plt.close()
    return
    
#ld: line or dot plot
def plot_energy_temp_byyear(df_energy, df_temp, df_hdd, df_cdd, theme,
                            b, s, ld, kind, remove0):
    sns.set_palette(sns.color_palette('Set2', 9))
    sns.mpl.rc("figure", figsize=(10,5))
    df = df_energy
    df['temp'] = df_temp[s].tolist()
    df['hdd'] = df_hdd[s].tolist()
    df['hdd'] = df['hdd'] * (-1.0)
    df['cdd'] = df_cdd[s].tolist()
    df.to_csv(os.getcwd() + '/csv_FY/energy_temperature_select/{0}_{1}_{2}.csv'.format(b, s, lb.title_dict[theme]), index=False)
    df1 = df.copy()
    df1['dd'] = df1['hdd']
    df2 = df.copy()
    df2['dd'] = df2['cdd']
    df3 = pd.concat([df1, df2], ignore_index=True)
    if kind != 'all':
        print df[kind].head()
        df = df[df[kind] != 0.0]
        if ld == 'line':
            gr = df.groupby('Fiscal Year')
            lines = []
            for name, group in gr:
                print (name, kind)
                group.sort([kind, theme], inplace=True)
                group = group[[kind, theme]]
                line, = plt.plot(group[kind], group[theme])
                lines.append(line)
                #print 'Building: {0}, year: {1}, {2} {3} [kbtu/sq.ft.]'.format(b, int(name), round(group[theme].sum(), 2), lb.title_dict[theme])
        else:
            if kind == 'cdd':
                sns.set_palette(sns.color_palette('Blues'))
            elif kind == 'hdd':
                sns.set_palette(sns.color_palette('Oranges'))
            sns.lmplot(x=kind, y=theme, hue='Fiscal Year', data=df, fit_reg=True)
            x = np.array(df[kind])
            y = np.array(df[theme])
            t_min = df[kind].min()
            t_max = df[kind].max()
            xd = np.r_[t_min:t_max:1]
            k1 = smooth.NonParamRegression(x, y, method=npr_methods.LocalPolynomialKernel(q=1))
            plt.plot(xd, k1(xd), '-', color=sns.color_palette('Set2')[5])
            plt.xlabel(lb.xlabel_dict[kind], fontsize=12)
            plt.ylabel(ylabel_dict[theme], fontsize=12)
            plt.title('{3}-{0} plot: Building {1}, Station {2}'.format(lb.title_dict[theme], b, s, ld.kind_dict[kind]))
    else:
        if ld == 'line':
            gr = df.groupby('Fiscal Year')
            lines = []
            for name, group in gr:
                print (name, kind)
                group_elec = group.sort(['cdd', 'eui_elec'])
                group_gas = group.sort(['hdd', 'eui_gas'])
                # offset temperature to 0F
                group['temp'] = group['temp'] - 65.0
                group['temp_dd'] = group.apply(lambda r: r['hdd'] if r['temp'] < 0 else r['cdd'], axis=1)
                group_temp = group.sort(['temp_dd', 'eui'])
                if remove0:
                    group_elec = group_elec[group_elec['cdd'] >= 10]
                    group_gas = group_gas[group_gas['hdd'] <= -10]
                group_temp = group.sort(['temp', 'eui'])
                line_elec, = plt.plot(group_elec['cdd'],
                                      group_elec['eui_elec'])
                line_gas, = plt.plot(group_gas['hdd'],
                                     group_gas['eui_gas'])
                line_temp, = plt.plot(group_temp['temp_dd'], group_temp['eui'])
                lines.append(line_elec)
                lines.append(line_gas)
                lines.append(line_temp)
            plt.ylabel(ylabel_dict[theme], fontsize=12)
            plt.title('{3}-{0} plot: Building {1}, Station {2}'.format(lb.title_dict[theme], b, s, ld.kind_dict[kind]))
        else:
            base_load = df3.copy()
            gr_base = base_load.groupby('Fiscal Year')
            base_gas_dict = {}
            base_elec_dict = {}
            for name, group in gr_base:
                tempdf_gas = group.copy()
                tempdf_elec = group.copy()
                tempdf_gas['base_month'] = tempdf_gas['month'].map(lambda x: True if x == 12 or x < 3 else False)
                tempdf_gas = tempdf_gas[tempdf_gas['base_month'] == True]
                print tempdf_gas
                base_gas_dict[name] = tempdf_gas['eui_gas'].mean()

                tempdf_elec['base_month'] = \
                  ap(lambda x: True \
                        if (6 <= x and x <= 8) else False)
                tempdf_elec = tempdf_elec[tempdf_elec['base_month'] == True]
                print tempdf_elec
                base_elec_dict[name] = tempdf_elec['eui_elec'].mean()

            print base_elec_dict
            print base_gas_dict
            if remove0:
                df3 = df3[df3['dd'].abs() >= 30]
            df_elec = df3.copy()
            df_elec['kind'] = 'Electricity'
            df_elec['eui_plot'] = df_elec['eui_elec']
            df_gas = df3.copy()
            df_gas['kind'] = 'Natural Gas'
            df_gas['eui_plot'] = df_gas.apply(lambda r: r['eui_gas'] + base_elec_dict[r['Fiscal Year']], axis=1)
            df_total = df3.copy()
            df_total['kind'] = 'Total'
            df_total['eui_plot'] = df_gas['eui']
            df_base_elec = df3.copy()
            df_base_elec['kind'] = 'Base-Electricity'
            df_base_elec['eui_plot'] = df_base_elec['Fiscal Year'].map(lambda x: base_elec_dict[x])
            df_base_gas = df3.copy()
            df_base_gas['kind'] = 'Base-Gas'
            df_base_gas['eui_plot'] = df_base_gas['Fiscal Year'].apply(lambda r: base_gas_dict[r] + base_elec_dict[r])
            df_all = pd.concat([df_elec, df_gas, df_total, df_base_gas, df_base_elec], ignore_index=True)
            #df_all = pd.concat([df_elec, df_gas, df_total], ignore_index=True)
            g = sns.lmplot(x='dd', y='eui_plot', data=df_all, col='Fiscal Year', hue = 'kind', fit_reg=True, truncate=True, lowess=True)
            #plt.xlabel(lb.xlabel_dict[kind], fontsize=12)
            g = g.set_axis_labels(lb.xlabel_dict[kind],
                                  ylabel_dict[kind])
    plt.ylim((0, 7))
    if ld == 'line':
        if kind != 'all':
            P.savefig(os.getcwd() + '/plot_FY_weather/eui_{3}/{2}_byyear/{0}_{1}.png'.format(b, s, theme, kind), dpi = 150)
        else:
            years = ['2013', '2014', '2015']
            line_labels = ['Electricity', 'Gas', 'Total']
            labels = reduce(lambda x, y: x + y, [['{0}-{1}'.format(x, y) for y in line_labels] for x in years])
            plt.legend(lines, labels)
            P.savefig(os.getcwd() + '/plot_FY_weather/eui_{2}/{2}_byyear/{0}_{1}.png'.format(b, s, kind), dpi = 150)

    else:
        if kind != 'all':
            print (b, s, theme, kind)
            P.savefig(os.getcwd() + '/plot_FY_weather/eui_{3}/{2}_byyear_dot/{0}_{1}.png'.format(b, s, theme, kind), dpi = 75)
        else:
            P.savefig(os.getcwd() + '/plot_FY_weather/eui_{2}/{2}_byyear_dot/{0}_{1}.png'.format(b, s, kind), dpi = 75)
    plt.close()

def regression_hdd(t, s, df, theme):
    df_temp = pd.read_csv(homedir + 'weatherData_HDD_{0}F.csv'.format(t))
    df_temp.drop(0, inplace=True)
    slope, intercept, r_value, p_value, std_err = stats.linregress(df_temp[s], df[theme])
    return (slope, intercept, r_value * r_value, t)

def regression_gas_temp(b, s, df, theme):
    x = np.array(df['temperature'])
    y = np.array(df[theme])
    def piecewise_linear(x, x0, y0, k1, k2):
        return np.piecewise(x, [x < x0], [lambda x:k1*x + y0-k1*x0, lambda x:k2*x + y0-k2*x0])
    p , e = optimize.curve_fit(piecewise_linear, x, y)
    t_min = df['temperature'].min()
    t_max = df['temperature'].max()
    xd = np.linspace(t_min, t_max, 15)
    plt.plot(x, y, "o")
    plt.plot(xd, piecewise_linear(xd, *p))
    P.savefig(os.getcwd() + '/plot_FY_weather/eui_gas_piece/{0}_{1}.png'.format(b, s), dpi=150)
    plt.close()
    return

def regression_gas_kernel(b, s, df, theme):
    x = np.array(df['temperature'])
    y = np.array(df[theme])
    t_min = df['temperature'].min()
    t_max = df['temperature'].max()
    xd = np.r_[t_min:t_max:1]
    k1 = smooth.NonParamRegression(x, y, method=npr_methods.LocalPolynomialKernel(q=1))
    plt.plot(x, y, "o")
    plt.plot(xd, k1(xd))
    plt.xlabel(xlabel_temp, fontsize=12)
    plt.ylabel(ylabel_dict[theme], fontsize=12)
    plt.title('Kernel Regression Fit {0} - Temperature Plot\n Building {1}, Station {2}'.format(lb.title_dict[theme], b, s), fontsize=15)
    P.savefig(os.getcwd() + '/plot_FY_weather/eui_gas_kernel/{0}_{1}.png'.format(b, s), dpi=150)
    plt.close()
    return k1

# Smoothing spline
def plot_normal(df, theme, b, s):
    title_weather = {'eui':'Original and Weather Normalized '\
                        'Electricity + Gas Consumption',
                    'eui_elec':'Original and Weather Normalized '
                            'Electricity Consumption',
                    'eui_gas':'Original and Weather Normalized Natural '\
                            'Gas Consumption',
                    'eui_oil':'Original and Weather Normalized Oil Consumption',
                    'eui_water':'Original and Weather Normalized '\
                                'Water Consumption'}
    sns.set_palette(sns.color_palette('Paired', 8))
    gr = df.groupby('year')
    lines = []
    for name, group in gr:
        ori,  = plt.plot(group['month'], group[theme])
        norm, = plt.plot(group['month'], group['e_norm'])
        lines.append(ori)
        lines.append(norm)
    plt.legend(lines, ['2012_ori', '2012_norm', '2013_ori', '2013_norm', '2014_ori', '2014_norm', '2015_ori', '2015_norm'], bbox_to_anchor=(0.2, 1))
    plt.title('{0}\nBuilding {1}'.format(title_weather[theme], b), fontsize=15, x = 0.5, y = 1)
    plt.xlim((1, 12))
    plt.xlabel('Month', fontsize=12)
    plt.xticks(range(1, 13), [calendar.month_abbr[m] for m in range(1, 13)])
    plt.ylabel(ylabel_dict[theme], fontsize=12)
    P.savefig(os.getcwd() + '/plot_FY_weather/{2}_ori_norm/{0}_{1}.png'.format(b, s, theme), dpi=150)
    plt.close()

def plot_normal_only(df, theme, b, s):
    sns.set_palette(sns.color_palette('Set2', 4))
    gr = df.groupby('year')
    lines = []
    for name, group in gr:
        norm, = plt.plot(group['month'], group['e_norm'])
        lines.append(norm)
    plt.legend(lines, ['2012_norm', '2013_norm', '2014_norm', '2015_norm'], bbox_to_anchor=(0.2, 1))
    plt.title('{0}\nBuilding {1}'.format(lb.title_dict_3[theme], b), fontsize=15, x = 0.5, y = 1)
    plt.xlim((1, 12))
    plt.xlabel('Month', fontsize=12)
    plt.xticks(range(1, 13), [calendar.month_abbr[m] for m in range(1, 13)])
    plt.ylabel(ylabel_dict[theme], fontsize=12)
    P.savefig(os.getcwd() + '/plot_FY_weather/{2}_norm/{0}_{1}.png'.format(b, s, theme), dpi=150)
    plt.close()

def get_gsalink_set():
    df = pd.read_csv(os.getcwd() + '/input/FY/GSAlink 81 Buildings Updated 9_22_15.csv')
    return list(set(df['Building ID'].tolist()))

# BOOKMARK
def gsalink_action():
    df_gsa = pd.read_csv(os.getcwd() + '/input/FY/Portfolio HPGB Dashboard_gsaLink.csv')

def calculate(theme, method):
    df_bs = pd.read_csv(weatherdir + 'building_station_lookup.csv')
    bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
    study_set = get_gsalink_set()
    bs_pair = [x for x in bs_pair if x[0] in study_set]
    df_temperature = read_temperature()
    df_temp_norm = read_temp_norm()
    df_temp_norm.drop(['WMO ID', 'ID'], axis=1, inplace=True)
    df_hdd_65 = pd.read_csv(homedir + 'weatherData_HDD_itg_65F.csv')
    df_hdd_65.drop(0, axis=0, inplace=True)
    df_cdh_65 = pd.read_csv(homedir + 'weatherData_CDD_itg_65F.csv')
    df_cdh_65.drop(0, axis=0, inplace=True)

    '''
    t_norm = df_temp_norm[df_temp_norm['ICAO Location'] == s]
    print len(t_norm)
    if len(t_norm) == 0:
        print s
        continue
    t_norm_list = list(list(t_norm.itertuples())[0])[2:]
    '''
    for b, s in bs_pair[:1]:
        print (b, s)
        df_energy = read_energy(b)[['Fiscal Year', 'year', 'month', 'eui_elec', 'eui_gas', 'eui']]
        # 2012 to 2015 data
        df_energy = df_energy[-48:] # remove this when weather data available
        df_t = df_temperature[[s]][-48:]
        df_h = df_hdd_65[[s]][-48:]
        df_c = df_cdh_65[[s]][-48:]
        plot_energy_temp_byyear(df_energy, df_t, df_h, df_c, theme,
                                b, s, 'dot', 'all', True)
        '''
        if theme == 'eui_gas':
            plot_energy_temp_byyear(df_energy, df_t, df_h, df_c, theme, b, s,
                    'dot', 'hdd', True)
        elif theme == 'eui_elec':
            plot_energy_temp_byyear(df_energy, df_t, df_h, df_c, theme, b, s,
                    'dot', 'cdd', True)
        plot_energy_temp_byyear(df_energy, df_t, df_h, df_c, theme,
                                b, s, 'line', 'all', True)
        '''

def getICAO(StateAbbr, City, Address, zipcode, df_lookup):
    df = df_lookup[df_lookup['StateAbbr'] == StateAbbr]
    City = City.upper()
    cities = df['City'].tolist()
    counter = 0
    if City in cities:
        df.set_index('City', inplace=True)
        return df.ix[City, 'ICAO']
    else:
        geolocator = Nominatim()
        location = geolocator.geocode('{0},{1},{2},{3}'.format(Address, City, StateAbbr, zipcode))
        #location = geolocator.geocode('{0}'.format(zipcode))
        if location == None:
            return 'Not Found'
        print location
        df['distance'] = df.apply(lambda r: vincenty((location.latitude, location.longitude), (r['Lat'], r['Long'])).miles, axis=1)
        min_distance = df['distance'].min()
        df = df[df['distance'] == min_distance]
        return df['ICAO'].iloc[0]

def pm_static_info_2_station():
    df = pd.read_csv(os.getcwd() + '/csv/all_column/sheet-0-all_col.csv')
    df = df[['Property Name', 'Street Address', 'City/Municipality',
             'State/Province', 'Postal Code']]
    df['Property Name'] = df['Property Name'].map(lambda x: x.partition(' ')[0][:8])
    df['Postal Code'] = df['Postal Code'].map(lambda x: x[:5])

    df_state = pd.read_csv(os.getcwd() + '/input/FY/state2abbr.csv')
    df_state = df_state[['State', 'Postal']]
    df_all = pd.merge(df, df_state, left_on='State/Province', right_on='State', how='left')
    df_all.drop('State', axis=1, inplace=True)
    df_all.rename(columns={'Property Name' : 'Building Number',
                           'City/Municipality' : 'City',
                           'State/Province' : 'State',
                           'Postal': 'StateAbbr',
                           'Postal Code': 'zipcode'}, inplace=True)
    df_stationlookup = pd.read_csv(homedir + 'weatherinput/Weather Station Mapping.csv')
    df_stationlookup = df_stationlookup[['City', 'ICAO', 'StateAbbr', 'Lat', 'Long']]
    step = 50
    dfs = [df_all[i * step: (i + 1) * step] for i in range(0, len(df_all)/step)]
    for i in range(len(dfs))[:1]:
        print i, len(dfs[i])
        starttime = time.time()
        dfs[i]['Weather Station'] = dfs[i].apply(lambda r: getICAO(r['StateAbbr'], r['City'], r['Street Address'], r['zipcode'], df_stationlookup), axis=1)
        endtime = time.time()
        # FIXME: print non- "Not Found" result count
        dfs[i].to_csv(homedir + 'location_info_{0}.csv'.format(i), index=False)
        print endtime-starttime
    print 'end'

# generate building station lookup table from state and city
def building_to_station():
    df_location = pd.read_csv(homedir + 'master_table/building_stationAvailability.csv')
    df_location = df_location[df_location['Valid Weather Data'] == 1]
    df_location = df_location[['Building Number', 'ICAO']]
    df = pd.read_csv(os.getcwd() + '/csv_FY/filter_bit/fis/indicator_all.csv')
    df = df[df['good_area'] == 1]
    df = df[['Building Number']]
    df_all = pd.merge(df, df_location, on='Building Number', how='inner')
    # df_all.dropna(subset=['ICAO'], inplace=True)
    df_all = df_all[df_all['ICAO'] != 'Not Found']
    print df_all.head()
    df_all.to_csv(weatherdir + 'building_station_lookup.csv', index=False)
    return

# deprecated
def building_to_station_dep():
    df = pd.read_csv(os.getcwd() + '/csv_FY/filter_bit/fis/indicator_all.csv')
    df = df[df['good_area'] == 1]
    good_building = set(df['Building Number'].tolist())
    good_station = set(list(pd.read_csv(homedir + 'weatherData_meanTemp.csv')))
    station_info = pd.read_csv(homedir + 'weatherStation.csv')
    station_info = station_info[['Building Number', 'Weather Station']]
    print len(station_info)
    station_info.dropna(inplace=True)
    print len(station_info)
    station_info['Weather Station'] = station_info['Weather Station'].map(lambda x: x.replace(' ', ''))
    station_info.drop_duplicates(cols='Building Number', inplace=True)
    #print '#{0}#'.format(station_info.iloc[60, 1])
    station_info = station_info[station_info['Weather Station'].isin(good_station)]
    station_info = station_info[station_info['Building Number'].isin(good_building)]
    print 'number of building with station {0}'.format(len(station_info))
    station_info.to_csv(homedir + 'building_station_lookup.csv', index=False)

# read building_station lookup table to pair list
def read_building_weather(path, id_col, weather_col):
    df = pd.read_csv(path)
    df.dropna(subset=[weather_col], inplace=True)
    df = df[df['Valid Weather Data'] == 1]
    return zip(df[id_col].tolist(), df[weather_col].tolist())

def sep_dd(kind, low, high):
    print 'start separating degree day'
    files_dd = [homedir + 'degreeday/{1}_itg_{0}F.csv'.format(i, kind)\
                for i in range(low, high)]
    keys = ['{0}F'.format(x) for x in range(low, high)]
    dfs_dd = [pd.read_csv(f) for f in files_dd]
    # time column
    col_time = dfs_dd[0].iloc[:, 0]
    stations = list(dfs_dd[0])[1:]
    for s in stations:
        dd_list = [df[s].tolist() for df in dfs_dd]
        d = dict(zip(keys, dd_list))
        d['timestamp'] = col_time
        df_s = pd.DataFrame(d)
        df_s['year'] = df_s['timestamp'].map(lambda x: x[:4])
        df_s['month'] = df_s['timestamp'].map(lambda x: x[5: 7])
        re_order_col = list(df_s)
        re_order_col.remove('year')
        re_order_col.remove('month')
        re_order_col.remove('timestamp')
        re_order_col = ['timestamp', 'year', 'month'] + re_order_col
        df_s = df_s[re_order_col]
        df_s.to_csv(homedir + 'station_dd/{0}_{1}.csv'.format(s, kind),
                    index=False)
    print 'end separating degree day'

def sep_temp():
    print 'start separating temperature'
    df_temp = pd.read_csv(homedir + 'weatherData_meanTemp.csv')
    col_time = df_temp.iloc[:, 0]
    stations = list(df_temp)[1:]
    for s in stations:
        d = {s: df_temp[s], 'timestamp': col_time}
        df_s = pd.DataFrame(d)
        df_s['year'] = df_s['timestamp'].map(lambda x: x[:4])
        df_s['month'] = df_s['timestamp'].map(lambda x: int(x[5: 7]))
        re_order_col = list(df_s)
        re_order_col.remove(s)
        re_order_col.append(s)
        df_s = df_s[re_order_col]
        df_s.to_csv(homedir + 'station_temp/{0}.csv'.format(s),
                    index=False)
    print 'end separating temperature'

def read_mean_temp(s):
    df = pd.read_csv(homedir + 'station_temp/{0}.csv'.format(s))
    return df

def join_building_temp(bs_pair):
    for (b, s) in bs_pair:
        df_energy = read_energy(b)
        print (b, s, len(df_energy))
        if len(df_energy) == 0:
            print 'No energy info for {0}'.format(b)
            continue
        df_energy['eui_heat'] = df_energy['eui_gas'] + \
            df_energy['eui_oil'] + df_energy['eui_steam']
        cols = list(df_energy)
        cols.remove('eui_heat')
        cols.insert(cols.index('eui_water'), 'eui_heat')
        df_energy = df_energy[cols]
        df_temp = pd.read_csv(weatherdir + 'station_temp/{0}.csv'.format(s)) 
        df = pd.merge(df_energy, df_temp, how='inner', on=['year',
                                                           'month'])
        df.to_csv(weatherdir + 'energy_temp/{0}_{1}.csv'.format(b, s),
                  index=False)

def join_err_msg_cnt():
    df = pd.read_csv(weatherdir + 'weatherinput/check_station_log.txt')
    print df.head()
    stations = df['ICAO'].tolist()
    df.set_index('ICAO', inplace=True)
    counts = []
    for s in stations:
        df_s = pd.read_csv(weatherdir + 'weatherinput/by_station/{0}.csv'.format(s))
        df_s = df_s[df_s[s] == df.ix[s]['err_message']]
        count = len(df_s)
        print s, count
        counts.append(count)
    df['count'] = counts
    print df
    df.to_csv(weatherdir + 'weatherinput/station_errline_count.csv',
              index=True)

def process_single_stations(missing_stations):
    if len(missing_stations) == 0:
        filelist = glob.glob(weatherdir + 'weatherinput/by_station/*.csv')
    else:
        filelist = ['{0}weatherinput/by_station/{1}.csv'.format(weatherdir, s) for s in missing_stations]
    length = len(filelist)
    for i in range(length):
        f = filelist[i]
        print i
        check_data_single(f)
    # join_err_msg_cnt()
    return

def process_weatherfile(source, missing_stations):
    if source == 'old':
        # slow, comment out once you have the output
        # excel2csv()

        # check_data()
        # union_weatherinput()
        # get_mean_temp()
        calculate_dd()
        sep_dd('HDD', 40, 81)
        sep_dd('CDD', 40, 81)
        sep_temp()

        building_to_station()
        df_bs = pd.read_csv(weatherdir + 'building_station_lookup.csv')
        bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
        join_building_temp(bs_pair)
    else:
        # BOOKMARK
        # process_single_stations(missing_stations)
        if len(missing_stations) == 0:
            filelist = glob.glob(weatherdir + 'weather_nostr/*.csv')
        else:
            filelist = ['{0}weather_nostr/{1}.csv'.format(weatherdir, s) for s in missing_stations]
        length = len(filelist)
        baselist = range(40, 81)
        for i in range(length):
            f = filelist[i]
            print i
            # get_mean_temp_single(f)
            # get_DD_itg_single(f, baselist, 'HDD')
            # get_DD_itg_single(f, baselist, 'CDD')
        # building_to_station()
        df_bs = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
        df_bs.info()
        df_bs = df_bs[df_bs['Valid Weather Data'] == 1]
        bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
        # join_building_temp(bs_pair)

        for (b, s) in bs_pair:
            join_dd_temp_energy(b, s, 'CDD')
            join_dd_temp_energy(b, s, 'HDD')
    # plot_building_temp()
    return

def calculate_dd():
    for base in range(40, 81):
        get_DD_itg(base, 'HDD')
        get_DD_itg(base, 'CDD')

def join_dd_temp_energy(b, s, kind):
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
    plot_dd_fit(df_all, slope_opt, intercept_opt, r_opt, p_opt,
                col_opt, theme, kind, b, s, timerange)
    return ordered_result[0]

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
    plt.ylabel(ylabel_dict[theme])
    P.savefig(os.getcwd() + '/plot_FY_weather/dd_energy/{2}/{0}_{1}_{3}.png'.format(b, s, theme, timerange), dpi = 150)
    plt.close()

# s: station id
def plot_temp_fit(df_all, basetemp, b, s, kind, theme, base_load):
    print (basetemp, b, s, kind, theme)
    x = df_all[s]
    y = df_all[theme]
    tmin = df_all[s].min()
    tmax = df_all[s].max()
    pairs = zip(x, y)
    base = int(basetemp[:2])
    left = [p for p in pairs if p[0] < base]
    right = [p for p in pairs if p[0] >= base]
    left_x = [p[0] for p in left]
    left_y = [p[1] for p in left]
    right_x = [p[0] for p in right]
    right_y = [p[1] for p in right]
    if len(left) > 0:
        left_ave = sum(left_y)/len(left)
        slope_l, intercept_l, r_value_l, p_value_l, std_err_l = \
            stats.linregress(left_x, left_y)
    if len(right) > 0:
        right_ave = sum(right_y)/len(right)
        slope_r, intercept_r, r_value_r, p_value_r, std_err_r = \
            stats.linregress(right_x, right_y)
    def fit(x, slope, intercept):
        return np.array([slope * xi + intercept for xi in x])
    def ave(x, length):
        average = sum(x) / len(x)
        return np.array([average] * length)
    sns.set_style("white")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1.5)
    #sns.mpl.rc("figure", figsize=(10,5.5))
    plot_x_left = np.array(left_x)
    plot_y_left = np.array(left_y)
    plot_tmin_left = tmin
    plot_tmax_left = base
    xd_left = np.r_[plot_tmin_left:plot_tmax_left:1]
    plot_x_right = np.array(right_x)
    plot_y_right = np.array(right_y)
    plot_tmin_right = base
    plot_tmax_right = tmax 
    xd_right = np.r_[plot_tmin_right:plot_tmax_right:1]
    bx = plt.axes()
    bx.plot(plot_x_left, plot_y_left, "o")
    bx.plot(plot_x_right, plot_y_right, "o")
    mean = -1.0
    if kind == 'HDD':
        if len(xd_right) > 0:
            '''
            meanlist = ave(plot_y_right, len(xd_right))
            mean = meanlist[0]
            '''
            meanlist = [base_load] * len(xd_right)
            bx.plot(xd_right, meanlist)
        if len(xd_left) > 0:
            '''
            if (mean > -1.0 and slope_l != 0 and not
                np.isnan(slope_l)): 
                plot_tmax_left = (meanlist[0] - intercept_l) / slope_l
                print ('modified base: {0}F'.format(plot_tmax_left))
                bx.annotate('break-even point: {0}F,\nbase load: {1}'.format(int(round(plot_tmax_left, 0)), round(mean, 1)), xy = (plot_tmax_left, mean), xytext = (plot_tmax_left, mean + 0.2), fontsize=15)
            '''
            xd_left = np.r_[plot_tmin_left:plot_tmax_left:1]
            bx.plot(xd_left, fit(xd_left, slope_l, intercept_l))
    else:
        if len(xd_left) > 0:
            meanlist = ave(plot_y_left, len(xd_left))
            mean = meanlist[0]
            bx.plot(xd_left, ave(plot_y_left, len(xd_left)))
        if len(xd_right) > 0:
            if (mean > -1.0 and slope_r != 0 and not
                np.isnan(slope_r)): 
                plot_tmin_right = (meanlist[0] - intercept_r) / slope_r
                print ('modified base: {0}F'.format(plot_tmin_right))
                bx.annotate('break-even point: {0}F,\nbase load: {1}'.format(int(round(plot_tmin_right, 0)), round(mean, 1)), xy = (plot_tmin_right, mean), xytext = (plot_tmin_right - 13, mean + 0.2), fontsize=15)
            xd_right = np.r_[plot_tmin_right:plot_tmax_right:1]
            bx.plot(xd_right, fit(xd_right, slope_r, intercept_r))
    plt.title('{0} - Temperature Plot'.format(lb.title_dict[theme]))
    plt.suptitle('Building {0}, Station {1}'.format(b, s))
    plt.xlabel('Temperature Deg F')
    plt.ylabel(ylabel_dict[theme])
    P.savefig(os.getcwd() + '/plot_FY_weather/temp_energy/{2}/{0}_{1}.png'.format(b, s, theme), dpi = 150)
    plt.close()

def calculate_dd_energy_regression(kind, theme, bs_pair, timerange):
    print 'calculating dd energy regression'
    bs = []
    ss = []
    slopes = []
    intercepts = []
    rs = []
    bases = []
    ps = []
    yearcol, timefilter = util.get_time_filter(timerange)
    for (i, (b, s)) in enumerate(bs_pair):
        print b, s
        df_all = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, kind, b, s))
        df_all['in_range'] = df_all[yearcol].map(timefilter)
        df_all = df_all[df_all['in_range']]
        if len(df_all) == 0:
            print 'no energy data: {0}, {1}, {2}'.format(b, s,
                                                         timerange)
            continue
        # print df_all[['year', 'month']]
        slope, intercept, r_value, p_value, basetemp = opt_lireg(b, s, df_all, kind, theme, timerange)
        bs.append(b)
        ss.append(s)
        slopes.append(slope)
        intercepts.append(intercept)
        rs.append(r_value)
        bases.append(int(basetemp[:2]))
        ps.append(p_value)
    summary = pd.DataFrame({'Building Number': bs, 'Weather Station':
                            ss, 'Base Temperature': bases, 'k':
                            slopes, 'b': intercepts, 'r': rs,
                            'p_value': ps})
    summary['R squared'] = summary['r'].map(lambda x: x * x)
    summary.to_csv(weatherdir + \
        '{0}_{1}_{2}_regression.csv'.format(kind, theme, timerange),
                   index=False)

# calculate savings for 'year', 'cutoff': r square cutoff
def calculate_savings(theme, kind, cutoff, year, timerange):
    df_reg = pd.read_csv(weatherdir + '{0}_{1}_{2}_regression.csv'.format(kind, theme, timerange))
    df_reg = df_reg[df_reg['R squared'] >= cutoff]
    df_reg_idx = df_reg.set_index('Building Number')
    bs = df_reg['Building Number'].tolist()
    #print df_reg_idx.head()
    filelist = ['{0}dd_temp_eng/{1}_{2}_{3}.csv'.\
                format(weatherdir, kind, b, 
                       df_reg_idx.ix[b, 'Weather Station']) for b in bs]
    for f in filelist:
        df = pd.read_csv(f)
        filename = f[f.rfind('/') + 1:]
        b = filename[4: 12]
        s = filename[13: 17]
        print b, s
        df = df[df['year'] == year]
        slope = df_reg_idx.ix[b, 'k']
        intercept = df_reg_idx.ix[b, 'b']
        t_base = str(df_reg_idx.ix[b, 'Base Temperature']) + 'F'
        df = df[['Building Number', s, 'eui_elec', 'eui_gas',
                 'eui_heat', 'year', 'month', t_base]]
        print (slope, intercept, t_base)
        if kind == 'HDD':
            df[theme + '_hat'] = df.apply(lambda r: slope * r[t_base] + intercept if r[t_base] > 0 else r[theme], axis=1)
        else:
            df[theme + '_hat'] = df.apply(lambda r: slope * r[t_base] + intercept if r[t_base] > 0 else r[theme], axis=1)
        df.to_csv('{0}saving_{1}/{4}/{2}_{3}.csv'.format(weatherdir, year, b, s, theme), index=False)
    return
    
def plot_saving_aggyear(df, years, pre_year, theme, ax, cvrmse):
    df = df[df['year'].isin(years)]
    df2 = df.groupby('month').sum()[[theme, theme + '_hat']]
    df2.rename(columns={theme: theme + '_agg', theme + '_hat': theme + '_agg_hat'}, inplace=True)
    df2.reset_index(inplace=True)
    df_all.sort
    df_all = pd.merge(df, df2, on='month', how='left')
    print df_all
    print df_all.head()
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
    x = df_all['month']
    y = df_all[theme + '_agg']
    y_hat = df_all[theme + '_agg_hat']
    save_percent = round((sum(y_hat) - sum(y)) / sum(y_hat) * 100, 1)
    line1, = ax.plot(x, y, c=c1, ls='-', lw=2, marker='o')
    line2, = ax.plot(x, y_hat, c=c2, ls='-', lw=2, marker='o')
    ax.fill_between(x, y, y_hat, where=y_hat >= y,
                    facecolor='lime', alpha=0.5,
                    interpolate=True)
    ax.fill_between(x, y, y_hat, where=y_hat < y, facecolor='red',
                    alpha=0.5, interpolate=True)
    year = '{0} -- {1}'.format(min(years), max(years))
    ax.legend([line1, line2], 
              ['Actual {1} use in {0}'.format(year, lb.title_dict[theme]), '\n'.join(tw.wrap('{1} use given before {2} habits but {0} weather'.format(year, lb.title_dict[theme], pre_year), wrapwidth))], loc=location)
    if save_percent > 0: 
        ax.set_title('{2} Savings {0} vs before {4}, {1}% less, CVRMSE: {3}'.format(year, save_percent, lb.title_dict[theme], round(cvrmse, 2), pre_year))
    else:
        ax.set_title('{2} Savings {0} vs before {4}, {1}% more, CVRMSE: {3}'.format(year, abs(save_percent), lb.title_dict[theme], round(cvrmse, 2), pre_year))

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

def plot_saving_two(theme, kind, timerange):
    sns.set_style("white")
    sns.set_context("talk", font_scale=1.5)
    filelist_1 = glob.glob('{0}saving_2014/{1}/*.csv'.format(weatherdir, theme))
    filelist_2 = glob.glob('{0}saving_2015/{1}/*.csv'.format(weatherdir, theme))
    df_summary = pd.read_csv(weatherdir +
                             '{0}_{1}_{2}_regression.csv'.format(kind,
                                                                 theme, timerange))
    df_summary.set_index('Building Number', inplace=True)
    if kind == 'HDD':
        c1 = 'brown'
        c2 = 'lightsalmon'
        location = 'upper center'
        wrapwidth = 30
    else:
        c1 = 'navy'
        c2 = 'lightskyblue'
        location = 'lower center'
        wrapwidth = 99

    for (f1, f2) in zip(filelist_1, filelist_2):
        filename_1 = f1[f1.rfind('/') + 1:]
        b_1 = filename_1[:8]
        s_1 = filename_1[9: 13]
        if b_1 not in df_summary.index:
            print 'empty data {0}'.format(b_1)
            continue
        df_1 = pd.read_csv(f1)
        k_1 = df_summary.ix[b_1, 'k']
        if k_1 == 0:
            continue
        r = df_summary.ix[b_1, 'r']
        r2 = round(r * r, 2)
        t_base = df_summary.ix[b_1, 'Base Temperature']
        print (b_1, s_1)
        x_1 = df_1['month']
        y1_1 = df_1[theme]
        y2_1 = df_1[theme + '_hat']
        if y2_1.sum() != 0:
            save_percent_1 = int(round((y2_1.sum() - y1_1.sum()) /
                                       y2_1.sum() * 100, 0))
        else:
            save_percent_1 = 0
        df_2 = pd.read_csv(f2)
        filename_2 = f1[f2.rfind('/') + 1:]
        b_2 = filename_2[:8]
        s_2 = filename_2[9: 13]
        x_2 = df_2['month']
        y1_2 = df_2[theme]
        y2_2 = df_2[theme + '_hat']
        if y2_2.sum() != 0:
            save_percent_2 = int(round((y2_2.sum() - y1_2.sum()) /
                                       y2_2.sum() * 100, 0))
        else:
            save_percent_2 = 0

        fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True,
                                         sharey=True)
        line1_1, = ax_1.plot(x_1, y1_1, c=c1, ls='-', lw=2, marker='o')
        line2_1, = ax_1.plot(x_1, y2_1, c=c2, ls='-', lw=2, marker='o')
        ax_1.fill_between(x_1, y1_1, y2_1, where=y2_1 >= y1_1,
                          facecolor='aquamarine', alpha=0.5,
                          interpolate=True)
        ax_1.fill_between(x_1, y1_1, y2_1, where=y2_1 < y1_1,
                          facecolor='orange', alpha=0.5,
                          interpolate=True)
        ax_1.legend([line1_1, line2_1], 
                    ['Actual {1} use in {0}'.format(2014, lb.title_dict[theme]), '\n'.join(tw.wrap('{1} use given before 2013 habits but {0} weather'.format(2014, lb.title_dict[theme]), wrapwidth))], loc=location)
        line1_2, = ax_2.plot(x_2, y1_2, c=c1, ls='-', lw=2, marker='o')
        line2_2, = ax_2.plot(x_2, y2_2, c=c2, ls='-', lw=2, marker='o')
        ax_2.fill_between(x_2, y1_2, y2_2, where=y2_2 >= y1_2,
                          facecolor='aquamarine', alpha=0.5,
                          interpolate=True)
        ax_2.fill_between(x_2, y1_2, y2_2, where=y2_2 < y1_2,
                          facecolor='orange', alpha=0.5,
                          interpolate=True)
        ax_2.legend([line1_2, line2_2], 
                    ['Actual {1} use in {0}'.format(2015, lb.title_dict[theme]), '\n'.join(tw.wrap('{1} use given before 2013 habits but {0} weather'.format(2015, lb.title_dict[theme]), wrapwidth))], loc=location)
        if save_percent_1 > 0:
            ax_1.set_title('{2} Savings Plot {0} vs before 2013, {1}% less, R^2: {3}'.format(2014, save_percent_1, lb.title_dict[theme], r2))
        else:
            ax_1.set_title('{2} Savings Plot {0} vs before 2013, {1}% more R^2: {3}'.format(2014, abs(save_percent_1), lb.title_dict[theme], r2))
        if save_percent_2 > 0:
            ax_2.set_title('{2} Savings Plot {0} vs before 2013, {1}% less R^2: {3}'.format(2015, save_percent_2, lb.title_dict[theme], r2))
        else:
            ax_2.set_title('{2} Savings Plot {0} vs before 2013, {1}% more R^2: {3}'.format(2015, abs(save_percent_2), lb.title_dict[theme], r2))
        plt.xticks(range(1, 13))
        xticklabels = [calendar.month_abbr[m] for m in range(1, 13)]
        plt.setp(ax_2, xticklabels=xticklabels)
        plt.xlim((1, 12))
        ylimit = max(max(y1_1.max(), y2_2.max()), max(y1_2.max(),
                                                      y2_2.max()))
        ax_1.set_ylim([0, ylimit * 1.1])
        ax_2.set_ylim([0, ylimit * 1.1])
        plt.suptitle('Building {0}, Station {1}, Base {2}F'.format(b_1, s_1, t_base))
        ax_1.set_ylabel('kBtu/sq.ft.')
        ax_2.set_ylabel('kBtu/sq.ft.')
        P.savefig(os.getcwd() + '/plot_FY_weather/saving/{2}/{0}_{1}.png'.format(b_1, s_1, theme), dpi = 150)
        plt.close()

# deprecated
def plot_saving(year, theme, kind):
    sns.set_style("white")
    sns.set_context("talk", font_scale=1.5)
    filelist = glob.glob('{0}saving_{1}/{2}/*.csv'.format(homedir, year, theme))
    df_summary = pd.read_csv(homedir +
                             '{0}_regression_fuel.csv'.format(kind))
    df_summary.set_index('Building Number', inplace=True)
    if kind == 'HDD':
        c1 = 'brown'
        c2 = 'lightsalmon'
    else:
        c1 = 'navy'
        c2 = 'lightskyblue'
    for f in filelist:
        df = pd.read_csv(f)
        filename = f[f.rfind('/') + 1:]
        b = filename[:8]
        s = filename[9: 13]
        r = df_summary.ix[b, 'r']
        r2 = round(r * r, 3)
        print (filename, b, s)
        x = df['month']
        y1 = df[theme]
        y2 = df[theme + '_hat']
        if y2.sum() != 0:
            save_percent = int(round((y2.sum() - y1.sum()) / y2.sum() *
                                    100, 0))
        else:
            save_percent = 0
        bx = plt.axes()
        line1, = bx.plot(x, y1, c=c1, ls='-', lw=2, marker='o')
        line2, = bx.plot(x, y2, c=c2, ls='-', lw=2, marker='o')
        bx.fill_between(x, y1, y2, where=y2 >= y1,
                         facecolor='aquamarine', alpha=0.5,
                         interpolate=True)
        bx.fill_between(x, y1, y2, where=y2 < y1, facecolor='orange',
                         alpha=0.5, interpolate=True)
        bx = plt.axes()
        if kind == 'HDD':
            location = 'upper left'
        else:
            location = 'lower left'
        plt.legend([line1, line2], 
                   ['Actual {1} use in {0}'.format(year, lb.title_dict[theme]), 
                    '{1} use given before 2013 habits but {0} weather'.format(year, lb.title_dict[theme])], 
                   loc=location)
        plt.xticks(range(1, 13))
        xticklabels = [calendar.month_abbr[m] for m in range(1, 13)]
        bx.set(xticklabels=xticklabels)
        plt.xlim((1, 12))
        plt.ylim((0, max(y1.max(), y2.max()) * 1.1))
        if save_percent > 0:
            plt.title('{2} Savings Plot {0} vs before 2013, {1}% less, R^2: {3}'.format(year, save_percent, lb.title_dict[theme], r2))
        else:
            plt.title('{2} Savings Plot {0} vs before 2013, {1}% more, R^2: {3}'.format(year, abs(save_percent), lb.title_dict[theme], r2))
        plt.suptitle('Building {0}, Station {1}'.format(b, s))
        #plt.xlabel('{0} Deg F'.format(kind))
        plt.ylabel(ylabel_dict[theme])
        P.savefig(os.getcwd() + '/plot_FY_weather/saving_{3}/{2}/{0}_{1}.png'.format(b, s, theme, year), dpi = 150)
        plt.close()

# join HDD_regression and CDD regression with 
# indicator_all for fuel type
def join_regression_indi(status):
    df_hdd = pd.read_csv(homedir +
                         'HDD_eui_gas_{0}_regression.csv'.format(status))
    df_cdd = pd.read_csv(homedir +
                         'CDD_eui_elec_{0}_regression.csv'.format(status))
    df_indi = pd.read_csv(os.getcwd() + \
                          '/csv_FY/filter_bit/fis/indicator_all_fuel.csv')
    # 'None (all electric?)' and 'Chilled Water' to be taken out for
    # electricity plot
    # 'Gas Only', the set used for gas plot
    cols = ['Building Number'] + [c for c in list(df_indi) if \
                                  ('None (all electric?)' in c) or \
                                  ('Chilled Water' in c) or \
                                  ('Gas Only' in c)]
    df_indi = df_indi[cols]
    print cols
    df_hdd_fuel = pd.merge(df_hdd, df_indi, on='Building Number',
                           how='inner')
    df_cdd_fuel = pd.merge(df_cdd, df_indi, on='Building Number',
                           how='inner')
    df_hdd_fuel.to_csv(homedir +
                       '{0}_HDD_regression_fuel.csv'.format(status),
                       index=False)
    df_cdd_fuel.to_csv(homedir +
                       '{0}_CDD_regression_fuel.csv'.format(status),
                       index=False)

# FIXME: not always set w, h right
def plot_stat_regression(status):
    sns.set_style("white")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1.0)
    my_dpi = 300

    f, (ax1, ax2) = plt.subplots(1, 2, sharey=True)
    df = pd.read_csv(homedir +
                     '{0}_CDD_regression_fuel.csv'.format(status))
    df = df[df['Chilled Water_10'] == 0]
    df = df[df['Chilled Water_12'] == 0]
    df = df[df['None (all electric?)_10'] == 0]
    df = df[df['None (all electric?)_12'] == 0]
    df2 = pd.read_csv(homedir + 
                     '{0}_HDD_regression_fuel.csv'.format(status))
    df2 = df2[df2['Gas Only_10'] == 1]
    df2 = df2[df2['Gas Only_12'] == 1]
    sns.boxplot(y='R squared', data=df, ax=ax1)
    sns.boxplot(y='R squared', data=df2, ax=ax2)
    ax1.set_ylabel('R square')
    ax1.set_title('Electricity - CDD regression distribution\n' + \
                  'No district chilled water no all electric building \n(n = {0})'.format(len(df)))
    ax2.set_title('Gas - HDD regression distribution\n' + \
                  'Gas Heating Only \n(n = {0})'.format(len(df2)))
    ax2.set_ylabel('')
    P.savefig(os.getcwd() + '/plot_FY_weather/summary/{0}_regression.png'.format(status), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    plt.close()
    print 'end'

def plot_dd_energy_byyear(kind, theme, cutoff):
    sns.set_style("white")
    sns.set_context("talk", font_scale=1.0)
    df_reg = pd.read_csv(homedir + 'pre_{0}_regression.csv'.format(kind))
    df_reg['r2'] = df_reg['r'].map(lambda x: x * x)
    df_reg = df_reg[df_reg['r2'] >= cutoff]
    df_reg_idx = df_reg.set_index('Building Number')
    bs = df_reg['Building Number'].tolist()
    filelist = ['{0}dd_temp_eng/{1}_{2}_{3}.csv'.\
                format(homedir, kind, b, 
                       df_reg_idx.ix[b, 'Weather Station']) \
                for b in bs]
    if kind == 'HDD':
        # colors = ['lightpink', 'deeppink', 'darkmagenta'] + sns.color_palette('Blues', 3)
        colors = sns.color_palette('RdPu', 3) + sns.color_palette('Blues_d', 3)
    else:
        colors = sns.color_palette('Oranges', 3) + sns.color_palette('Greens', 3)
        # colors = ['lightsalmon', 'salmon', 'brown', 'red', 'aquamarine', 'teal', 'seagreen']
    for f in filelist:
        df = pd.read_csv(f)
        if df[theme].sum() == 0:
            continue
        filename = f[f.rfind('/') + 1:]
        b = filename[4: 12]
        s = filename[13: 17]
        print b, s
        t_base = str(df_reg_idx.ix[b, 'Base Temperature']) + 'F'
        df = df[['Building Number', s, 'eui_elec', 'eui_gas', 'year',
                 'month', t_base]]
        df = df[df['year'] > 2009]
        df['year'] = df['year'].map(int)
        df['GSALink rollout'] = df['year'].map(lambda x: 'Before' if x < 2014 else 'After')
        if kind == 'CDD':
            sns.set_palette(sns.color_palette('Blues'))
        elif kind == 'HDD':
            df[t_base] = df[t_base] * (-1.0)
            sns.set_palette(sns.color_palette('Oranges'))
        g = sns.lmplot(x=t_base, y=theme, hue='year', data=df,
                       palette=sns.color_palette(colors),
                       fit_reg=True, size=4, aspect=1)
        plt.title('Building {0}, Station {1}, base {2}'.format(b, s, t_base))
        plt.xlabel('{0} Deg F'.format(kind))
        plt.ylabel(ylabel_dict[theme])
        if kind == 'HDD':
            plt.xlim((df[t_base].min(), 0))
        else:
            plt.xlim((0, df[t_base].max()))
        plt.ylim((0, df[theme].max() * 1.1))
        P.savefig(os.getcwd() + '/plot_FY_weather/{0}_{1}/{2}_{3}.png'.format(kind, theme, b, s), dpi = 300)
        plt.close()

def saving_summary(kind, theme):
    df_dd = pd.read_csv(homedir + '{0}_regression.csv'.format(kind))
    dfs = []
    for year in [2014, 2015]:
        filelist = glob.glob('{0}saving_{1}/{2}/*.csv'.format(weatherdir, year, theme))
        bs = []
        saves = []
        for f in filelist:
            filename = f[f.rfind('/') + 1:]
            b = filename[:8]
            s = filename[9: 13]
            print (b, s)
            df = pd.read_csv(f)
            y = df[theme]
            y_hat = df[theme + '_hat']
            if y_hat.sum() != 0:
                save_percent = (y_hat.sum() - y.sum()) / y_hat.sum() * 100
            else:
                save_percent = 0
            bs.append(b)
            saves.append(save_percent)
        d = {'Building Number': bs, 'Saving_{0}'.format(year): saves}
        df_year = pd.DataFrame(d)
        dfs.append(df_year)
    df_all = reduce(lambda x, y: pd.merge(x, y, how='left', on='Building Number'), [df_dd] + dfs)
    print df_all.head()
    df_all.to_csv(homedir + '{0}_{1}_saving_summary.csv'.format(kind,
                                                                theme),
                  index=False)
    
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

def test_dd2temp():
    dd = 100
    t_base = 50
    kind = 'CDD'
    print '({0}, {1}F, {2}) => {3}'.format(dd, t_base, kind,
                                           dd2temp(dd, t_base, kind))

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
    with open(os.getcwd() + '/plot_FY_weather/html/{0}.html'.format(b),
              'w+') as wt:
        wt.write(''.join(lines))

def process_html_lean(b, s, yearlist, ds):
    with open(os.getcwd() + '/plot_FY_weather/html/template.html', 'r') as rd:
        lines = rd.readlines()
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("building", b)
        lines[i] = lines[i].replace("station", s)
    for i, line in enumerate(lines):
        for year, d in ds:
            lines[i] = lines[i].replace("{0} Base electric load: <br>".format(year[-4:]), "{0} Base electric load: {1}<br>".format(year[-4:], round(d['base_elec'], 2)))
            lines[i] = lines[i].replace("{0} Base gas load: ".format(year[-4:]), "{0} Base gas load: {1}".format(year[-4:], round(d['base_gas'], 2)))
            lines[i] = lines[i].replace("{0} Start cooling at: ".format(year[-4:]), "{0} Start cooling at: {1}".format(year[-4:], d['break_elec']))
            lines[i] = lines[i].replace("{0} Start heating at: ".format(year[-4:]), "{0} Start heating at: {1}".format(year[-4:], d['break_gas']))
            lines[i] = lines[i].replace("{0} ECM action: ".format(year[-4:]), "{0} ECM action: {1}".format(year[-4:], d['ECM action']))
    with open(os.getcwd() + '/plot_FY_weather/html/{0}.html'.format(b),
              'w+') as wt:
        wt.write(''.join(lines))

# FIXME: ECM action
def plot_lean_saving_oneatime():
    print 'plot savings and lean plot ...'
    cutoff = -0.1 # no cutoff limitation for R square
    df_bs3 = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    df_bs3 = df_bs3[df_bs3['Valid Weather Data'] == 1]
    bs_pair = zip(df_bs3['Building Number'], df_bs3['ICAO'])
    study_set = get_gsalink_set()
    bs_pair = [x for x in bs_pair if x[0] in study_set]
    # join_building_temp(bs_pair)
    # for (b, s) in bs_pair:
    #     join_dd_temp_energy(b, s, 'CDD')
    #     join_dd_temp_energy(b, s, 'HDD')
    years = [str(x) for x in range(2010, 2016)]
    yearlist = ['CY{0}'.format(x) for x in years]
    df_ecm = pd.read_csv(homedir + \
                         'master_table/gsa_ecm_year_action.csv')
    df_ecm.rename(columns=dict(zip(years, yearlist)), inplace=True)
    df_ecm['CY2015'] = np.nan
    print list(df_ecm)
    df_ecm.set_index('Building_Number', inplace=True)
    for (b, s) in bs_pair:
        pairs = []
        for year in yearlist:
            d = plot_lean_saving_one(b, s, year)
            if d == None:
                continue
            d['base_elec'] = max(0, d['base_elec'])
            d['base_gas'] = max(0, d['base_gas'])
            d['ECM action'] = df_ecm.ix[b, year]
            print d['ECM action']
            with open (os.getcwd() + \
                       '/plot_FY_weather/html/{0}/{1}.json'.format(year, b),
                       'w+') as wt:
                json.dump(d, wt)
            pairs.append((year, d))
        process_html_lean(b, s, yearlist, pairs)

def plot_action_alone():
    df = pd.read_csv(homedir + \
                     'master_table/ECM/EUAS_ecm_solo_buildingaction.csv')
    buildings = df['Building Number'].tolist()
    df['year'] = df['Substantial Completion Date'].map(lambda x: x[:4])
    df['pre_start'] = df['year'].map(lambda x: 'CY{0}'.format(int(x) - 4))
    df['pre_end'] = df['year'].map(lambda x: 'CY{0}'.format(x))
    df['post_start'] = df['year'].map(lambda x: 'CY{0}'.format(int(x)))
    df['post_end'] = df['year'].map(lambda x: 'CY{0}'.format(int(x) + 3))
    df.set_index('Building Number', inplace=True)
    # print len(buildings)
    df_bs3 = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    df_bs3 = df_bs3[df_bs3['Valid Weather Data'] == 1]
    bs_pair = zip(df_bs3['Building Number'], df_bs3['ICAO'])
    bs_pair = [x for x in bs_pair if x[0] in buildings]
    for (b, s) in bs_pair:
        action = '{0}--{1}'.format(df.ix[b, 'high_level_ECM'], 
                                  df.ix[b, 'detail_level_ECM'])
        action = action.replace('--nan', '')
        print action
        pre_start = df.ix[b, 'pre_start']
        pre_end = df.ix[b, 'pre_end']
        post_start = df.ix[b, 'post_start']
        post_end = df.ix[b, 'post_end']
        endyear = max(2016, int(post_end[-4:]))
        post_end = 'CY{0}'.format(endyear)
        pre_timerange = 'before {0} and after {1}'.format(pre_end, pre_start)
        post_timerange = 'before {0} and after {1}'.format(post_end, post_start)
        result_pre = ltm.lean_temperature(b, s, 2, pre_timerange,
                                          action='pre ' + action)
        if result_pre == None:
            continue
        result_post = ltm.lean_temperature(b, s, 2, post_timerange,
                                          action='post ' + action)
        if result_post == None:
            continue
        d_gas_pre, d_elec_pre, dplot = result_pre
        d_gas_post, d_elec_post, dplot = result_post
        base_gas_pre = d_gas_pre['base_gas']
        base_elec_pre = d_elec_pre['base_elec']
        area_gas_pre = d_gas_pre['area_gas']
        area_elec_pre = d_elec_pre['area_elec']
        breakpoint_gas_pre = d_gas_pre['breakpoint']
        breakpoint_elec_pre = d_elec_pre['breakpoint']
        pre_args = [base_gas_pre, base_elec_pre, breakpoint_gas_pre,
                    breakpoint_elec_pre]
        base_gas_post = d_gas_post['base_gas']
        base_elec_post = d_elec_post['base_elec']
        area_gas_post = d_gas_post['area_gas']
        area_elec_post = d_elec_post['area_elec']
        breakpoint_gas_post = d_gas_post['breakpoint']
        breakpoint_elec_post = d_elec_post['breakpoint']
        post_args = [base_gas_post, base_elec_post,
                     breakpoint_gas_post, breakpoint_elec_post]

        df_gas_pre = d_gas_pre['df']
        df_elec_pre = d_elec_pre['df']
        df_pre = pd.merge(df_gas_pre, df_elec_pre, on=['timestamp', 'year', 'month', s], how='inner')
        df_gas_post = d_gas_post['df']
        df_elec_post = d_elec_post['df']
        df_post = pd.merge(df_gas_post, df_elec_post, on=['timestamp', 'year', 'month', s], how='inner')
        df_post['eui_gas_hat'] = d_gas_pre['fun'](np.array(df_post[s].tolist()), *d_gas_pre['regression_par'])
        print  d_gas_pre['regression_par']
        df_post['eui_elec_hat'] = d_elec_pre['fun'](np.array(df_post[s].tolist()), *d_elec_pre['regression_par'])
        print  d_elec_pre['regression_par']
        years = list(set(df_post['year'].tolist()))
        print years
        if len(years) == 0:
            print 'no post retrofit data'
            return
        sns.set_style("whitegrid")
        sns.set_context("talk", font_scale=1)
        years = [x for x in years if x < 2016]
        # fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True,
        #                                     sharey=True)
        # plot_saving_aggyear(df_post, years, pre_end, 'eui_elec', ax_1, d_elec_pre['CV(RMSE)'])
        # plot_saving_aggyear(df_post, years, pre_end, 'eui_gas', ax_2, d_gas_pre['CV(RMSE)'])
        # P.savefig(os.getcwd() + '/plot_FY_weather/html/savings/{0}_{1}_agg.png'.format(b, s), dpi = 300)
        for year in years:
            fig, (ax_1, ax_2) = plt.subplots(2, 1, sharex=True,
                                             sharey=True)
            plot_saving_year(df_post, year, pre_end, 'eui_elec', ax_1, d_elec_pre['CV(RMSE)'])
            plot_saving_year(df_post, year, pre_end, 'eui_gas', ax_2, d_gas_pre['CV(RMSE)'])
            P.savefig(os.getcwd() + '/plot_FY_weather/html/savings/{0}_{1}_{2}.png'.format(b, s, int(year)), dpi = 300)
        process_html_lean_saving(b, s, action, pre_start, pre_end,
                                 post_start, post_end, pre_args,
                                 post_args, years)
        # with open (os.getcwd() + \
        #            '/plot_FY_weather/html/test/{0}_{1}.json'.format(b,
        #                                                             pre_timerange), 'w+') as wt:
        #     json.dump(d_pre, wt)
        # with open (os.getcwd() + \
        #            '/plot_FY_weather/html/test/{0}_{1}.json'.format(b,
        #                                                             post_timerange), 'w+') as wt:
        #     json.dump(d_post, wt)
    
    return

# try plotting it with average monthly temperature
def plot_lean_saving_one_fixbase(b, s, timerange, base_temp):
    base_temp = '46F'
    yearcol, timefilter = util.get_time_filter(timerange)
    df_gas = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'HDD', b, s))
    df_gas['in_range'] = df_gas[yearcol].map(timefilter)
    df_gas = df_gas[df_gas['in_range']]
    df_gas = df_gas[['eui_gas', base_temp, 'year', 'month']]
    if len(df_gas) == 0:
        print 'no energy data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    df_elec = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'CDD', b, s))
    df_elec['in_range'] = df_elec[yearcol].map(timefilter)
    df_elec = df_elec[df_elec['in_range']]
    df_elec = df_elec[['eui_elec', base_temp, 'year', 'month']]
    df_all = pd.merge(df_gas, df_elec, on=['year', 'month'], how='outer', suffixes=['_gas', '_elec'])
    df_all[base_temp + '_gas'] = df_all[base_temp + '_gas'] * (-1)
    df_all['dd_gas'] = df_all.apply(lambda r: r[base_temp + '_gas'] if r[base_temp + '_gas'] < 0 else r[base_temp + '_elec'], axis=1)
    df_all['dd_elec'] = df_all.apply(lambda r: r[base_temp + '_elec'] if r[base_temp + '_elec'] > 0 else r[base_temp + '_elec'], axis=1)
    df_all.to_csv(homedir + 'temp/{0}_{1}.csv'.format(b, s),
                    index=False)
    df_all.info()
    if len(df_elec) == 0:
        print 'no elec data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    bx = plt.axes()
    sns.regplot(x='dd_gas', y='eui_gas', data=df_all, ax=bx, fit_reg=False)
    sns.regplot(x='dd_elec', y='eui_elec', data=df_all, ax=bx, fit_reg=False)
    plt.ylim((0, max(df_all['eui_gas'].max(), df_all['eui_elec'].max())))
    plt.show()

def plot_lean_saving_one_dep(b, s, timerange):
    yearcol, timefilter = util.get_time_filter(timerange)
    df_gas = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'HDD', b, s))
    df_gas['in_range'] = df_gas[yearcol].map(timefilter)
    df_gas = df_gas[df_gas['in_range']]
    if len(df_gas) == 0:
        print 'no energy data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    df_elec = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'CDD', b, s))
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
    lean_one(b, s, slope_gas, slope_elec, intercept_gas,
             intercept_elec, int(basetemp_gas[:-1]),
             int(basetemp_elec[:-1]), r2_gas, r2_elec, timerange,
             'eui_gas', 'eui_elec', 'combined')
    return d

def plot_lean_saving_one(b, s, timerange):
    yearcol, timefilter = util.get_time_filter(timerange)
    df_gas = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'HDD', b, s))
    df_gas['in_range'] = df_gas[yearcol].map(timefilter)
    df_gas = df_gas[df_gas['in_range']]
    if len(df_gas) == 0:
        print 'no energy data: {0}, {1}, {2}'.format(b, s, timerange)
        return
    df_elec = pd.read_csv('{0}dd_temp_eng/{1}_{2}_{3}.csv'.format(weatherdir, 'CDD', b, s))
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
    lean_one(b, s, slope_gas, slope_elec, intercept_gas,
             intercept_elec, int(basetemp_gas[:-1]),
             int(basetemp_elec[:-1]), r2_gas, r2_elec, timerange,
             'eui_gas', 'eui_elec', 'combined')
    return d

def lean_one(b, s, k_hdd, k_cdd, base_gas, base_elec, t_base_hdd,
             t_base_cdd, r2_hdd, r2_cdd, timerange, theme_h, theme_c,
             side):
    sns.mpl.rc("figure", figsize=(5,5))
    y_upperlim = 10
    x_leftlim = 22
    x_rightlim = 90
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
    df_hdd = df_hdd[['year', 'month', theme_h, s, hdd_base_header]]
    df_cdd = df_cdd[['year', 'month', theme_c, cdd_base_header]]
    df_hdd['in_range'] = df_hdd[yearcol].map(timefilter)
    df_hdd = df_hdd[df_hdd['in_range']]
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
        bx.annotate('HEATING', xy = (xmin + 1, base_elec + base_gas + \
                                     0.2), fontsize=8,
                    color=heating_note_color,
                    weight='semibold',
                    family=font_family)
        plt.plot(sorted_x1, sorted_y1_hat, '-', color=gas_line_color)
        bx.fill_between(sorted_x1, base_elec + base_gas, sorted_y1_hat, facecolor=gas_line_color, alpha=alpha)
    if side == 'cooling' or side == 'combined':
        plt.plot(x2, y2, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
        bx.annotate('COOLING', xy = (xmax - 9, (base_elec + base_gas)
                                     + 0.2), fontsize=8,
                    color=cooling_note_color,
                    weight='semibold',
                    family=font_family)
        plt.plot(sorted_x2, sorted_y2_hat, '-', color=elec_line_color)
        bx.fill_between(sorted_x2, base_elec + base_gas, sorted_y2_hat, facecolor=elec_line_color, alpha=alpha)
        bx.annotate('BASE ELECTRIC', xy = ((xmin + xmax)/2 - 8,
                                           base_elec/2), fontsize=8,
                    color=base_elec_text_color,
                    weight='semibold',
                    family=font_family)
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
    plt.yticks(range(11), range(11))
    plt.ylabel('Monthly [kBtu/sq.ft.]')
    plt.ylim((0, 10))
    # plt.ylim((0, y_upperlim))
    plt.xlim((20, 90))
    # P.savefig(os.getcwd() + '/plot_FY_weather/html/{2}/{0}_{1}_{2}.png'.format(b, s, timerange, side), dpi = 300)
    P.savefig(os.getcwd() + '/plot_FY_weather/html/test/{0}_{1}_{2}.png'.format(b, s, timerange, side), dpi = 300)
    plt.close()
    return (xmin, xmax, y1.max(), y2.max())

# joinkey is the field so that the returned lat lng could be joint to
# the existing static info database
def get_lat_long(joinkey, address):
    g = geocoder.google(address)
    if not (g.json['ok']):
        print '{0},{1},{2}'.format(joinkey, 'Not Found', 'Not Found')
    else:
        latlng = g.latlng
        print '{0},{1},{2}'.format(joinkey, latlng[0], latlng[1])

def geocode_gsa():
    df = pd.read_csv(os.getcwd() + '/input/FY/GSAlink 81 Buildings Updated 9_22_15.csv')
    df['Zip'] = df['Zip Code'].map(lambda x: x[:5])
    df['geocoding_input'] = df.apply(lambda r: '{0} {1}'.format(r['State'], r['Zip']), axis=1)
    df.to_csv(weatherdir + 'GSA_geoinput.csv', index=False)
    addresses = list(set(df['geocoding_input'].tolist()))
    length = len(addresses)
    for i in range(length):
        address = addresses[i]
        get_lat_long(address, address)

def geocode_stat():
    df = pd.read_csv(fldir + 'Building List.csv')
    # df['geocoding_input'] = df.apply(lambda r: '{0} {1}'.format(r['Address'], r['City, State, Zip']), axis=1)
    df['geocoding_input'] = df['City, State, Zip'].map(lambda x: x[x.find(',') + 2:])
    df.to_csv(fldir + 'Building List_geoinput.csv', index=False)
    addresses = list(set(df['geocoding_input'].tolist()))
    length = len(addresses)
    # for i in range(length):
    #     address = addresses[i]
    #     get_lat_long(address, address)

def get_station(dirname, infile, outfile):
    names=['geocoding_input', 'Latitude', 'Longitude']
    df = pd.read_csv(dirname + infile, header=None,
                     names=names)
    result_dict = dict(zip(df['geocoding_input'].tolist(),
                           [-1.0] * len(df)))
    df2 = df.set_index('geocoding_input')
    print df2.head()
    df_lookup = pd.read_csv(weatherdir + \
                            'weatherinput/Weather Station Mapping.csv')
    for key in result_dict:
        lat = df2.ix[key]['Latitude']
        lng = df2.ix[key]['Longitude']
        df_lookup['distance'] = df_lookup.apply(lambda r: vincenty((lat, lng), (r['Lat'], r['Long']), miles=True), axis=1)
        min_distance = df_lookup['distance'].min()
        df_temp = df_lookup[df_lookup['distance'] == min_distance]
        result_dict[key] = (df_temp['ICAO'].tolist()[0], 
                            df_temp['distance'].tolist()[0])
        print result_dict[key]
    # icaos = {k: v[0] for (k, v) in icaos.iteritems()}
    # dists = {k: v[1] for (k, v) in icaos.iteritems()}
    df['ICAO'] = df['geocoding_input'].map(lambda k: result_dict[k][0])
    df['distance [mile]'] = df['geocoding_input'].map(lambda k: result_dict[k][1])
    df.to_csv(dirname + outfile, index=False)

def join_geocode():
    df_geo = pd.read_csv(fldir + 'address_station.csv')
    df = pd.read_csv(fldir + 'Building List_geoinput.csv')
    df_all = pd.merge(df, df_geo, how='left', on='geocoding_input')
    df_all.to_csv(fldir + 'Building List_latlng.csv', index=False)

def join_geocode_gsa():
    df_geo = pd.read_csv(weatherdir + 'gsa_latlng.csv')
    df = pd.read_csv(weatherdir + 'GSA_geoinput.csv')
    df_all = pd.merge(df, df_geo, how='left', on='geocoding_input')
    df_all.to_csv(weatherdir + 'gsa_station.csv', index=False)

def read_energy_fl_clean():
    print 'reading energy fl ...'
    filelist = glob.glob(fldir + 'excel_energy/cleaned/*.xlsx')
    fs = []
    st = []
    bd = []
    total = []
    unique = []
    for f in filelist[:1]:
        filename = f[f.rfind('/') + 1:]
        print 'read', filename
        df = pd.read_excel(f, sheetname='Validation')
        typed_cols = [x for x in list(df) if type(x) !=
                      datetime.datetime]
        print typed_cols
        sum_cols = [x for x in typed_cols if 'Meter ' in x]
        df = df[['Time Stamp'] + sum_cols]
        if 'Meter Total' in list(df):
            df.drop('Meter Total', axis=1, inplace=True)
        sum_cols = [x for x in list(df) if 'Meter ' in x]
        df['Meter Total'] = df[sum_cols].sum(axis=1)
        df = df[['Time Stamp', 'Meter Total']]
        df.set_index(pd.DatetimeIndex(df['Time Stamp']), inplace=True)
        df.index.name = 'Time Stamp'
        df_r = df.resample('H', how='sum')
        # df_r.reset_index(inplace=True)
        outfile = filename[:filename.rfind('.')]
        df_r.to_csv(fldir + 'csv_energy_hour/{0}.csv'.format(outfile))
    return

def read_energy_fl():
    print 'reading energy fl ...'
    # df_lookup = pd.read_csv(fldir + 'file_sheet_buildingname.csv')
    # d = dict(zip(zip(df_lookup['filename'], df_lookup['sheetname']),
    #              df_lookup['building name']))
    filelist = glob.glob(fldir + 'excel_energy/cleaned/*.xlsx')
    fs = []
    st = []
    bd = []
    total = []
    unique = []
    for f in filelist[:1]:
        filename = f[f.rfind('/') + 1:]
        print 'read', filename
        excel = pd.ExcelFile(f)
        sheets = excel.sheet_names
        for i in range(len(sheets)):
            df = excel.parse(i)
            if meter_col not in df:
                print 'bad input: {0} sheet: {1}'.format(filename,
                                                         sheets[i])
                continue
            df = df[[meter_col, 'Start Date', 'End Date']]
            df.drop_duplicates(inplace=True)
            building = d[(filename, sheets[i])]
            df['building name'] = building
            gr = df.groupby(['Start Date', 'End Date'])
            dfs = []
            for name, group in gr:
                if len(group) > 1:
                    print 'conflicting result: File "{3}", sheet "{0}", {1}, {2}'.format(sheets[i], building, name[0], filename)
                    continue
                starttime = group['Start Date'].tolist()[0]
                endtime = group['End Date'].tolist()[0]
                # does not include endpoint
                days = (endtime - starttime).days
                index = pd.date_range(starttime, periods=days,
                                      freq='D')
                daily_energy = df[meter_col].tolist()[0] / days
                df_resample = pd.DataFrame({'Daily Usage (kWh)': [daily_energy] * days, 'Timestamp': index})
                df_resample = df_resample[['Timestamp', 
                                           'Daily Usage (kWh)']]
                dfs.append(df_resample)
            df_all = pd.concat(dfs)
            # df['month'] = df['Start Date'].map(lambda x: x.month)
            # df['year'] = df['Start Date'].map(lambda x: x.year)
            # df.sort(['year', 'month'], inplace=True)
            # total_reading = len(df)
            # unique_reading = len(df.groupby(['year', 'month']).mean())
            # fs.append(filename)
            # st.append(sheets[i])
            # total.append(total_reading)
            # unique.append(unique_reading)
            # bd.append(building)

            df_all.to_csv(fldir + \
                          'csv_energy_day/{0}.csv'.format(building),
                          index=False)

    # df_summary = pd.DataFrame({'File': fs, 'Sheet': st, 'Building': bd,
    #                            'Total Number of Records': total, 
    #                            'Number of Unique Records': unique})
    # df_summary['Exist duplicates'] = df_summary.apply(lambda r: r['Total Number of Records'] != r['Number of Unique Records'], axis=1)
    # df_summary = df_summary[['File', 'Sheet', 'Building', 'Total Number of Records', 'Number of Unique Records', 'Exist duplicates']]
    # df_summary.to_csv(fldir + 'summary_energy.csv', index=False)
    return

def plot_lean_fl():
    # geocode_stat()
    # get_station(fldir, 'geocoding_log.txt', 'address_station.csv')
    # join_geocode()
    read_energy_fl_clean()
    return

def lean(bs_pair, theme_h, theme_c, cutoff, side, timerange):
    df_pre_hdd = pd.read_csv(weatherdir + 'HDD_{0}_{1}_regression.csv'.format(theme_h, timerange))
    bs_pair = zip(df_pre_hdd['Building Number'].tolist(), df_pre_hdd['Weather Station'].tolist())
    print 'reading', (weatherdir + 'HDD_{0}_{1}_regression.csv'.format(theme_h, timerange))
    # print df_pre_hdd['Building Number']
    df_pre_cdd = pd.read_csv(weatherdir + 'CDD_{0}_{1}_regression.csv'.format(theme_c, timerange))
    df_upperlim = pd.read_csv(weatherdir + 'upper_limit_summary.csv')
    df_pre_hdd.set_index('Building Number', inplace=True)
    df_pre_cdd.set_index('Building Number', inplace=True)
    df_upperlim.set_index('Building Number', inplace=True)
    bs_pre = []
    ss_pre = []
    y1max_pre = []
    y2max_pre = []
    xmins_pre = []
    xmaxs_pre = []
    y_upperlim = 5
    for (b, s) in bs_pair:
        print (b, s)
        k_hdd = df_pre_hdd.ix[b, 'k']
        k_cdd = df_pre_cdd.ix[b, 'k']
        base_gas = df_pre_hdd.ix[b, 'b']
        base_elec = df_pre_cdd.ix[b, 'b']
        t_base_hdd = df_pre_hdd.ix[b, 'Base Temperature']
        t_base_cdd = df_pre_cdd.ix[b, 'Base Temperature']
        r2_hdd = df_pre_hdd.ix[b, 'R squared']
        r2_cdd = df_pre_cdd.ix[b, 'R squared']
        # if (k_hdd == 0.0 or k_cdd == 0.0): 
        #     print 'Zero energy consumption error'
        #     continue
        if (r2_hdd < cutoff or r2_cdd < cutoff):
            print 'low R squared: r2_hdd = {0}, r2_cdd = {1}'.format(r2_hdd, r2_cdd)
            continue
        # y_upperlim = df_upperlim.ix[b, 'y_upperlim']
        xmin, xmax, y1max, y2max = lean_one(b, s, k_hdd, k_cdd,
                                            base_gas, base_elec,
                                            t_base_hdd, t_base_cdd,
                                            r2_hdd, r2_cdd, timerange,
                                            theme_h, theme_c, side)
        bs_pre.append(b)
        ss_pre.append(s)
        y1max_pre.append(y1max)
        y2max_pre.append(y2max)
        xmins_pre.append(xmin)
        xmaxs_pre.append(xmax)
    # run it once to generate plotting upper bound
    # df1 = pd.DataFrame({'Building Number': bs_pre, 'Weather Station':
    #                     ss_pre, 'plot xmin': xmins_pre, 'plot xmax':
    #                     xmaxs_pre, 'HDD plot upper limit': y1max_pre,
    #                     'CDD plot upper limit': y2max_pre}) 
    # df1.to_csv(homedir + \
    #            'upper_limit_summary_{0}.csv'.format(timerange),
    #            index=False)
    return

def process_missing_weather():
    # join_geocode_gsa()
    # missing_stations = ['KBFM', 'KAVL', 'KBJC', 'KSAN', 'KFTW', 'KLIT', 'KSYR', 'KDSM', 'KCGX']
    # rd.read_weather_data(missing_stations)
    return

# plot energy vs degree day derived temperature for multiple buildings
# kind is HDD or CDD
def plot_energy_dd_multi(timerange_list, kind, theme, side):
    print 'plotting energy degree day LEAN for multiple building'
    df_bs = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    # df_bs = df_bs[df_bs['good_area_15'] == 1]
    df_bs = df_bs[df_bs['Valid Weather Data'] == 1]
    bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
    study_set = get_gsalink_set()
    bs_pair = [x for x in bs_pair if x[0] in study_set]
    for timerange in timerange_list:
        print 'plotting multi-building energy degree day LEAN for {0}'.format(timerange)
        plot_energy_dd_multi_oneyear(bs_pair, timerange, kind, theme,
                                     side)

def plot_energy_dd_multi_oneyear(bs_pair, timerange, kind, theme,
                                 side):
    # calculate_dd_energy_regression(kind, theme, bs_pair, timerange)
    df_summary = pd.read_csv(weatherdir + '{2}_{0}_{1}_regression.csv'.format(theme, timerange, kind))
    # to remove the very horizontal lines
    if theme == 'heating':
        df_summary = df_summary[df_summary['p_value'] < 0.1]
    df_summary = df_summary[df_summary['k'] > 0.0001]
    bs_pair = [x for x in bs_pair if x[0] in df_summary['Building Number'].tolist()]
    df_summary.set_index('Building Number', inplace=True)
    sns.set_style("white")
    sns.set_context("talk", font_scale=2)
    if side == 'heating':
        sns.set_palette(sns.cubehelix_palette(50))
        base_elec = 0
    elif side == 'cooling':
        sns.set_palette(sns.cubehelix_palette(50, rot=-.4))
        base_gas = 0
    elif side == 'base':
        sns.set_palette(sns.light_palette("yellow"))
    labels = []
    lines = []
    for b, s in bs_pair:
        df_dd = pd.read_csv(weatherdir +
                            'dd_temp_eng/{2}_{0}_{1}.csv'.format(b, s,
                                                                 kind))
        yearcol, timefilter = util.get_time_filter(timerange)
        df_dd['in_range'] = df_dd[yearcol].map(timefilter)
        df_dd = df_dd[df_dd['in_range']]
        t_base = df_summary.ix[b, 'Base Temperature']
        k = df_summary.ix[b, 'k']
        intercept = df_summary.ix[b, 'b']
        dd_base_header = '{0}F'.format(t_base)
        df_dd = df_dd[['year', 'month', theme, s, dd_base_header]]
        df_dd['cvt_temp_dd'] = df_dd[dd_base_header].map(lambda x: dd2temp(x, t_base, kind))
        df_plot = df_dd.copy()
        if side == 'heating':
            df_plot[theme + '_hat'] = df_plot[dd_base_header].map(lambda x: k * x + intercept)
            df_plot[theme + '_filter'] = \
                df_plot.apply(lambda r: r[theme] if r['cvt_temp_dd'] <
                            t_base else np.nan, axis=1)
            df_plot[theme + '_hat_filter'] = \
                df_plot.apply(lambda r: r[theme + '_hat'] if
                            r['cvt_temp_dd'] < t_base else np.nan,
                            axis=1)
        elif side == 'cooling':
            df_plot[theme + '_hat'] = df_plot[dd_base_header].map(lambda x: k * x)
            df_plot[theme + '_filter'] = \
                df_plot.apply(lambda r: r[theme] if r['cvt_temp_dd'] >=
                            t_base else np.nan, axis=1)
            df_plot[theme + '_hat_filter'] = \
                df_plot.apply(lambda r: r[theme + '_hat'] if
                            r['cvt_temp_dd'] >= t_base else np.nan,
                            axis=1)
        elif side == 'base':
            df_plot[theme + '_hat'] = df_plot[dd_base_header].map(lambda x: intercept)
        x1 = df_plot['cvt_temp_dd']
        y1 = df_plot[theme + '_filter']
        y1_hat = df_plot[theme + '_hat_filter']
        sorted_x1y1hat = sorted(zip(x1, y1_hat), key=lambda x: x[0])
        sorted_x1 = [p[0] for p in sorted_x1y1hat]
        sorted_y1_hat = [p[1] for p in sorted_x1y1hat]
        xmin = x1.min()
        xmax = x1.max()
        if side == 'heating':
            plt.plot(x1, np.array(y1), 'o')
        elif side == 'cooling':
            plt.plot(x1, [max(0, y-intercept) for y in y1], 'o')
        line, = plt.plot(sorted_x1, sorted_y1_hat, '-')
        plt.fill_between(sorted_x1, sorted_y1_hat,
                         alpha=0.3)
        # plt.fill_between([xmin, xmax], 0, [intercept] * 2, alpha=0.5)
        label = 'Building {0}, y = {1}x + {2}'.format(b, round(k, 5), round(intercept, 5))
        lines.append(line)
        labels.append(label)
    plt.title('{0} lean plot: {1} vs {2}, {3}'.format(side.capitalize(), lb.title_dict[theme], kind, timerange))
    plt.xlabel('Temperature Represented Degree Day [F]')
    plt.ylabel('Monthly kBtu/sq.ft.')
    if side == 'heating':
        plt.ylim((0, 12))
    elif side == 'cooling':
        plt.ylim((0, 4))
    plt.legend(lines, labels, loc='center left', 
               bbox_to_anchor=(1, 0.5), prop={'size':6})
    # plt.xlim((10, 70))
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_weather/lean_multi/{0}_{1}_{2}.png'.format(timerange, theme, kind), dpi = 150, bbox_inches='tight')
    plt.close()
    return

def float2rgb(x):
    return "rgb" + str(tuple([int(x[i] * 255) for i in range(3)]))

def plot_trend_action_onetype_dy(theme, plot_set, action, summary_step, pal, agg):
    pal_str = str([float2rgb(x) for x in pal] + ["gray"])
    pal_str = pal_str.replace("'", "\"")
    print 'plotting ECM action trend'
    df_energy = pd.read_csv(homedir + 'master_table/energy_eui_monthly.csv')
    if plot_set == 'GSALink':
        study_set = get_gsalink_set()
        df_energy = df_energy[df_energy['Building Number'].isin(study_set)]
    num = len(set(df_energy['Building Number']))
    df_energy = df_energy[['Building Number', 'year', 'month', theme]]
    df_energy = df_energy[df_energy['year'] < 2016]
    df_ecm = pd.read_csv(homedir + 'master_table/ECM/EUAS_ecm_detail_long.csv')
    df_ecm = df_ecm[df_ecm['high_level_ECM'] == action]
    detail_actions = list(set(df_ecm['ECM_combined_header'].tolist())) + ['No Action']
    # colors = sns.color_palette(pal, len(detail_actions))
    # print color_dict
    df = pd.merge(df_energy, df_ecm, on='Building Number', how='left')
    df.fillna({'ECM_combined_header': 'No Action'}, inplace=True)
    if summary_step == 'month':
        df['Date'] = df.apply(lambda r: '{0}{1}01'.format(int(r['year']), str(int(r['month'])).zfill(2)) if not np.isnan(r['year']) else np.nan, axis=1)
    else:
        df['Date'] = df.apply(lambda r: '{0}0101'.format(int(r['year'])) if not np.isnan(r['year']) else np.nan, axis=1)
    gr = df.groupby('ECM_combined_header')
    labels = []
    dfs = []
    i = 0
    names = []
    for name, group in gr:
        size = len(set(group['Building Number'].tolist()))
        idx_sep = name.find('_')
        if agg == 'median':
            group = group.groupby('Date').median()
        elif agg == 'mean':
            group = group.groupby('Date').mean()
        group.reset_index(inplace=True)
        group.set_index('Date', inplace=True)
        temp = group.copy()
        temp.drop(['year', 'month'], axis=1, inplace=True)
        temp.rename(columns={theme: name[name.find('_')+1:]}, inplace=True)
        dfs.append(temp)
        labels.append('{0} (n={1})'.format(name, size))
        names.append(name)
        i += 1
    df_all = reduce(lambda x, y: pd.merge(x, y, left_index=True, right_index=True), dfs)
    outfile = "ECM_{0}_{1}_{2}_{3}_{4}.csv".format(plot_set, action, theme, summary_step, agg)
    print outfile
    outhtml = outfile.replace(".csv", ".html")
    names = [x[x.find('_') + 1:] for x in names]
    df_all.rename(columns=dict(zip(names, labels)), inplace=True)
    df_all.to_csv(os.getcwd() + '/plot_dynamic/' + outfile)
    title = '{2} building {3} {4} {0} trend plot (n = {1})'.format(lb.title_dict[theme], num, plot_set, action, agg)
    with open(os.getcwd() + '/plot_dynamic/template.html', 'r') as rd:
        lines = rd.readlines()
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("target_csv", outfile)
        lines[i] = lines[i].replace("colors: []", "colors: {0}".format(pal_str))
        lines[i] = lines[i].replace("title: \"\"", "title: \"{0}\"".format(title))
    with open(os.getcwd() + '/plot_dynamic/{0}'.format(outhtml), 'w+') as wt:
        wt.write(''.join(lines))
    return

# pal: palette
def plot_trend_action_onetype(theme, plot_set, action, summary_step, pal, agg):
    # pal = "RdPu"
    print 'plotting ECM action trend'
    df_energy = pd.read_csv(homedir + 'master_table/energy_eui_monthly.csv')
    if plot_set == 'GSALink':
        study_set = get_gsalink_set()
        df_energy = df_energy[df_energy['Building Number'].isin(study_set)]
    num = len(set(df_energy['Building Number']))
    df_energy = df_energy[['Building Number', 'year', 'month', theme]]
    df_ecm = pd.read_csv(homedir + 'master_table/ECM/EUAS_ecm_detail_long.csv')
    df_ecm = df_ecm[df_ecm['high_level_ECM'] == action]
    detail_actions = list(set(df_ecm['ECM_combined_header'].tolist())) + ['No Action']
    colors = sns.color_palette(pal, len(detail_actions))
    # print color_dict
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    sns.set_palette(sns.color_palette(pal, len(set(detail_actions))))
    df = pd.merge(df_energy, df_ecm, on='Building Number', how='left')
    df.fillna({'ECM_combined_header': 'No Action'}, inplace=True)
    if summary_step == 'month':
        df['Date'] = df.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1) if not np.isnan(r['year']) else np.nan, axis=1)
    else:
        df['Date'] = df.apply(lambda r: datetime.datetime(int(r['year']), 1, 1) if not np.isnan(r['year']) else np.nan, axis=1)
    gr = df.groupby('ECM_combined_header')
    lines = []
    labels = []
    i = 0
    bx = plt.axes()
    for name, group in gr:
        size = len(set(group['Building Number'].tolist()))
        idx_sep = name.find('_')
        # group = group.groupby('Date').median()
        if agg == 'median':
            group = group.groupby('Date').median()
        elif agg == 'mean':
            group = group.groupby('Date').mean()
        group.reset_index(inplace=True)
        group.set_index('Date', inplace=True)
        if name == 'No Action':
            line, = plt.plot(group.index, group[theme], color='gray')
        else:
            line, = plt.plot(group.index, group[theme])
        lines.append(line)
        labels.append('{0} (n={1})'.format(name, size))
        i += 1
    plt.title('{2} building {3} {4} {0} trend plot (n = '
              '{1})'.format(lb.title_dict[theme], num, plot_set, action,
                            agg))
    plt.xlabel('Calendar Year')
    plt.ylabel(ylabel_dict[theme])
    # if plot_set == 'GSALink':
    x1 = [datetime.datetime(2011, 1, 1), datetime.datetime(2012,
                                                            1, 1)]
    x2 = [datetime.datetime(2013, 9, 1), datetime.datetime(2014,
                                                            9, 1)]
    x3 = [datetime.datetime(2009, 1, 1), datetime.datetime(2014,
                                                            12, 31)]
    if theme == 'eui_elec' or theme == 'eui_gas':
        ylimit = 8
    else:
        ylimit = bx.get_ylim()[1]
    y = [ylimit] * 2
    area1 = bx.fill_between(x1, 0, y, facecolor='blue', alpha=0.3)

    area2 = bx.fill_between(x2, 0, y, facecolor='red', alpha=0.3)
    area3 = bx.fill_between(x3, 0, y, facecolor='gray', alpha=0.3)
    p0 = bx.fill(np.NaN, np.NaN, 'b', alpha=0.3)
    p1 = bx.fill(np.NaN, np.NaN, 'r', alpha=0.3)
    p2 = bx.fill(np.NaN, np.NaN, 'gray', alpha=0.3)
    plt.ylim((0, ylimit))
    plt.legend(list(reversed(lines)) + [p0[0], p1[0], p2[0]],
               list(reversed(labels)) + [action + 'dominant period', 'GSALink rollout period', 'All types of ECM action period'], loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    if summary_step == 'month':
        plt.xlim((datetime.datetime(2002, 10, 1),
                  datetime.datetime(2016, 1, 1)))
    elif summary_step == 'year':
        plt.xlim((datetime.datetime(2002, 10, 1),
                  datetime.datetime(2015, 1, 1)))
    P.savefig(os.getcwd() + '/plot_FY_annual/ECM_{0}_{1}_{2}_{3}_{4}.png'.format(plot_set, action, theme, summary_step, agg), dpi = 150, bbox_inches='tight')
    # plt.show()
    plt.close()
    return

def plot_trend_action(theme, plot_set, action_level):
    pal = "Set1"
    print 'plotting ECM action trend'
    df_energy = pd.read_csv(homedir + 'master_table/energy_eui_monthly.csv')
    if plot_set == 'gsa':
        study_set = get_gsalink_set()
        df_energy = df_energy[df_energy['Building Number'].isin(study_set)]
    num = len(set(df_energy['Building Number']))
    df_energy = df_energy[['Building Number', 'year', 'month', theme]]
    df_ecm = pd.read_csv(homedir + 'master_table/ECM/EUAS_ecm_detail_long.csv')
    high_level_actions = list(set(df_ecm['high_level_ECM'])) + ['No Action']
    colors = sns.color_palette(pal, len(high_level_actions))
    color_dict = dict(zip(high_level_actions, colors))
    sns.set_style("whitegrid")
    sns.set_palette(sns.color_palette(pal, len(high_level_actions)))
    df = pd.merge(df_energy, df_ecm, on='Building Number', how='left')
    df.fillna({action_level: 'No Action'}, inplace=True)
    df['Date'] = df.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1) if not np.isnan(r['year']) else np.nan, axis=1)
    gr = df.groupby(action_level)
    lines = []
    labels = []
    for name, group in gr:
        print name
        idx_sep = name.find('_')
        if idx_sep == -1:
            color = color_dict[name]
        else:
            color = color_dict[name[:idx_sep]]
        if agg == 'median':
            group = group.groupby('Date').median()
        elif agg == 'mean':
            group = group.groupby('Date').mean()
        group.reset_index(inplace=True)
        group.set_index('Date', inplace=True)
        line, = plt.plot(group.index, group[theme], color=color)
        lines.append(line)
        labels.append(name)
    plt.legend(list(reversed(lines)), list(reversed(labels)),
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.title('All Building {0} Trend Plot (n = '
        '{1})'.format(lb.title_dict[theme], num))
    plt.xlabel('Calendar Year')
    plt.ylabel(ylabel_dict[theme])
    plt.xlim((datetime.datetime(2002, 10, 1), datetime.datetime(2016,
                                                                4, 1)))
    P.savefig(os.getcwd() + '/plot_FY_annual/{0}_{1}_{2}_.png'.format(plot_set, action_level, theme), dpi = 150, bbox_inches='tight')
    plt.close()
    return

def plot_gsa_trend_fan(theme, summary_step):
    sns.set_style("whitegrid")
    # colors = sns.color_palette("Blues", 5)
    # colors2 = colors + list(reversed(colors)) 
    blues = sns.color_palette("Blues", 5)
    colors2= [blues[0], blues[1], "red", blues[1], blues[0]]
    sns.set_palette(sns.color_palette(colors2))
    sns.set_context("talk", font_scale=1)
    study_set = get_gsalink_set()
    df_energy = pd.read_csv(homedir + 'master_table/energy_eui_monthly.csv')
    df_energy = df_energy[df_energy['Building Number'].isin(study_set)]
    num = len(set(df_energy['Building Number']))
    df_energy = df_energy[['Building Number', 'year', 'month', theme]]
    if summary_step == 'month':
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1), axis=1)
    else:
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), 1, 1), axis=1)
    # df_energy.set_index(pd.DatetimeIndex(df_energy['Date']), inplace=True)
    # df_energy.drop(['Building Number', 'year', 'month'], axis=1, inplace=True)
    # quantiles = np.arange(0.1, 1.1, 0.1)
    quantiles = np.arange(0, 1.25, 0.25)
    print quantiles
    lines = []
    labels = ['{0}%'.format(x) for x in range(0, 125, 25)]
    bx = plt.axes()
    ys = []
    for q in quantiles:
        # df = df_energy.resample('M', how=lambda x: np.percentile(x[theme], q=q))
        df = df_energy.groupby(['Date']).quantile(q)
        df.reset_index(inplace=True)
        df.set_index('Date', inplace=True)
        ys.append(df[theme])
        if q == 0.5:
            line, = plt.plot(df.index, df[theme], lw=2)
            df_median = df
        else:
            line, = plt.plot(df.index, df[theme], lw=1)
        lines.append(line)
        idx = df.index
    plt.fill_between(idx, ys[0], ys[1], facecolor=blues[0], alpha=0.3)
    plt.fill_between(idx, ys[1], ys[2], facecolor=blues[1], alpha=0.5)
    plt.fill_between(idx, ys[2], ys[3], facecolor=blues[1], alpha=0.5)
    plt.fill_between(idx, ys[3], ys[4], facecolor=blues[0], alpha=0.3)
    df_median.reset_index(inplace=True)
    # df_median.set_index('Date', inplace=True)
    # df_median.info()
    plt.legend(list(reversed(lines)), list(reversed(labels)),
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})
    plt.title('GSALink Building {0} Trend Fan Plot (n = '
        '{1})'.format(lb.title_dict[theme], num))
    plt.xlabel('Calendar Year')
    plt.ylabel(ylabel_dict[theme])
    plt.xlim((datetime.datetime(2002, 10, 1),
                datetime.datetime(2016, 4, 1)))
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/gsa_trend_fan_{0}_{1}.png'.format(theme, summary_step), dpi = 150)
    plt.close()

def plot_gsa_trend():
    study_set = get_gsalink_set()
    df_energy = pd.read_csv(homedir + 'master_table/energy_eui_monthly.csv')
    df_energy = df_energy[df_energy['Building Number'].isin(study_set)]
    df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1), axis=1)
    # df_energy.dropna(subset=['Date'], inplace=True)
    df_energy.set_index('Date', inplace=True)
    gr = df_energy.groupby('Building Number')
    sns.set_style("whitegrid")
    sns.set_palette("Set3")
    sns.set_context("talk", font_scale=1)
    num = len(gr)
    for theme in ['eui', 'eui_elec', 'eui_gas', 'eui_water']:
        lines = []
        names = []
        for name, group in gr:
            line, = plt.plot(group.index, group[theme])
            lines.append(line)
            names.append(name)
        plt.title('GSALink Building {0}'
                  ' Trend (n = {1})'.format(lb.title_dict[theme], num))
        plt.xlabel('Calendar Year')
        plt.ylabel(ylabel_dict[theme])
        # plt.legend(lines, names, loc='center left',
        #            bbox_to_anchor=(1, 0.5), prop={'size':6})
        plt.xlim((datetime.datetime(2002, 10, 1),
                  datetime.datetime(2016, 4, 1)))
        # plt.tight_layout()
        # plt.show()
        P.savefig(os.getcwd() + '/plot_FY_annual/gsa_trend_{0}.png'.format(theme), dpi = 150)
        df_out = df_energy.groupby('Building Number').sum()
        df_out = df_out[['eui', 'eui_elec', 'eui_gas', 'eui_water']]
        df_out.rename(columns=ylabel_dict, inplace=True)
        df_out.to_csv(os.getcwd() + '/plot_FY_annual/gsa_trend_data.csv')
        plt.close()

# FIXME: slight difference between from last round of regression calculation and saving percent
def plot_lean_savings():
    print 'plot savings and lean plot ...'
    cutoff = -0.1 # no cutoff limitation for R square
    df_bs3 = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    df_bs3 = df_bs3[df_bs3['Valid Weather Data'] == 1]
    bs_pair = zip(df_bs3['Building Number'], df_bs3['ICAO'])
    study_set = get_gsalink_set()
    bs_pair = [x for x in bs_pair if x[0] in study_set]
    # bs_pair = [('TX0211ZZ', 'KHOU'), ('NM0050ZZ', 'KABQ'),
    #            ('OH0046ZZ', 'KCMH')]
    # join_building_temp(bs_pair)
    # for (b, s) in bs_pair:
    #     join_dd_temp_energy(b, s, 'CDD')
    #     join_dd_temp_energy(b, s, 'HDD')
    yearlist = ['CY{0}'.format(x) for x in range(2010, 2016)]
    for year in yearlist[5:]:
        calculate_dd_energy_regression('CDD', 'eui_elec', bs_pair, 
                                       year)
        calculate_dd_energy_regression('HDD', 'eui_gas', bs_pair, 
                                       year)
        lean(bs_pair, 'eui_gas', 'eui_elec', cutoff, 'combined', year)
    # calculate_dd_energy_regression('CDD', 'eui_elec', bs_pair, 
    #                                'before CY2016 and after CY2013')
    # calculate_dd_energy_regression('HDD', 'eui_gas', bs_pair, 
    #                                'before CY2016 and after CY2013')
    # for year in [2014, 2015]:
    #     calculate_savings('eui_elec', 'CDD', cutoff, year, 
    #                       'before CY2013 and after CY2009')
    #     calculate_savings('eui_gas', 'HDD', cutoff, year, 
    #                       'before CY2013 and after CY2009')
    # plot_saving_two('eui_elec', 'CDD', 'before CY2013 and after CY2009')
    # plot_saving_two('eui_gas', 'HDD', 'before CY2013 and after CY2009')
    # plot_saving_two('eui_heat', 'HDD', 'before CY2013 and after CY2009')
    cutoff = 0.0
    # bs_pair = bs_pair[:1]
    # lean(bs_pair, 'eui_gas', 'eui_elec', cutoff, 'combined', 'before CY2013 and after CY2009')
    # lean(bs_pair, 'eui_gas', 'eui_elec', cutoff, 'combined', 'before CY2016 and after CY2013')

def process_gsalink():
    # import read_fy_calendar as rd 
    # geocode_gsa()
    # get_station(weatherdir, 'gsa_geocoding_log.txt', 'gsa_latlng.csv')
    # for FY2015
    # df_bs3 = pd.read_csv(homedir + 'master_table/indicator_wECM_weather.csv')
    # df_bs3 = df_bs3[df_bs3['good_area_15'] == 1]
    # df_bs3 = df_bs3[df_bs3['Valid Weather Data'] == 1]
    # bs_pair = zip(df_bs3['Building Number'], df_bs3['ICAO'])
    # study_set = get_gsalink_set()
    # bs_pair = [x for x in bs_pair if x[0] in study_set]
    # join_building_temp(bs_pair)
    # for (b, s) in bs_pair:
        # join_dd_temp_energy(b, s, 'CDD')
        # join_dd_temp_energy(b, s, 'HDD')
    # plot_energy_dd_multi(bs_pair, 'FY2015')
    # cutoff = -0.1 # no cutoff limitation for R square
    # plot_lean_savings()
    # join_regression_indi('pre')
    # join_regression_indi('post')
    # plot_stat_regression('pre')
    # plot_stat_regression('post')
    # plot_dd_energy_byyear('CDD', 'eui_elec', cutoff)
    # plot_dd_energy_byyear('HDD', 'eui_gas', cutoff)

    # BOOKMARK FIXME
    # df_bs_cool = pd.read_csv(weatherdir + 'CDD_{0}_{1}_regression.csv'.format('eui_elec', status))
    # df_bs_heat = pd.read_csv(weatherdir + 'HDD_{0}_{1}_regression.csv'.format('eui_gas', status))
    # # to remove the very horizontal lines
    # df_bs_heat = df_bs_heat[df_bs_heat['p_value'] < 0.1]
    # df_bs_heat = df_bs_heat[df_bs_heat['k'] > 0.0001]
    # # BOOKMARK: preprocess so that cooling and heating load are in a
    # # table
    # bs_pair_cool = zip(df_bs_cool['Building Number'],
    #                    df_bs_cool['Weather Station'])
    # bs_pair_heat = zip(df_bs_heat['Building Number'],
    #                    df_bs_heat['Weather Station'])
    # bs_pair = list(set(bs_pair_cool).intersection(set(bs_pair_heat)))
    # lean(bs_pair_cool, 'eui_gas', 'eui_elec', -0.1, 'cooling', 'FY2015')
    # lean(bs_pair_heat, 'eui_gas', 'eui_elec', -0.1, 'heating', 'FY2015')
    # lean(bs_pair_cool, 'eui_gas', 'eui_elec', -0.1, 'cooling', 'pre 2013')
    lean(bs_pair, 'eui_gas', 'eui_elec', cutoff, 'combined', 'pre 2013')
    # lean(bs_pair, 'eui_gas', 'eui_elec', cutoff, 'combined', 'post 2013')
    # plot_energy_dd_multi(bs_pair_heat, 'FY2015', 'HDD', 'eui_gas', 'heating')
    # plot_energy_dd_multi(bs_pair_cool, 'FY2015', 'CDD', 'eui_elec', 'cooling')
    return

def piecewise_lean():
    df_bs = pd.read_csv(weatherdir + 'building_station_lookup.csv')
    bs_pair = zip(df_bs['Building Number'], df_bs['ICAO'])
    study_set = get_gsalink_set()
    bs_pair = [x for x in bs_pair if x[0] in study_set]
    for (b, s) in bs_pair[:1]:
        print (b, s)
        d_gas, d_elec, d_plot = ltm.lean_temperature(b, s, 2)
    return

def plot_dynamic():
    pal = sns.color_palette('Oranges', 5) + sns.color_palette('Blues', 5)
    # plot_trend_action_onetype_dy('eui', 'GSALink', 'HVAC', 'year', pal, 'mean')
    for theme in ['eui', 'eui_elec', 'eui_gas']:
        for plot_set in ['GSALink', 'All']:
            plot_trend_action_onetype_dy(theme, plot_set, 'HVAC',
                                         'month', pal, 'mean')
            plot_trend_action_onetype_dy(theme, plot_set, 'HVAC',
                                         'month', pal, 'median')
    pal = sns.color_palette('Oranges', 3) + sns.color_palette('Blues', 3)
    for theme in ['eui', 'eui_elec', 'eui_gas']:
        for plot_set in ['GSALink', 'all']:
            plot_trend_action_onetype_dy(theme, plot_set, 
                                         'Building Envelope', 'month', 
                                         pal, 'mean')
            plot_trend_action_onetype_dy(theme, plot_set, 
                                         'Building Envelope', 'month', 
                                         pal, 'median')
    return

def main():
    # plot_action_alone()
    # plot_lean_saving_oneatime()
    # plot_lean_fl()
    # missing_stations = ['KBFM', 'KAVL', 'KBJC', 'KSAN', 'KFTW', 'KLIT', 'KSYR', 'KDSM', 'KCGX']
    # missing_stations = []
    # process_weatherfile('new', missing_stations)
    # process_gsalink()
    # for theme in ['eui', 'eui_elec', 'eui_gas', 'eui_water']:
    #     for step in ['month', 'year']:
    #         plot_gsa_trend_fan(theme, step)

    # plot_dynamic()
    # pal = sns.color_palette('Oranges', 5) + sns.color_palette('Blues', 5)
    # plot_trend_action_onetype('eui', 'GSALink', 'HVAC', 'year', pal, 'mean')
    # plot_trend_action_onetype('eui', 'All', 'HVAC', 'year', pal)
    # plot_trend_action_onetype_dy('eui', 'GSALink', 'HVAC', 'year', pal, 'mean')
    # for theme in ['eui', 'eui_elec', 'eui_gas']:
    #     for plot_set in ['GSALink', 'All']:
    #         for step in ['month', 'year']:
    #             plot_trend_action_onetype(theme, plot_set, 'HVAC',
    #                                       step, pal, 'mean')
    #             plot_trend_action_onetype(theme, plot_set, 'HVAC',
    #                                       step, pal, 'median')
    # pal = sns.color_palette('Oranges', 3) + sns.color_palette('Blues', 3)
    # for theme in ['eui', 'eui_elec', 'eui_gas']:
    #     for plot_set in ['GSALink', 'All']:
    #         for step in ['month', 'year']:
    #             plot_trend_action_onetype(theme, plot_set, 
    #                                       'Building Envelope', step, 
    #                                       pal, 'mean')
    #             plot_trend_action_onetype(theme, plot_set, 
    #                                       'Building Envelope', step, 
    #                                       pal, 'median')

    # plot_trend_action('eui', 'gsa', 'high_level_ECM')
    # plot_trend_action('eui', 'All', 'ECM_combined_header')
    # plot_trend_action('eui', 'All', 'high_level_ECM')
    # plot_lean_savings()
    # plot_lean_fl()
    # plotyears = ['FY{0}'.format(y) for y in range(2010, 2016)]
    # plot_energy_dd_multi(plotyears, 'HDD', 'eui_gas', 'heating')
    # plot_energy_dd_multi(plotyears, 'CDD', 'eui_elec', 'cooling')
    #calculate('eui_gas', 'kernel')
    #calculate('eui_elec', 'kernel')
    #plot_building_temp()
    # piecewise_lean()
    return

main()
