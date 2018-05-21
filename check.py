from __future__ import division
import sqlite3
import pandas as pd
import numpy as np
import os
import glob
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
from datetime import datetime

import util
import util_io as uo
import get_building_set as gbs
homedir = os.getcwd() + '/csv_FY/'
inputdir = os.getcwd() + '/input/FY/interval/0624/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
my_dpi = 70

def missing_gsalink():
    conn1 = uo.connect('all')
    with conn1:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly', conn1)
    conn2 = uo.connect('other_input')
    with conn2:
        df2 = pd.read_sql('SELECT DISTINCT Building_ID AS Building_Number FROM GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates', conn2)
    # print len(df2)
    missing = set(df2['Building_Number'].tolist()).difference(df1['Building_Number'].tolist())
    print len(missing)
    print
    print missing

def compare_interval_db(b):
    conn = uo.connect('ION data.db')
    with conn:
        df = read_sql('SELECT * FROM ION_electricity WHERE Building_Number = \'{0}\''.format(b))
    df.info()

def check_covered_component():
    ids = gbs.get_covered_set()
    f_ids = [x for x in ids if '0000' in x]
    b_ids = [x for x in ids if not '0000' in x]
    euas = gbs.get_all_building_set()
    print len(euas)
    print 'total {0}, facility {1}, building {2}'.format(len(ids), len(f_ids), len(b_ids))
    for f in f_ids:
        dfs = []
        df = uo.view_building(f, 'Electric_(kBtu)')
        bs = [x for x in euas if '{0}0000{1}'.format(x[:2], x[-2:]) == f and not '0000' in x]
        if len(bs) == 0:
            print 'no building under {0}'.format(f)
            continue
        dfs.append(df)
        for b in bs:
            df = uo.view_building(b, 'Electric_(kBtu)')
            dfs.append(df)
        df_all = pd.concat(dfs, ignore_index=True)
        df_all.sort(['year', 'month', 'Building_Number'], inplace=True)
        print 'write to {0}.csv'.format(f)
        df_all.to_csv(homedir + 'question/facility_building/{0}.csv'.format(f), index=False)

def facility_vs_building_set(s):
    print s
    conn = uo.connect('all')
    if s == 'AI':
        ids = gbs.get_cat_set(['A', 'I'], conn)
    elif s == 'covered':
        ids = gbs.get_covered_set()
        df_f = pd.read_csv(os.getcwd() + '/input/FY/covered/Covered_Facilities_All Energy mmBTUs_FY14_EISA07Sec432_input.csv')
        facility_eisa = set(df_f['Facility_Number'].tolist())
        facility_eisa = [x[:8] for x in facility_eisa if type(x) != float]
    f_ids = [x for x in ids if '0000' in x]
    # for x in sorted(f_ids):
    #     print x
    b_ids = [x for x in ids if not '0000' in x]
    print 'total {0}, facility {1}, building {2}'.format(len(ids), len(f_ids), len(b_ids))
    bf_ids = ['{0}0000{1}'.format(x[:2], x[-2:]) for x in b_ids]
    print len(common)
    common = (set(bf_ids).intersection(f_ids))
    for y in common:
        print y
        print [x for x in b_ids if '{0}0000{1}'.format(x[:2], x[-2:]) == y]
    if s == 'covered':
        print 'eisa facility', len(facility_eisa)
        print 'common ids from eisa', len(set(f_ids).intersection(facility_eisa))
        print 'different ids from eisa', (set(f_ids).difference(facility_eisa))
        common = (set(f_ids).difference(facility_eisa))
        for y in common:
            print y
            print [x for x in b_ids if '{0}0000{1}'.format(x[:2], x[-2:]) == y]

def eisa_building_in_euas():
    df = pd.read_csv(os.getcwd() + '/input/FY/covered/Covered_Facilities_All Energy mmBTUs_FY14_EISA07Sec432_input.csv')
    buildings = df['Building_Number'].unique()
    facility = df['Facility_Number'].unique()
    print 'total building {0}, total facility {1}'.format(len(buildings), len(facility))
    euas = gbs.get_all_building_set()
    print len(euas.intersection(buildings)), len(euas.intersection(facility))
    print euas.intersection(buildings)

def facility_vs_building():
    # facility_vs_building_set('AI')
    facility_vs_building_set('covered')
    return

def check_interval(filename):
    df = pd.read_csv(inputdir + filename)
    df.rename(columns=lambda x: x[:8] if x != 'Timestamp' else x,
              inplace=True)
    df.dropna(axis=1, how='all', inplace=True)
    df['Timestamp'] = pd.to_datetime(df['Timestamp'])
    df.set_index(pd.DatetimeIndex(df['Timestamp']), inplace=True)
    # df.info()
    df_re = df.resample('M', how='sum')
    cols = list(df_re)
    df_re.reset_index(inplace=True)
    df_long = pd.melt(df_re, id_vars='index', value_vars=cols)
    # print
    # print df_long.head()
    df_long.rename(columns={'index':'Timestamp', 'variable': 'Building_Number', 'value': 'Electricity_(KWH)'}, inplace=True)
    df_long['month'] = df_long['Timestamp'].map(lambda x: x.month)
    df_long['year'] = df_long['Timestamp'].map(lambda x: x.year)
    col_str = ','.join(['\'{0}\''.format(x) for x in cols])
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT Building_Number, year, month, [Electricity_(KWH)] FROM EUAS_monthly WHERE Building_Number IN ({0}) AND year = \'2015\''.format(col_str), conn)
    # print df.head()
    df_long.drop('Timestamp', axis=1, inplace=True)
    df_all = pd.merge(df, df_long, how='left', on=['Building_Number', 'year', 'month'], suffixes=['_EUAS', '_ION'])
    df_all['ratio'] = df_all['Electricity_(KWH)_ION']/df_all['Electricity_(KWH)_EUAS'].map(lambda x: round(x, 3))
    df_all['percent_diff'] = df_all['ratio'].map(lambda x: abs(1 - x) * 100.0)
    # print df_all.head()
    return df_all
    # print df_all[['percent_diff']].describe()
    
def plot_diff():
    df = pd.read_csv(inputdir + 'cmp/summary.csv')
    gr = df.groupby('Building_Number')
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.color_palette('Set2'))
    for name, group in list(gr):
        line1, = plt.plot(group['month'], group['Electricity_(KWH)_EUAS'], '-o')
        line2, = plt.plot(group['month'], group['Electricity_(KWH)_ION'], '-o')
        total_ion = group['Electricity_(KWH)_ION'].sum(axis=1)
        total_euas = group['Electricity_(KWH)_EUAS'].sum(axis=1)
        # if total_euas != 0:
        #     percent_diff = 1 - (total_ion/total_euas)
        # else:
        #     percent_diff = None
        ratio = total_ion / total_euas
        plt.title('Building {0} Total ION/Total EUAS {1}'.format(name, ratio))
        plt.legend([line1, line2], 
            ['EUAS monthly', 'ION monthly aggregated'], loc = 2, 
            bbox_to_anchor=(1, 1))
        plt.gca().set_ylim(bottom=0)
        P.savefig(inputdir + 'cmp/plot/{0}'.format(name), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()
    # plt.show()

def output_diff():
    df1 = check_interval('BKS_AA_CMU_MA.csv')
    df2 = check_interval('BKS_AA_CMU_region_1_exceptMA.csv')
    df = pd.concat([df1, df2], ignore_index=True)
    df.to_csv(inputdir + 'cmp/summary.csv', index=False)
    print df[['percent_diff']].describe()
    return
    
def check_hourly(b, measure_type, status):
    conn = uo.connect('interval_ion')
    euas_dict = {'electric': 'Electricity_(KWH)', 'gas': 'Gas_(Cubic_Ft)'}
    ion_dict = {'electric': 'Electric_(KWH)', 'gas':'Gas_(CubicFeet)'}
    with conn:
        if status == 'raw':
            df1 = pd.read_sql('SELECT * FROM {1} WHERE Building_Number = \'{0}\''.format(b, measure_type), conn)
        else:
            df1 = pd.read_sql('SELECT * FROM {1}_outlier_tag WHERE Building_Number = \'{0}\' AND outlier == \'0\''.format(b, measure_type), conn)
    conn.close()
    if len(df1) == 0:
        print 'building {b} not in db ...'
    df1['Date'] = pd.DatetimeIndex(pd.to_datetime(df1['Timestamp']))
    df1.set_index(df1['Date'], inplace=True)
    df1_re = df1.resample('M', 'sum')
    df1_re['month'] = df1_re.index.month
    df1_re['year'] = df1_re.index.year
    df1_re.reset_index(inplace=True)
    conn = uo.connect('all')
    with conn:
        df2 = pd.read_sql('SELECT Building_Number, year, month, [{1}] FROM EUAS_monthly WHERE Building_Number = \'{0}\' AND year != 2016.0'.format(b, euas_dict[measure_type]), conn)
    if len(df1) == 0 or len(df2) == 0:
        return
    df_all = pd.merge(df1_re, df2, on=['year', 'month'], how='left')
    df_all.set_index(pd.DatetimeIndex(pd.to_datetime(df_all['Date'])), inplace=True)
    df_all.drop('Date', axis=1, inplace=True)
    df_all.rename(columns={ion_dict[measure_type]: 'ION', euas_dict[measure_type]: 'EUAS'}, inplace=True)

    df_inn = pd.merge(df1_re, df2, on=['year', 'month'], how='inner')
    df_inn.set_index(pd.DatetimeIndex(pd.to_datetime(df_inn['Date'])), inplace=True)
    df_inn.drop('Date', axis=1, inplace=True)
    df_inn.rename(columns={ion_dict[measure_type]: 'ION', euas_dict[measure_type]: 'EUAS'}, inplace=True)
    df_inn[b] = df_inn['ION']/df_inn['EUAS']
    # df_inn.to_csv(homedir + 'temp/{0}_{1}_ion_euas.csv'.format(b, measure_type)) # temp check the data
    dsc = df_inn[[b]].describe().transpose()
    dsc['overall'] = df_inn['ION'].sum()/df_inn['EUAS'].sum()
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.color_palette('Set2'))
    line1, = plt.plot(df_inn.index, df_inn['ION'], '-o')
    line2, = plt.plot(df_inn.index, df_inn['EUAS'], '-o')
    plt.legend([line1, line2], ['ION', 'EUAS'], loc='center left', bbox_to_anchor=(1, 0.5))
    plt.title('{0} {1} ION vs EUAS monthly'.format(b, measure_type), fontsize=30)
    if measure_type == 'electric':
        plt.ylabel('KWH')
    else:
        plt.ylabel('Cubic Feet')
    # plt.show()
    # plt.xlim((datetime(2013, 9, 1), datetime(2016, 1, 1)))
    path = os.getcwd() + '/input/FY/interval/ion_0627/cmp_euas/{0}_{1}.png'.format(b, measure_type)
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()
    return dsc

def check_match(conn, measure_type):
    with conn:
        df = pd.read_sql('SELECT * FROM {0}_id'.format(measure_type),
                         conn)
    ids = df['id']
    print len(ids)
    dfs = []
    for i, b in enumerate(ids):
        print i, b
        dsc = check_hourly(b, measure_type, "clean")
        # dsc = check_hourly(b, measure_type, "raw")
        dfs.append(dsc)
    df_all = pd.concat(dfs)
    df_all.sort('75%', inplace=True)
    path = os.getcwd() + '/input/FY/interval/ion_0627/cmp_euas/{0}_ratio.csv'.format(measure_type)
    df_all.to_csv(path)
    return

def check_0711():
    conn = uo.connect('interval_ion')
    # check_match(conn, 'electric')
    check_match(conn, 'gas')
    conn.close()
    # keys = ['mean', 'std', 'min', '25%', '50%', '75%', 'max', 'overall']
    # format_dict = {k: lambda x: '{0:.2f}'.format(x) for k in keys}
    # uo.csv2html(os.getcwd() + '/input/FY/interval/ion_0627/cmp_euas/electric_ratio.csv', {'Unnamed: 0': 'Building_Number'}, format_dict)
    # uo.csv2html(os.getcwd() + '/input/FY/interval/ion_0627/cmp_euas/gas_ratio.csv', {'Unnamed: 0': 'Building_Number'}, format_dict)
    return

# demonstrate the '0000' rule to identify facility number does not apply
def show_covered_exception(b, **kwargs):
    euas = gbs.get_all_building_set()
    print b in euas
    bs = [x for x in euas if '{0}0000{1}'.format(x[:2], x[-2:]) == b]
    if 'bs' in kwargs:
        bs = kwargs['bs']
    dfs = []
    df = uo.view_building(b, 'Electric_(kBtu)')
    dfs.append(df)
    if 'year' in kwargs:
        year = kwargs['year']
        df = df[(df['year'] == year) & (df['month'].isin([1, 2, 3]))]
        print b
        print df[['Region_No.', 'month', 'Electric_(kBtu)']]
    for x in bs[:3]:
        print x
        df = uo.view_building(x, 'Electric_(kBtu)')
        if 'year' in kwargs:
            year = kwargs['year']
            df = df[(df['year'] == year) & (df['month'].isin([1, 2, 3]))]
        dfs.append(df)
        if 'year' in kwargs:
            print
            print df[['Region_No.', 'month', 'Electric_(kBtu)']]
    df_all = pd.concat(dfs, ignore_index=True)
    df_all.drop(['year', 'month'], axis=1, inplace=True)
    df_all.sort(['Fiscal_Year', 'Fiscal_Month', 'Building_Number'],
                inplace=True)
    print 'write to {0}.csv'.format(b)
    df_all.to_csv(homedir + 'question/facility_building/{0}.csv'.format(b), index=False)
    return

def change_area():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT Building_Number, [Gross_Sq.Ft] FROM EUAS_area', conn)
    df_max = df.groupby('Building_Number').max()
    df_min = df.groupby('Building_Number').min()
    df_all = pd.merge(df_max, df_min, how='inner', left_index=True, right_index=True, suffixes=['_max', '_min'])
    df_all['diff'] = df_all['Gross_Sq.Ft_max'] - \
                     df_all['Gross_Sq.Ft_min']
    df_all['percent_diff'] = df_all.apply(lambda r: np.nan if r['Gross_Sq.Ft_max'] == 0 else (1 - r['Gross_Sq.Ft_min']/r['Gross_Sq.Ft_max']) * 100, axis=1)
    df_large = df_all[df_all['percent_diff'] > 10]
    print len(df_large)
    df_large.drop('diff', axis=1, inplace=True)
    print df_large.head()
    df_large.to_csv(homedir + 'question/change_area.csv', index=True)
    return
    
def ecm_program_no_date():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm_program', conn)
    df = df[df['ECM_program'].notnull()]
    df.drop_duplicates(cols=['Building_Number', 'ECM_program'], inplace=True)
    df.rename(columns={'ECM_program': 'energy_program'}, inplace=True)
    df.to_csv(homedir + 'question/program_date.csv', index=False)
    return

def euas_covered():
    covered = gbs.get_covered_set()
    euas = gbs.get_all_building_set()
    good_elec = gbs.get_energy_set('eui_elec')
    good_gas = gbs.get_energy_set('eui_gas')
    print len(covered.intersection(euas))
    print len(covered.intersection(good_elec))
    print len(covered.intersection(good_gas))

def check_0706():
    # euas_covered()
    ion_gsalink_time()
    return

def ion_gsalink_time():
    df = pd.read_csv(os.getcwd() + \
                     '/input/FY/interval/ion_0627/summary_long/summary_electric.csv')
    df = df[['Building_Number', 'min_time']]
    conn = uo.connect('other_input')
    with conn:
        df_gsalink = pd.read_sql('SELECT Building_ID as Building_Number, Rollout_Date as GSALink_start_time FROM  GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates', conn)
    df_all = pd.merge(df, df_gsalink, on='Building_Number', how='left')
    df_all.rename(columns={'min_time': 'ION_start_time'}, inplace=True)
    df_all['GSALink_start_time'] = df_all['GSALink_start_time'].map(lambda x: np.nan if type(x) == float else datetime.strptime(x, '%Y/%m/%d').strftime('%Y-%m-%d'))
    df_all['ION_start_time'] = df_all['ION_start_time'].map(lambda x: np.nan if type(x) == float else datetime.strptime(x, '%Y-%m-%d %H:%M:%S').strftime('%Y-%m-%d'))
    df_all['days_diff'] = (pd.to_datetime(df_all['GSALink_start_time']) - pd.to_datetime(df_all['ION_start_time']))/np.timedelta64(1, 'D')
    df_all.to_csv(os.getcwd() + '/input/FY/interval/ion_0627/ion_gsalink_start.csv')
    print len(df_all[df_all['days_diff'] > 100])
    return
    
def missing_area():
    conn = uo.connect('interval_ion')
    with conn:
        df_area = pd.read_sql('SELECT * FROM area', conn)
        id_elec = pd.read_sql('SELECT id FROM electric_id', conn)
        id_gas = pd.read_sql('SELECT id FROM gas_id', conn)
    ids = np.union1d(id_elec['id'], id_gas['id'])
    missing = np.setdiff1d(ids, df_area['Building_Number'])
    print
    for x in missing:
        print x

def exclude():
    df = pd.read_csv(os.getcwd() + '/input/FY/excluded_buildings.csv')
    df['exclude'] = 'Yes'
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly WHERE Fiscal_Year = \'2015\'', conn)
        df2 = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    df3 = pd.merge(df1, df2, on='Building_Number', how='left')
    df4 = pd.merge(df3, df, on='Building_Number', how='left')
    # df4 = df4[~df4['Cat'].isin(['A', 'I'])]
    print df4.head()
    conn.close()
    df4.to_csv(homedir + 'temp/exclude_2015.csv', index=False)
    print 'end'
    
def compare_id():
    conn = uo.connect('interval_ion')
    with conn:
        id_elec = pd.read_sql('SELECT * FROM electric_id', conn)['id']
        id_gas = pd.read_sql('SELECT * FROM gas_id', conn)['id']
    conn2 = uo.connect('all')
    with conn2:
        id_gsalink = pd.read_sql('SELECT * FROM EUAS_ecm WHERE high_level_ECM = \'GSALink\'', conn2)['Building_Number']
    print len(set(id_gsalink))
    print len(set(id_elec).intersection(set(id_gsalink)))
    print len(set(id_gas).intersection(set(id_gsalink)))
    print len(set(id_elec).intersection(set(id_gas)).intersection(set(id_gsalink)))
    
def gsalink_facility_id():
    conn = uo.connect('other_input')
    with conn:
        df1 = pd.read_sql('SELECT Building_ID AS Building_Number FROM GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates', conn)
        df2 = pd.read_sql('SELECT Building_Number, Facility_Number FROM building_facility', conn)
    # print len(df2)
    print df1.head()
    # print '${0}$'.format(df1.ix[1, 0])
    # print df2.head()
    # print '${0}$'.format(df2.ix[0, 0])
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    print df.head()
    df.dropna(subset=['Facility_Number'], inplace=True)
    print len(df.groupby('Facility_Number').filter(lambda x: len(x) > 1))
    print len(df.groupby('Facility_Number').filter(lambda x: len(x) == 1))
    # print df.head()
    # print len(df)
    # print df['Facility_Number'].value_counts()
    
def count_db_entries(db):
    conn = uo.connect(db)
    c = conn.cursor()
    tables = util.get_list_tables(c)
    acc = 0
    for t in tables:
        c.execute("SELECT COUNT (*) FROM {0};".format(t))
        acc += c.fetchone()[0]
    print acc
    return

def count_invest():
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number, high_level_ECM FROM EUAS_ecm WHERE high_level_ECM != \'GSALink\'', conn)
        df2 = pd.read_sql('SELECT DISTINCT Building_Number, ECM_program FROM EUAS_ecm_program', conn)
    eng_set = gbs.get_energy_set('eui')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    study_set = eng_set.intersection(ai_set)
    df1.dropna(subset=['high_level_ECM'], inplace=True)
    df2.dropna(subset=['ECM_program'], inplace=True)
    df1 = df1[df1['Building_Number'].isin(study_set)]
    df2 = df2[df2['Building_Number'].isin(study_set)]
    print df1['high_level_ECM'].value_counts()
    print df2['ECM_program'].value_counts()
    return
    
def invest_cnt():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, high_level_ECM, detail_level_ECM FROM EUAS_ecm WHERE detail_level_ECM != \'GSALink\'', conn)
    eng_set = gbs.get_energy_set('eui')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    study_set = eng_set.intersection(ai_set)
    df = df[df['Building_Number'].isin(study_set)]
    print df.groupby(['high_level_ECM', 'detail_level_ECM']).count()
    print len(df)
    df = df.groupby(['Building_Number']).filter(lambda x: len(x) == 1)
    print len(df)
    print df.head()
    print df.groupby(['high_level_ECM', 'detail_level_ECM']).count()
    return

def study_set_plot():
    conn = uo.connect('all')
    with conn:
        # df1 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn)
        df1 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM eui_by_fy', conn)
        df2 = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    meter_set = gbs.get_action_set('high_level_ECM', ['Advanced Metering'])
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    df = df[df['Fiscal_Year'] > 2006]
    df = df[df['Fiscal_Year'] < 2016]
    df3 = df.groupby('Building_Number').filter(lambda x: len(x) > 5)
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    # invest = gbs.get_invest_set()[-1]
    invest = gbs.get_ecm_set()
    all_building = set(df3['Building_Number'].tolist())
    print 'all building > 5 years of data: {0}'.format(len(all_building))
    print 'all building > 5 years of data + ecm: {0}'.format(len(all_building.intersection(invest)))
    print 'all building > 5 years of data + meter: {0}'.format(len(all_building.intersection(meter_set)))

    df4 = df[df['Cat'].isin(['A', 'I'])].groupby('Building_Number').filter(lambda x: len(x) > 5)
    ai_building = set(df4['Building_Number'].tolist())
    print 'A + I building > 5 years of data: {0}'.format(len(ai_building))
    print 'A + I building > 5 years of data + ecm: {0}'.format(len(ai_building.intersection(invest)))
    print 'A + I building > 5 years of data + meter: {0}'.format(len(ai_building.intersection(meter_set)))

    print 'elec ',len(gbs.get_energy_set('elec').intersection(ai_set))
    print 'elec + ecm', len(gbs.get_energy_set('elec').intersection(ai_set).intersection(invest))
    print 'elec + meter', len(gbs.get_energy_set('elec').intersection(ai_set).intersection(meter_set))

    print 'gas ',len(gbs.get_energy_set('gas').intersection(ai_set))
    print 'gas + ecm', len(gbs.get_energy_set('gas').intersection(ai_set).intersection(invest))
    print 'gas + meter', len(gbs.get_energy_set('gas').intersection(ai_set).intersection(meter_set))
    print 'eui',len(gbs.get_energy_set('eui').intersection(ai_set))
    print 'eui + ecm', len(gbs.get_energy_set('eui').intersection(ai_set).intersection(invest))
    print 'eui + meter', len(gbs.get_energy_set('eui').intersection(ai_set).intersection(meter_set))
    return
    
def num_invest():
    conn = uo.connect('all')
    studyset = gbs.get_650_set(conn)
    df1 = pd.DataFrame({'Building_Number': list(studyset)})
    with conn:
        df2 = pd.read_sql('SELECT * FROM EUAS_invest_nona', conn)
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    df.groupby('Building_Number').count().to_csv(homedir + 'temp/invest_cnt_all.csv')
    cnt = df.groupby('Building_Number').count()
    print cnt.groupby('investment').count()
    # before = len(df)
    # df_nonull = (df[df.notnull()])
    # end = len(df_nunull)
    # print df_nonnull.groupby('Building_Number').count()
    # print df_nonull

def partition_seq_lin(cuts):
    labels = ['<= {0}'.format(x) for x in cuts]
    length = len(cuts)
    def classify(x):
        if x <= cuts[0]:
            return labels[0]
        elif x > cuts[-1]:
            return '> {0}'.format(cuts[-1])
        else:
            for i in range(length - 1):
                if cuts[i] <= x and x < cuts[i + 1]:
                    return labels[i + 1]
    return classify

def partition(column='eui', cuts=[50, 100, 150, 200, 250], energyfilter=False):
    conn = uo.connect('all')
    labels = ['<= {0}'.format(x) for x in cuts]
    labels.append('> {0}'.format(cuts[-1]))
    if energyfilter:
        filename = 'eui_by_fy_high_eui'
        outname = 'btu_with_energy_filter'
        sup = 'with energy filter'
    else:
        filename = 'eui_by_fy'
        outname = 'btu_no_energy_filter'
        sup = 'without energy filter'
    with conn:
        df1 = pd.read_sql('SELECT * FROM EUAS_area ' + \
            'WHERE [Gross_Sq.Ft] > 10000 and Fiscal_Year in (\'2003\', \'2015\')', conn)
        df2 = pd.read_sql('SELECT Building_Number, Fiscal_Year, ' + \
                          '{0} FROM {1} '.format(column, filename) + \
            'WHERE Fiscal_Year in (\'2003\', \'2015\')', conn)
    # good_area_set = df1['Building_Number'].unique()
    df = pd.merge(df2, df1, on=['Building_Number', 'Fiscal_Year'], how='inner')
    df.info()
    df['BTU'] = df['eui'] * df['Gross_Sq.Ft'] * 1000
    invest_set = gbs.get_invest_set()[-1]
    df3 = df.pivot(index='Building_Number', columns='Fiscal_Year',
                   values='BTU')
    df3.dropna(axis=0, how='any', inplace=True)
    total_2003 = df3[2003.0].sum()
    total_2015 = df3[2015.0].sum()
    # count = len(df3)
    df3['impact_2003'] = df3[2003.0] / total_2003
    df3['impact_2015'] = df3[2015.0] / total_2015
    df3['has_invest'] = df3.index.map(lambda x: 1 if x in invest_set else 0)
    df3['no_invest'] = 1 - df3['has_invest']
    df3['saving'] = df3[2003.0] - df3[2015.0]
    df3['saving_invest'] = df3['saving'] * df3['has_invest']
    df3['saving_no_invest'] = df3['saving'] * df3['no_invest']
    df3['total_btu_2003_invest'] = df3[2003] * df3['has_invest']
    df3['total_btu_2015_invest'] = df3[2015] * df3['has_invest']
    df3['total_btu_2003_no_invest'] = df3[2003] * df3['no_invest']
    df3['total_btu_2015_no_invest'] = df3[2015] * df3['no_invest']
    df_temp = df2[['Building_Number', 'Fiscal_Year', 'eui']]
    df_eui = df.pivot(index='Building_Number', columns='Fiscal_Year',
                       values='eui')
    df_eui.rename(columns={2003: 'eui_2003', 2015: 'eui_2015'}, inplace=True)
    # df_eui.set_index('Building_Number', inplace=True)
    df3 = pd.merge(df3, df_eui, left_index=True, right_index=True, how='left')
    df3['class'] = df3['eui_2003'].map(partition_seq_lin(cuts))
    df3['count'] = 1
    sns.set_context("talk", font_scale=1.0)
    for year in ['2003', '2015']:
        col = 'eui_{0}'.format(year)
        sns.distplot(df3[df3[col] < 500][col])
        n = len(df3[df3[col] >= 500])
        plt.title('{0} EUI distribution'.format(year))
        plt.suptitle('{0} ({1} outliers (eui >= 500 kbtu))'.format(sup, n))
        # plt.show()
        plt.xlim((-50, 450))
        plt.gca().set_ylim(top=0.022)
        P.savefig(homedir + 'temp/{0}_{1}.png'.format(outname, year), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
        plt.close()
    df3.to_csv(homedir + 'temp/xxx.csv')
    df4 = df3.groupby('class').sum()
    df4['percent_has_invest'] = df4['has_invest'] / df4['count']
    # df4['impact_2003'] = df4['impact_2003'].map(lambda x: '{0: %}'.format(x))
    # df4['impact_2015'] = df4['impact_2015'].map(lambda x: '{0: %}'.format(x))
    df4 = df4.reindex(index = labels)
    df4.rename(columns={2003: '2003_total_btu'}, inplace=True)
    df4.rename(columns={2015: '2015_total_btu'}, inplace=True)
    df4.to_csv(homedir + 'temp/{0}.csv'.format(outname))
    print 'end'
    return

def main():
    partition(energyfilter=False)
    partition(energyfilter=True)
    # num_invest()
    # study_set_plot()
    # invest_cnt()
    # count_invest()
    # count_db_entries('all')
    # count_db_entries('other_input')
    # gsalink_facility_id()
    # compare_id()
    # exclude()
    # missing_gsalink()
    # missing_area()
    # check_0711()
    # check_0706()
    # b = 'FL0067ZZ'
    # compare_interval_db(b)
    # ecm_program_no_date()
    # change_area()
    # plot_diff()
    # facility_vs_building()
    # eisa_building_in_euas()
    # show_covered_exception('MD0000WO', 2015)
    # show_covered_exception('MD0000AG', 2015)
    # show_covered_exception('PA0000ER', bs=['PA0064ZZ', 'PA0644ZZ', 'PA0600ZZ'])
    # show_covered_exception('MD0000AG', bs=['MD0778AG', 'MD0767AG'])
    # set(['MD0778AG', 'MD0767AG', 'PA0064ZZ', 'PA0644ZZ', 'PA0600ZZ'])
    # check_covered_component()
    return
    
main()
# conn = uo.connect('all')
# with conn:
#     df = pd.read_sql('SELECT * FROM EUAS_ecm WHERE high_level_ECM in (\'Advanced Metering\', \'GSALink\')', conn)
# conn.close()
# conn2 = uo.connect('interval_ion')
# with conn2:
#     df2 = pd.read_sql('SELECT * FROM electric_id', conn2)
# set_meter = set(df['Building_Number'].tolist())
# set_inter = set(df2['id'].tolist())
# print len(set_meter), len(set_inter)
# print len(set_meter.intersection(set_inter))

