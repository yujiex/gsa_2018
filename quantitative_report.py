# How to interpret scatter plot
import os
import glob
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import pylab as P
import textwrap as tw
from scipy import stats
import time
import datetime
import get_building_set as gbs
import sqlite3
import calendar
import shutil

import util_io as uo
import label as lb
import util

homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
r_input = os.getcwd() + '/input_R/'
my_dpi = 70

plot_set_label = {'AI': 'A + I', 'All': 'All', 'ACI': 'A + C + I'}

def eui_distribution():
    conn = uo.connect('all')
    df = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas FROM eui_by_fy', conn)
    # print df.groupby('Fiscal_Year').count()
    df['Fiscal_Year'] = df['Fiscal_Year'].map(int)
    df2 = df[df['eui_elec'] > 200000]
    print df2
    print len(df2)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.color_palette('Set3'))
    sns.factorplot(x='Fiscal_Year', y='eui_elec', data=df[df['eui_elec'] < 100000],
                   kind='violin', size=6, aspect=2)
    print my_dpi
    # P.savefig(os.getcwd() + '/plot_FY_annual/quant/cat_count.png', dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.show()
    plt.close()

def area_distribution():
    conn = uo.connect('all')
    df = pd.read_sql('SELECT * FROM EUAS_area', conn)
    # print df.groupby('Fiscal_Year').count()
    df['Fiscal_Year'] = df['Fiscal_Year'].map(int)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.color_palette('Set3'))
    sns.barplot(x='Fiscal_Year', y='Gross_Sq.Ft', data=df)
    plt.title('Average Gross_Sq.Ft by Fiscal Year')
    plt.xlabel('Fiscal Year')
    print my_dpi
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/area_dist.png', dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def cat_distribution():
    conn = uo.connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year, Cat FROM EUAS_monthly', conn)
    df['Fiscal_Year'] = df['Fiscal_Year'].map(int)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.color_palette('Set3'))
    sns.factorplot(x='Fiscal_Year', hue='Cat', hue_order=['A', 'I',
                                                          'C', 'B',
                                                          'D', 'E'],
                   data=df,
                   kind='count',
                   size=6,
                   aspect=2)
    plt.ylabel('Building Count')
    print my_dpi
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/cat_count.png', dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def temp():
    df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    df = df[df['Cat'].isin(['A', 'C', 'I'])]
    df = df[df['Fiscal Year'] > 2006]
    df = df[df['Fiscal Year'] < 2016]
    print df.groupby(['Fiscal Year']).count()[['Building Number']]
    df2 = df.groupby(['Building Number']).filter(lambda x: len(x) > 5)
    print '> 5 year records', len(df2['Building Number'].unique())
    return

def building_count_plot(plot_set):
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_category', conn)
    # df = pd.read_csv(master_dir + 'EUAS_static_tidy.csv')
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        # df2 = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
        df2 = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy', conn)
        print df2.groupby('Fiscal_Year').count().head(n=15)
    if 'AI' in plot_set:
        study_set = gbs.get_cat_set(['A', 'I'])
    if 'ACI' in plot_set:
        study_set = gbs.get_cat_set(['A', 'C', 'I'])
    if 'ECM' in plot_set:
        study_set = gbs.get_ecm_set()
    if 'Invest' in plot_set:
        study_set = gbs.get_invest_set()[-1]
    else:
        study_set = gbs.get_all_building_set()
    df = df[df['Building_Number'].isin(study_set)]
    df2 = df2[df2['Building_Number'].isin(study_set)]
    df2['good_elec'] = df2['eui_elec'].map(lambda x: 'Electric EUI >='+
        ' 12' if x >= 12 else np.nan)
    df2['good_gas'] = df2['eui_gas'].map(lambda x: 'Gas EUI >= 3' if x
                                         >= 3 else np.nan)
    good_both_str = '\n'.join(tw.wrap('Electric EUI >= 12 and Gas EUI'+
        ' >= 3', 20))
    df2['good_both'] = df2.apply(lambda r: good_both_str if
                                 (r['eui_elec'] >= 12 and r['eui_gas']
                                  >= 3) else np.nan, axis=1)
    df3 = pd.melt(df2, id_vars=['Building_Number', 'Fiscal_Year'], value_vars=['good_elec', 'good_gas', 'good_both'])
    df2 = df2[['Building_Number', 'Fiscal_Year']]
    df2['value'] = 'All Building'
    df3.drop('variable', axis=1, inplace=True)
    df4 = pd.concat([df2, df3], ignore_index=True)
    df4['Fiscal_Year'] = df4['Fiscal_Year'].map(lambda x: str(int(x)))
    print 'Plot set: {0}'.format(plot_set)
    df_gr = (df4.groupby(['Fiscal_Year', 'value']).size()).to_frame('count')
    df_gr.replace('\n', ' ', inplace=True)
    print df_gr.head(n=15)
    df_gr.to_csv(os.getcwd() + '/plot_FY_annual/quant_data/count_{0}.csv'.format(plot_set))
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.5)
    sns.set_palette(sns.color_palette('Set3'))
    # sns.countplot(hue='value', x='Fiscal Year', data=df4)
    sns.factorplot(hue='value', x='Fiscal_Year', data=df4,
                   kind='count', size=6, aspect=2)
    if plot_set == 'ECM':
        plt.title('Count of Building in EUAS data set with ECM')
    elif plot_set == 'AIECM':
        plt.title('Count of A + I Building in EUAS data set with ECM')
    elif plot_set == 'AllInvest':
        plt.title('Count of All Building in EUAS data set with Investment')
    elif plot_set == 'AIInvest':
        plt.title('Count of A + I Building in EUAS data set with Investment')
    else:
        plt.title('Count of {0} Building in EUAS data set'.format(plot_set_label[plot_set]))
    # plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    plt.ylabel('Building Count')
    plt.xlabel('Fiscal Year')
    print my_dpi
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/count_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def good_elec_gas():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM eui_by_fy', conn)
        df3 = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    df = df[df['Fiscal_Year'] > 2006]
    df = df[df['Fiscal_Year'] < 2016]
    df['good_both'] = df.apply(lambda r: 1 if (r['eui_elec'] >= 12 and
                                               r['eui_gas'] >= 3) else
                               0, axis=1)
    df = df[['Building_Number', 'good_both']]
    df2 = df.groupby('Building_Number').sum()
    df2 = df2[df2['good_both'] > 5]
    df2.reset_index(inplace=True)
    df_long = pd.merge(df3, df2, on='Building_Number', how='left')
    df_short = pd.merge(df3, df2, on='Building_Number', how='right')
    df_long['Status'] = "All building in EUAS data set"
    df_short['Status'] = "Electric EUI >= 12 and\nGas EUI >= 3\nfor at least 6 years\nfrom FY2007 to FY2015"
    df_c = pd.concat([df_long, df_short], ignore_index=True)
    print len(df_long), len(df_short), len(df_c)
    rename_dict = {\
        'A': 'A\n{0}'.format('\n'.join(tw.wrap('Government Goal', 10))), 
        'I': 'I\n{0}'.format('\n'.join(tw.wrap('Energy Intensive', 10))), 
        'B': 'B\n{0}'.format('\n'.join(tw.wrap('Government Exempt', 10))), 
        'C': 'C\n{0}'.format('\n'.join(tw.wrap('Lease', 10))), 
        'D': 'D\n{0}'.format('\n'.join(tw.wrap('Lease Exempt', 10))), 
                   'E': 'E\n{0}'.format('\n'.join(tw.wrap('Reimbursable non reportable', 12)))}
    df_c.replace(rename_dict, inplace=True)
    order = [rename_dict[k] for k in ['A', 'I', 'B', 'C', 'D', 'E']]
    sns.set_palette(sns.color_palette('Set2'))
    sns.set_context("talk", font_scale=1.2)
    sns.countplot(x='Cat', order=order, hue='Status', data=df_c)
    plt.title('Building Category Count Plot')
    plt.ylabel('Building Count')
    plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    print 'write to cat_count_all_building_quality.png ...'
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/cat_count_all_building_quality.png', dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def fuel_type_plot(years=None, catfilter=None):
    print 'plot fuel type {0} to {1}, {2}'.format(years[0], years[-1], catfilter)
    # year_labels = [str(y)[2:] for y in years]
    # filelist = ['{0}fuel_type/FY{1}.csv'.format(homedir, yr) for yr in year_labels]
    # dfs = []
    # for f in filelist:
    #     df = pd.read_csv(f)
    #     year = f[-6: -4]
    #     df['year'] = '20{0}'.format(year)
    #     dfs.append(df)
    # df_all = pd.concat(dfs, ignore_index=False)
    conn = uo.connect('all')
    with conn:
        df_all = pd.read_sql('SELECT * FROM fuel_type', conn)
    
    df_all['Fiscal_Year'] = df_all['Fiscal_Year'].map(int)
    if not years is None:
        df_all = df_all[df_all['Fiscal_Year'].isin(years)]
    fuel_type_cols = ['No Data', 'Gas Only', 'Oil Only', 
                      'Steam Only', 'Gas + Oil', 'Gas + Steam', 
                      'Oil + Steam', 'Gas + Oil + Steam']
    df_all = df_all[['Building_Number', 'heating_fuel_type', 'Fiscal_Year']]
    with conn:
        df_cat = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    df_all2 = pd.merge(df_all, df_cat, on='Building_Number',
                       how='left')
    if catfilter == 'AI':
        df_all2 = df_all2[df_all2['Cat'].isin(['A', 'I'])]
    sns.set_style("whitegrid")
    sns.set_palette(sns.color_palette('Set2'))
    sns.set_context("talk", font_scale=1.5)
    # sns.mpl.rc("figure", figsize=(10, 5))
    sns.factorplot(x='Fiscal_Year',
                   hue='heating_fuel_type', palette='Set3',
                   hue_order=fuel_type_cols, data=df_all2,
                   kind='count', size=6, aspect=2)
    # sns.countplot(x='year', order= [str(x) for x in years],
    #               hue='Heating Fuel Type', palette='Set3',
    #               hue_order=fuel_type_cols, data=df_all2)
    # plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    plt.title('Count of {2} Building by Heating Fuel Type (FY{0} - FY{1})'.format(years[0], years[-1], plot_set_label[catfilter]))
    plt.ylabel('Number of Buildings')
    plt.xlabel('Fiscal Year')
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/fuel_type_{0}.png'.format(catfilter), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()

def plot_type(plot_set):
    ecm_set = gbs.get_ecm_set()
    energy_set = gbs.get_energy_set('eui')
    # df = pd.read_csv(master_dir + 'EUAS_type.csv')
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_type', conn)
    if plot_set == 'AI':
        ai_set = gbs.get_cat_set(['A', 'I'], conn)
        df = df[df['Building_Number'].isin(ai_set)]
    df.fillna({'Self-Selected_Primary_Function': 'No Data'},
              inplace=True)
    eng_ecm_str = '\n'.join(tw.wrap('with ECM Action and at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 20))
    df['ECM'] = df['Building_Number'].map(lambda x: 'with ECM action'
                                          if x in ecm_set else
                                          'without ECM Action')
    df2 = df.copy()
    df2 = df2[df2['Building_Number'].isin(ecm_set)]
    df2 = df2[df2['Building_Number'].isin(energy_set)]
    # df['good_energy'] = df['Building Number'].map(lambda x: 'at least 6 years of Electric EUI >= 12 and Gas EUI >= 3' if x in energy_set else np.nan]
    df2['ECM'] = eng_ecm_str
    df3 = pd.concat([df, df2], ignore_index=True)
    sns.set_style("whitegrid")
    sns.set_palette(sns.color_palette('Set2'))
    sns.set_context("talk", font_scale=1.2)
    sns.countplot(y='Self-Selected_Primary_Function', hue='ECM',
                  data=df3, orient='v')
    plt.title('Building Type Count Plot')
    plt.suptitle('plot set: {0}'.format(plot_set_label[plot_set]) +
        ' building')
    plt.ylabel('Self-Selected Primary Function')
    plt.xlabel('Building Count')
    plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    print 'write to use_count_{0}.png'.format(plot_set)
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/use_count_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()
    return

def plot_co_count(plot_set, **kwargs):
    sns.set_style("whitegrid")
    sns.set_palette(sns.color_palette('Set3'))
    if 'energy_set' in kwargs:
        df = pd.read_csv(master_dir + 'EUAS_static_tidy.csv')
        df = df[df['Building Number'].isin(kwargs['energy_set'])]
        sns.set_context("talk", font_scale=1)
    else:
        df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
        sns.set_context("talk", font_scale=1.5)
    if plot_set == 'AI':
        df = df[df['Cat'].isin(['A', 'I'])]
    s1 = get_co_set('Capital')
    s2 = get_co_set('Operational')
    sc = s1.difference(s2)
    so = s2.difference(s1)
    sco = s1.intersection(s2)
    sn = s1.union(s2)
    def classify(x):
        if x in sc:
            return 'C Only'
        elif x in so:
            return 'O Only'
        elif x in sco:
            return 'C&O'
        elif x not in sn:
            return 'No'
    order=['C&O', 'C Only' ,'O Only' , 'No']
    df['Capital or Operational'] = df['Building Number'].map(classify)
    df['Fiscal Year'] = df['Fiscal Year'].map(lambda x: str(int(x)))
    if 'energy_set' in kwargs:
        eng_ecm_str = '\n'.join(tw.wrap('with ECM Action and at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
        sns.factorplot(x='Capital or Operational', order=order,
                       data=df, kind='count', size=6, aspect=1)
        # sns.countplot(x='Capital or Operational', order=order, data=df)
        plt.subplots_adjust(top=0.85)
        plt.suptitle('plot set: building {0}'.format(eng_ecm_str))
    else:
        sns.factorplot(hue='Capital or Operational', x='Fiscal Year',
                       hue_order=order, data=df, kind='count', size=6,
                       aspect=2)
        # sns.countplot(hue='Capital or Operational', x='Fiscal Year',
        #             hue_order=order, data=df)
        plt.subplots_adjust(top=0.9)
    plt.title('{0} Building Count of Capital vs Operational Investment'.format(plot_set_label[plot_set]))
    plt.ylabel('Building Count')
    if 'energy_set' in kwargs:
        P.savefig(os.getcwd() + '/plot_FY_annual/quant/co_count_good_energy_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    else:
        P.savefig(os.getcwd() + '/plot_FY_annual/quant/co_count_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()
    return

def get_stat(plot_set, energy_set):
    ai_set = gbs.get_ai_set()
    if energy_set == None:
        study = ai_set
    else:
        study = ai_set.intersection(energy_set)
    df_ecm = pd.read_csv(master_dir + 'ECM/EUAS_ecm.csv')
    df_ecm = df_ecm[df_ecm['Building Number'].isin(study)]
    df_ecm = df_ecm[df_ecm['high_level_ECM'].notnull()]
    gr = df_ecm.groupby(['high_level_ECM', 'detail_level_ECM']).count()
    solo = df_ecm.groupby(['Building Number']).filter(lambda x: len(x) == 1)
    print solo.head()
    if energy_set == None:
        gr.to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmAction_{0}.csv'.format(plot_set))
        solo.groupby(['high_level_ECM', 'detail_level_ECM']).count().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmAction_solo_{0}.csv'.format(plot_set))
    else:
        gr.to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmAction_{0}_energy.csv'.format(plot_set))
        solo.groupby(['high_level_ECM', 'detail_level_ECM']).count().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmAction_solo_{0}_energy.csv'.format(plot_set))
    df_pro = pd.read_csv(master_dir + 'ecm_program_tidy.csv')
    df_pro = df_pro[df_pro['ECM program'] != 'Energy Star']
    df_pro = df_pro[df_pro['Building Number'].isin(study)]
    gr = df_pro.groupby('ECM program').count()
    solo = df_pro.groupby(['Building Number']).filter(lambda x: len(x) == 1)
    if energy_set == None:
        gr.to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmProgram_{0}.csv'.format(plot_set))
        solo.groupby(['ECM program']).count().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmProgram_solo_{0}.csv'.format(plot_set))
    else:
        gr.to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmProgram_{0}_energy.csv'.format(plot_set))
        solo.groupby(['ECM program']).count().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/ecmProgram_solo_{0}_energy.csv'.format(plot_set))
    return

def plot_c_or_o(plot_set, co, energy_set):
    df = pd.read_csv(master_dir + 'EUAS_static_tidy.csv')
    if plot_set == 'AI':
        df = df[df['Cat'].isin(['A', 'I'])]
    study_set = set(df['Building Number'].tolist())
    df = df[df['Building Number'].isin(energy_set)]
    dfs = []
    def cap_df(buildings, invest):
        df = pd.DataFrame({'Building Number': list(buildings)})
        df['Investment'] = invest 
        return df
    if co == 'Capital':
        dfs.append(cap_df(gbs.get_ecm_nogsalink(), 'ECM Action'))
        dfs.append(cap_df(gbs.get_program_set('LEED'), 'LEED'))
        dfs.append(cap_df(gbs.get_program_set('GP'), 'GSA Guiding' +
            ' Principles'))
    elif co == 'Operational':
        dfs.append(cap_df(gbs.get_program_set('first fuel'), 'First Fuel'))
        dfs.append(cap_df(gbs.get_program_set('Shave Energy'), 'Shave Energy'))
        dfs.append(cap_df(gbs.get_program_set('E4'), 'E4'))
        dfs.append(cap_df(gbs.get_action_set('high_level_ECM', 'GSALink'),
                   'GSALink'))
    df_all = pd.concat(dfs, ignore_index=True)
    df_all = df_all[df_all['Building Number'].isin(study_set)]
    print df_all.groupby('Investment').count()
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    sns.set_palette(sns.color_palette('Set3'))
    # sns.countplot(x='Investment', data=df_all)
    sns.factorplot(x='Investment', data=df_all, kind='count', size=6,
                   aspect=1)
    plt.subplots_adjust(top=0.85)
    if co == 'Capital':
        plt.ylim((0, 350))
    plt.ylabel('Building Count')
    plt.title('Count of Building with {0} Investment'.format(co))
    eng_ecm_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('plot set: {0} building {1}'.format(plot_set_label[plot_set], eng_ecm_str))
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/{1}_count_good_energy_{0}.png'.format(plot_set, co), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()
    return
    
def get_size(df, col, label_dict):
    df_size = df.groupby(['Fiscal_Year', col]).size().to_frame('count')
    df_size.reset_index(inplace=True)
    keys = label_dict.keys()
    df_size['order'] = df_size[col].map(lambda x: keys.index(x))
    df_size.sort('order', inplace=True)
    df_size['label'] = df_size.apply(lambda r: '{0} = {1}'.format(r[col], r['count']), axis=1)
    df_size.replace({'label': label_dict}, inplace=True)
    df_size = df_size[['Fiscal_Year', 'label']]
    d = df_size.groupby('Fiscal_Year')['label'].apply(lambda x: '\n'.join(x)).to_dict()
    return {k: '{0}\n{1}'.format(k, d[k]) for k in d}

def classify_fullname(x, cap_only, op_only, cap_and_op, cap_or_op):
    if x in cap_only:
        return 'Capital Only'
    elif x in op_only:
        return 'Operational Only'
    elif x in cap_and_op:
        return 'Capital and Operational'
    else:
        return 'No Known Investment'
        
def plot_pnnl(x1, y1, y2 ,y3, plot_set, energy_filter, total_type, method, cat_current):
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1)
    f, (ax1, ax2, ax3) = plt.subplots(3, sharex=True)
    ax1.plot(x1, y1, color='blue', marker='o')
    for x,y in zip(x1, y1):
        ax1.annotate('{:,.0f}'.format(int(round(y, 0))), xy = (x - 0.5, y-0.7e4), fontsize=12, weight='semibold', color='black')
    ax1.set_ylabel('EUI Btu/SF ({0})'.format(lb.total_type_dict[total_type]))
    for x,y in zip(x1, y2):
        ax2.annotate('{:,.0f}'.format(int(round(y, 0))), xy = (x - 0.5, y-0.7e3), fontsize=12, weight='semibold', color='black')
    ax2.plot(x1, y2, color='red', marker='o')
    ax2.set_ylabel('Energy Use (BBtu)')
    ax3.plot(x1, y3, color='green', marker='o')
    plt.sca(ax1)
    plt.yticks(range(40000, 90000, 20000), ['40k', '60k', '80k'])
    ax1.set_ylim((40000, 95000))
    plt.sca(ax2)
    plt.yticks(range(5000, 19000, 5000), ['5k', '10k', '15k'])
    ax2.set_ylim((5000, 19000))
    plt.sca(ax3)
    plt.yticks(range(100000, 210000, 50000), ['100k', '150k', '200k'])
    for x,y in zip(x1, y3):
        ax3.annotate('{:,.0f}'.format(int(round(y, 0))), xy = (x - 0.5, y-0.1e4), fontsize=12, weight='semibold', color='black')
    ax3.set_ylabel('Floor Space/kSF')
    plt.xlim((2002, 2015))
    plt.xlabel('Fiscal Year')
    # plt.show()
    if cat_current:
        P.savefig(os.getcwd() + '/plot_FY_annual/quant/eui_trend_{0}_{1}_{2}_{3}_currentCat.png'.format(plot_set, total_type, method, energy_filter), dpi = my_dpi)
    else:
        P.savefig(os.getcwd() + '/plot_FY_annual/quant/eui_trend_{0}_{1}_{2}_{3}_latestCat.png'.format(plot_set, total_type, method, energy_filter), dpi = my_dpi)
    plt.close()

# cat_current: if true, using current year category, else use latest record
def prepare(plot_set, cat_current, energy_filter):
    conn = sqlite3.connect(homedir + 'db/all.db')
    df_exc = pd.read_csv(os.getcwd() + \
                         '/input/FY/excluded_buildings.csv')
    exc_set = set(df_exc['Building_Number'].tolist())
    # exc_set = set(['AL0011ZZ','FL0033ZZ','FL0039ZZ','MO0039ZZ','TX0000TG','TX0000BM','TX0000CB','TX0000CR','TX0000CV','TX0000EP','TX0000JL','TX0000LI','TX0000LT','TX0000NW','TX0000PH','TX0000RM'])
    with conn:
        df = pd.read_sql('SELECT Building_Number, Fiscal_Year, [Electric_(kBtu)], [Gas_(kBtu)], [Total_(kBtu)], Cat FROM EUAS_monthly', conn)
        df_eui = pd.read_sql('SELECT Building_Number, Fiscal_Year, Cat, eui, eui_total FROM eui_by_fy', conn)
    if energy_filter != None:
        energy_set = gbs.get_energy_set(energy_filter)
    if not cat_current:
        study_set = None
        if plot_set == 'AI':
            ai_set = gbs.get_cat_set(['A', 'I'], conn)
            if energy_filter != None:
                study_set = ai_set.intersection(energy_set)
            else:
                study_set = ai_set
        elif plot_set == 'ACI':
            aci_set = gbs.get_cat_set(['A', 'C', 'I'], conn)
            if energy_filter != None:
                study_set = aci_set.intersection(energy_set)
            else:
                study_set = aci_set
        elif plot_set == 'select':
            study_set = gbs.get_all_building_set().difference(exc_set)
        else:
            if energy_filter != None:
                study_set = energy_set
        study_set = study_set.difference(exc_set)
        if study_set != None:
            df = df[df['Building_Number'].isin(study_set)]
            df_eui = df_eui[df_eui['Building_Number'].isin(study_set)]
    elif cat_current:
        if plot_set == 'AI':
            df = df[df['Cat'].isin(['A', 'I'])]
            df_eui = df_eui[df_eui['Cat'].isin(['A', 'I'])]
        elif plot_set == 'ACI':
            df = df[df['Cat'].isin(['A', 'C', 'I'])]
            df_eui = df_eui[df_eui['Cat'].isin(['A', 'C', 'I'])]
        if energy_filter != None:
            df = df[df['Building_Number'].isin(energy_set)]
            df_eui = df_eui[df_eui['Building_Number'].isin(energy_set)]
    # total consumption
    df2 = df.groupby('Fiscal_Year').sum()
    df2.reset_index(inplace=True)
    with conn:
        df3 = pd.read_sql('SELECT * FROM EUAS_area_cat', conn)
    if plot_set != 'All' or energy_filter != None:
        if not cat_current:
            df3 = df3[df3['Building_Number'].isin(study_set)]
        else:
            if plot_set == 'AI':
                df3 = df3[df3['Cat'].isin(['A', 'I'])]
            elif plot_set == 'ACI':
                df3 = df3[df3['Cat'].isin(['A', 'C', 'I'])]
            if energy_filter != None:
                df3 = df3[df3['Building_Number'].isin(energy_set)]
    # total area
    df_area = df3.groupby('Fiscal_Year').sum()
    # df_area.reset_index(inplace=True)
    df_all = pd.merge(df2, df_area, left_on='Fiscal_Year', right_index=True, how='left')
    df_all['Total Electric + Gas'] = df_all['Gas_(kBtu)'] + df_all['Electric_(kBtu)']
    df_merge = pd.merge(df_eui, df3, on=['Building_Number', 'Fiscal_Year'], how='left')
    return df_all, df_eui, df_merge, df_area

# weight needs re calculate based on selection set
def plot_eui_trend_weighted_mean(plot_set, total_type, energy_filter, cat_current):
    df_all, _, df_merge, df_area = prepare(plot_set, cat_current, energy_filter)
    df_merge['weight'] = df_merge.apply(lambda r: r['Gross_Sq.Ft']/df_area.ix[int(r['Fiscal_Year']),'Gross_Sq.Ft'], axis=1)
    df_merge['weighted'] = df_merge['eui'] * df_merge['weight']
    df_merge['weighted_total'] = \
        df_merge['eui_total'] * df_merge['weight']
    df_result = df_merge.groupby('Fiscal_Year').sum()
    x1 = df_all['Fiscal_Year']
    if total_type == 'elec_gas':
        y1 = df_result['weighted']*1e3
        y2 = df_all['Total Electric + Gas']*1e-6
    elif total_type == 'all_type':
        y1 = df_result['weighted_total']*1e3
        y2 = df_all['Total_(kBtu)']*1e-6
    y3 = df_all['Gross_Sq.Ft']*1e-3
    plot_pnnl(x1, y1, y2, y3, plot_set, energy_filter, total_type, 'weighted', cat_current)

def plot_eui_trend_simple_mean(plot_set, total_type, energy_filter, cat_current):
    df_all, df_eui, _, _ = prepare(plot_set, cat_current, energy_filter)
    df_result = df_eui.groupby('Fiscal_Year').mean()
    x1 = df_all['Fiscal_Year']
    if total_type == 'elec_gas':
        y1 = df_result['eui']*1e3
        y2 = df_all['Total Electric + Gas']*1e-6
    elif total_type == 'all_type':
        y1 = df_result['eui_total']*1e3
        y2 = df_all['Total_(kBtu)']*1e-6
    y3 = df_all['Gross_Sq.Ft']*1e-3
    # print
    # print df_all[['Fiscal_Year', 'EUI']]
    plot_pnnl(x1, y1, y2, y3, plot_set, energy_filter, total_type, 'simpleMean', cat_current)

def plot_eui_trend_total_total(plot_set, total_type, energy_filter, cat_current):
    df_all, _, _, _ = prepare(plot_set, cat_current, energy_filter)
    x1 = df_all['Fiscal_Year']
    if total_type == 'elec_gas':
        y1 = df_all['Total Electric + Gas']/df_all['Gross_Sq.Ft']*1e3
        y2 = df_all['Total Electric + Gas']*1e-6
    elif total_type == 'all_type':
        y1 = df_all['Total_(kBtu)']/df_all['Gross_Sq.Ft']*1e3
        y2 = df_all['Total_(kBtu)']*1e-6
    y3 = df_all['Gross_Sq.Ft']*1e-3
    # print
    # print df_all[['Fiscal_Year', 'EUI']]
    plot_pnnl(x1, y1, y2, y3, plot_set, energy_filter, total_type, 'totalOverTotal', cat_current)

def plot_co_type_boxtrend(plot_set, theme, ylimit, agg):
    order=['Capital and Operational', 'Capital Only', 'Operational'+
    ' Only' ,'No Known Investment']
    df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    if plot_set == 'AI':
        df = df[df['Cat'].isin(['A', 'I'])]
    energy_set = gbs.get_energy_set(theme)
    df = df[df['Building Number'].isin(energy_set)]
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    if agg == 'ave':
        df = get_agg(df)
    df['Investment Type'] = df['Building Number'].map(lambda x: classify_fullname(x, cap_only, op_only, cap_and_op, cap_or_op))
    if agg != 'ave':
        df['in_range'] = df['Fiscal Year'].map(lambda x: 2006 < x and x <
                                            2016)
        df = df[df['in_range']]
        df.sort('Fiscal Year', inplace=True)
        df['Fiscal Year'] = df['Fiscal Year'].map(lambda x: str(int(x)))
    label_dict = {'Capital Only': 'C', 
                  'Operational Only': 'O', 
                  'Capital and Operational': 'C&O', 
                  'No Known Investment': 'No'}
    size_dict = get_size(df, 'Investment Type', label_dict)
    df.replace({'Fiscal Year': size_dict}, inplace=True)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.2)
    sns.set_palette(sns.color_palette('Set2'))
    if agg == 'ave':
        aspect = 1.5
    else:
        aspect = 2
    g = sns.FacetGrid(df, aspect=aspect, size=6, legend_out=True)
    g = g.map(sns.boxplot, x='Fiscal Year', y='eui', hue='Investment Type', hue_order=order, data=df, palette='Set2')
    if agg == 'ori':
        pointsize = 1
    else:
        pointsize = 2
    g.map(sns.stripplot, x='Fiscal Year', y='eui', hue='Investment Type', hue_order=order, data=df, jitter=0.3, size=pointsize, color='dimgray', edgecolor='dimgray', label='_nolegend_')
    df.groupby(['Fiscal Year', 'Investment Type']).mean().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/mean_type_{0}_{1}.csv'.format(plot_set, agg))
    # sns.boxplot(x='Fiscal Year', y='eui', hue='Investment Type',
    #             hue_order=order, data=df)
    # sns.stripplot(x='Fiscal Year', y='eui', hue='Investment Type',
    #               hue_order=order, data=df, jitter=0.3, size=1,
    #               color='dimgray', edgecolor='dimgray',
    #               label='_nolegend_')
    plt.subplots_adjust(top=0.87)
    plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    if agg == 'ori':
        plt.title('{0} Distribution Trend'.format(lb.title_dict[theme]))
    elif agg == 'ave':
        plt.title('Average {0} Distribution Trend'.format(lb.title_dict[theme]))
    eng_ecm_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('plot set: {0} building {1}'.format(plot_set_label[plot_set], eng_ecm_str))
    plt.ylabel(lb.ylabel_dict[theme])
    plt.ylim((0, 140))
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/invest_type_{0}_{1}_{2}.png'.format(plot_set, theme, agg), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def get_agg(df):
    df1 = df[df['Fiscal_Year'] < 2009]
    df1 = df1.drop('Fiscal_Year', axis=1)
    df2 = df[df['Fiscal_Year'] > 2013]
    df2 = df2.drop('Fiscal_Year', axis=1)
    df1_mean = df1.groupby('Building_Number').mean()
    df1_mean.reset_index(inplace=True)
    df1_mean['Fiscal_Year'] = 'FY2007 and FY2008'
    df2_mean = df2.groupby('Building_Number').mean()
    df2_mean.reset_index(inplace=True)
    df2_mean['Fiscal_Year'] = 'FY2014 and FY2015'
    df = pd.concat([df1_mean, df2_mean], ignore_index=True)
    df.reset_index(inplace=True)
    return df

def plot_co_boxtrend(plot_set, theme, ylimit, agg):
    # df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM eui_by_fy', conn)
    if plot_set == 'AI':
        ai_set = gbs.get_cat_set(['A', 'I'], conn)
        # df = df[df['Cat'].isin(['A', 'I'])]
    energy_set = gbs.get_energy_set(theme)
    study_set = ai_set.intersection(energy_set)
    df = df[df['Building_Number'].isin(energy_set)]
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    df['Have any investment'] = df['Building_Number'].map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    def classify(x):
        if x in cap_only:
            return 'Capital Only'
        elif x in op_only:
            return 'Operational Only'
        elif x in cap_and_op:
            return 'Capital and Operational'
        elif x not in cap_or_op:
            return 'No Capital or Operational'
    if agg == 'ave':
        df = get_agg(df)
    df['Have any investment'] = df['Building_Number'].map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    if agg != 'ave':
        df['in_range'] = df['Fiscal_Year'].map(lambda x: 2006 < x and x <
                                            2016)
        df = df[df['in_range']]
        df.sort('Fiscal_Year', inplace=True)
        df['Fiscal_Year'] = df['Fiscal_Year'].map(lambda x: str(int(x)))
    label_dict = {'With Investment': 'Yes', 'No Known Investment': 'No'}
    size_dict = get_size(df, 'Have any investment', label_dict)
    df.replace({'Fiscal_Year': size_dict}, inplace=True)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.2)
    sns.set_palette(sns.color_palette('Set2'))
    if agg == 'ave':
        aspect = 1.5
    else:
        aspect = 2
    g = sns.FacetGrid(df, aspect=aspect, size=6, legend_out=True)
    g = g.map(sns.boxplot, x='Fiscal_Year', y='eui', hue='Have any investment', data=df, palette='Set2', fliersize=0)
    df.groupby(['Fiscal_Year', 'Have any investment']).median().to_csv(homedir + 'temp/co_saving.csv')
    # g.map(sns.stripplot, x='Fiscal_Year', y='eui', hue='Have any investment', data=df, jitter=0.3, size=1, color='dimgray', edgecolor='dimgray')

    # sns.boxplot(x='Fiscal Year', y='eui', hue='Have any investment', data=df)
    # sns.stripplot(x='Fiscal Year', y='eui', hue='Have any investment',
    #               data=df, jitter=0.3, size=1, color='dimgray',
    #               edgecolor='dimgray', label='_nolegend_')
    df.groupby(['Fiscal_Year', 'Have any investment']).mean().to_csv(os.getcwd() + '/plot_FY_annual/quant_data/mean_with_{0}_{1}.csv'.format(plot_set, agg))
    plt.subplots_adjust(top=0.87)
    plt.legend(loc = 2, bbox_to_anchor=(1, 1))
    if agg == 'ori':
        plt.title('{1} Building {0} Distribution Trend'.format(lb.title_dict[theme], plot_set_label[plot_set]))
    elif agg == 'ave':
        plt.title('{1} Building Average {0} Distribution Trend'.format(lb.title_dict[theme], plot_set_label[plot_set]))
    eng_ecm_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('plot set: Building {0}'.format(eng_ecm_str))
    plt.ylabel(lb.ylabel_dict[theme])
    plt.ylim((0, 140))
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/invest_{0}_{1}_{2}.png'.format(plot_set, theme, agg), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()

def ecm_time_dist(plot_set):
    df = pd.read_csv(master_dir + 'ECM/EUAS_ecm.csv')
    df = df[df['high_level_ECM'].notnull()]
    df = df[df['Substantial Completion Date'].notnull()]
    if plot_set == 'AI':
        ai_set = gbs.get_ai_set()
        df = df[df['Building Number'].isin(ai_set)]
    df['Calendar Year'] = df['Substantial Completion Date'].map(lambda x: x[:4])
    years = set(df['Calendar Year'].tolist())
    sns.set_context("talk", font_scale=1.5)
    sns.set_palette(sns.color_palette('Set3'))
    df.rename(columns={'high_level_ECM': 'High Level ECM Action'},
              inplace=True)
    sns.factorplot(x='Calendar Year', order=sorted(years),
                   hue='High Level ECM Action', data=df, kind='count', size=6, aspect=2)
    plt.title('{0} Building ECM Action Completion Year Distribution'.format(plot_set_label[plot_set]))
    plt.xlabel('Calendar Year')
    plt.ylabel('Building Count')
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/ecm_time_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.show()
    plt.close()
    return

def get_time_label(status, yearlist):
    if len(yearlist) > 1:
        return '{0} (FY{1}--FY{2})'.format(status, *yearlist)
    elif len(yearlist) == 1:
        return '{0} (FY{1})'.format(status, *yearlist)

def compute_saving(theme, study_set, shape, pre_years, post_years):
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy', conn)
    conn.close()
    if study_set != None:
        df = df[df['Building_Number'].isin(study_set)]
    energy_set = gbs.get_energy_set(theme)
    def agg(df, years):
        df = df[df['Fiscal_Year'].isin(years)]
        df = df.groupby(['Building_Number']).filter(lambda x: len(x)
                                                    == len(years))
        df = df[df['Building_Number'].isin(energy_set)]
        df = df[['Building_Number', theme]]
        df_g = df.groupby('Building_Number').mean()
        return df_g
    df_pre = agg(df, pre_years)
    df_post = agg(df, post_years)
    print len(df_pre), len(df_post)
    if shape == 'long':
        df_pre['status'] = get_time_label('pre', pre_years)
        df_post['status'] = get_time_label('post', post_years)
        df_all = pd.concat([df_pre, df_post])
    else:
        df_all = pd.merge(df_pre, df_post, left_index=True,
                          right_index=True, suffixes=['_pre', '_post'])
        df_all['saving amount'] = df_all[theme + '_pre'] - df_all[theme +
                                                                '_post']
        df_all['saving percent'] = \
            df_all['saving amount']/df_all[theme + '_pre'] * 100
    print df_all.head()
    return df_all

def timing(ori, current, funname):
    print '{0} takes {1}s...'.format(funname, current - ori)
    return current

def process_stat(df, col, label_yes, label_no, var, name, lines, dfs):
    df_all = df.reset_index()
    df_yes = df_all[df_all[col] == label_yes]
    df_no = df_all[df_all[col] == label_no]
    a = np.array(df_yes[var])
    b = np.array(df_no[var])
    ave_yes = np.average(a)
    ave_no = np.average(b)
    median_yes = np.median(a)
    median_no = np.median(b)
    print ave_yes, ave_no
    result = stats.ttest_ind(a, b, equal_var=False)
    t = result[0]
    p = result[-1]
    line = '{7},{0},{1},{2},{3},{4},{5},{6},{8},{9},{10},{11}'.format(var,label_yes, label_no, ave_yes, ave_no, t, p/2, name, len(a), len(b), median_yes, median_no)
    print line
    lines.append(line)
    dfs.append(df_yes)
    dfs.append(df_no)
    if name == 'Capital Only':
        print label_yes, len(label_yes)
        # print df_yes['Building_Number'].unique()
        print label_no, len(label_no)
        # print df_no['Building_Number'].unique()
        print set(df_yes['Building_Number'].tolist()).union(set(df_no['Building_Number'].tolist()))
    return
    
def process_stat_no(df, col, label_yes, label_no, var, df_no, name, lines):
    df_all = df.reset_index()
    df_yes = df_all[df_all[col] == label_yes]
    df_no = df_no[df_no[col] == label_no]
    a = np.array(df_yes[var])
    b = np.array(df_no[var])
    ave_yes = np.average(a)
    ave_no = np.average(b)
    median_yes = np.median(a)
    median_no = np.medianr(b)
    print ave_yes, ave_no
    result = stats.ttest_ind(a, b, equal_var=False)
    t = result[0]
    p = result[-1]
    line = '{7},{0},{1},{2},{3},{4},{5},{6},{8},{9},{10},{11}'.format(var,label_yes, label_no, ave_yes, ave_no, t, p, name, len(a), len(b), median_yes, median_no)
    print line
    lines.append(line)

def test_hypo_covered(theme, plot_set, shape, pre_years, post_years):
    ori = time.time()
    energy_set = gbs.get_energy_set('eui')
    if plot_set == 'AI':
        cat_set = gbs.get_cat_set(['A', 'I'])
        study_set = energy_set.intersection(cat_set)
    else:
        study_set = energy_set
    df_all = compute_saving(theme, study_set, shape, pre_years, post_years)
    conn = uo.connect('all')
    with conn:
        df_cover = pd.read_sql('SELECT DISTINCT Building_Number FROM covered_facility', conn)
    covered_buildings = df_cover['Building_Number'].tolist()
    df_all['Is Covered'] = df_all.index.map(lambda x: 'Covered' if x in covered_buildings else 'Not Covered')
    lines = []
    lines.append('name,variable,group A,group B,mean A,mean B,t,p,n_a,n_b,median A, median B')
    dfs = []
    gr = df_all.groupby('Is Covered')
    pre_label = get_time_label('pre', pre_years)
    post_label = get_time_label('post', post_years)
    for name, group in gr:
        print name 
        process_stat(group, 'status', pre_label, post_label, 'eui',
                     name, lines, dfs)
    with open(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_covered_{0}.csv'.format(plot_set), 'w+') as wt:
        wt.write('\n'.join(lines))
    df_all = pd.concat(dfs, ignore_index=True)
    sns.barplot(x='Is Covered', order=['Covered', 'Not Covered'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all)
    plt.title('Average EUI reduction: pre (FY2007 and FY2008) vs. post (FY2014 and FY2015)')
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('plot set: building {0}'.format(eng_str))
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/covered.png', dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()

def box_cap_op(theme, plot_set, shape, pre_years, post_years):
    ori = time.time()
    energy_set = gbs.get_energy_set('eui')
    conn = uo.connect('all')
    if plot_set == 'AI':
        cat_set = gbs.get_cat_set(['A', 'I'], conn)
        study_set = energy_set.intersection(cat_set)
    elif plot_set == 'AIcovered':
        cat_set = gbs.get_cat_set(['A', 'I'], conn)
        covered_set = gbs.get_covered_set()
        study_set = energy_set.intersection(cat_set.intersection(covered_set))
    else:
        study_set = energy_set
    df_all = compute_saving(theme, study_set, shape, pre_years, post_years)
    ori = timing(ori, time.time(), 'compute_saving')
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    ori = timing(ori, time.time(), 'get_invest_set')
    df_all['Investment'] = df_all.index.map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    df_all['Investment Type'] = df_all.index.map(lambda x: classify_fullname(x, cap_only, op_only, cap_and_op, cap_or_op))
    df_all.to_csv(homedir + 'temp/cap_op_eui.csv')
    # print df_all['Investment Type'].value_counts()
    # ori = timing(ori, time.time(), 'classify')
    # eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    # sns.set_style("whitegrid")
    # sns.set_context("talk", font_scale=1)
    # sns.set_palette(sns.color_palette('Set2'))
    # # gr = df_all.groupby('Investment Type')
    # gr = df_all.groupby('Investment')
    # lines = []
    # lines.append('name,variable,group A,group B,mean A,mean B,t,p,n_a,n_b,median A, median B')
    # dfs = []
    # pre_label = get_time_label('pre', pre_years)
    # post_label = get_time_label('post', post_years)
    # print pre_label, post_label
    # with open(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_abs_w_wout{0}.csv'.format(plot_set), 'w+') as wt:
    #     wt.write('\n'.join(lines))

    # if plot_set == 'AIcovered':
    #     pointsize = 2
    #     sns.boxplot(x='Investment Type', order=['Capital and Operational', 'Operational Only'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, fliersize=0)
    #     # sns.stripplot(x='Investment Type', order=['Capital and Operational', 'Operational Only'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, jitter=0.3, size=pointsize, color='dimgray', edgecolor='dimgray', label='_nolegend_')
    # else:
    #     pointsize = 1
    #     sns.boxplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, fliersize=0)
    #     # sns.stripplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, jitter=0.3, size=pointsize, color='dimgray', edgecolor='dimgray', label='_nolegend_')
    # plt.title('Median EUI reduction: {0} vs. {1}'.format(pre_label, post_label))
    # plt.ylim((0, 200))
    # eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    # plt.suptitle('plot set: building {0}'.format(eng_str))
    # plt.show()
    # # P.savefig(os.getcwd() + '/plot_FY_annual/quant/invest_box_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # plt.close()
    return

def test_hypo_absolute(theme, plot_set, shape, pre_years, post_years):
    ori = time.time()
    energy_set = gbs.get_energy_set('eui')
    conn = uo.connect('all')
    if plot_set == 'AI':
        cat_set = gbs.get_cat_set(['A', 'I'], conn)
        study_set = energy_set.intersection(cat_set)
    elif plot_set == 'AIcovered':
        cat_set = gbs.get_cat_set(['A', 'I'], conn)
        covered_set = gbs.get_covered_set()
        study_set = energy_set.intersection(cat_set.intersection(covered_set))
    else:
        study_set = energy_set
    df_all = compute_saving(theme, study_set, shape, pre_years, post_years)
    ori = timing(ori, time.time(), 'compute_saving')
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    ori = timing(ori, time.time(), 'get_invest_set')
    df_all['Investment'] = df_all.index.map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    df_all['Investment Type'] = df_all.index.map(lambda x: classify_fullname(x, cap_only, op_only, cap_and_op, cap_or_op))
    print df_all['Investment Type'].value_counts()
    ori = timing(ori, time.time(), 'classify')
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    # bookmark
    # sns.set_palette(sns.color_palette('Set2'))
    sns.set_palette(sns.color_palette(["#FC8D62", "#66C2A5"]))
    # gr = df_all.groupby('Investment Type')
    gr = df_all.groupby('Investment')
    lines = []
    lines.append('name,variable,group A,group B,mean A,mean B,t,p,n_a,n_b,median A, median B')
    dfs = []
    pre_label = get_time_label('pre', pre_years)
    post_label = get_time_label('post', post_years)
    print pre_label, post_label

    # if plot_set == 'AIcovered':
    #     names = ['Capital and Operational', 'Operational Only']
    # else:
    #     names = gr.groups.keys()
    # print names

    # for name, group in gr:
    #     print name 
    # for name in names:
    #     print name, '1111111111111'
    #     group = gr.get_group(name)
    #     process_stat(group, 'status', post_label, pre_label, 'eui',
    #                  name, lines, dfs)
    # process_stat(df_all, 'status', post_label, pre_label, 'eui', 'all',
    #              lines, dfs)
    # df_all = pd.concat(dfs, ignore_index=True)
    # print df_all['status'].value_counts()
    with open(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_abs_w_wout{0}.csv'.format(plot_set), 'w+') as wt:
        wt.write('\n'.join(lines))

    # sns.barplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all)
    # sns.boxplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all)

    if plot_set == 'AIcovered':
        pointsize = 2
        sns.boxplot(x='Investment Type', order=['Capital and Operational', 'Operational Only'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, fliersize=0)
        # sns.stripplot(x='Investment Type', order=['Capital and Operational', 'Operational Only'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, jitter=0.3, size=pointsize, color='dimgray', edgecolor='dimgray', label='_nolegend_')
    else:
        pointsize = 1
        sns.boxplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, fliersize=0)
        # sns.stripplot(x='Investment Type', order=['Capital and Operational', 'Operational Only', 'Capital Only', 'No Known Investment'], hue='status', y='eui', hue_order = [pre_label, post_label], data=df_all, jitter=0.3, size=pointsize, color='dimgray', edgecolor='dimgray', label='_nolegend_')
    plt.title('Median EUI reduction: {0} vs. {1}'.format(pre_label, post_label))
    plt.ylim((0, 200))
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('plot set: building {0}'.format(eng_str))
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/invest_box_{0}.png'.format(plot_set), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()

    # no use now
    # no_invest = gr.get_group('No Known Investment')
    # lines = []
    # lines.append('name,variable,group A,group B,mean A,mean B,t,p,n_a, n_b')
    # for name, group in gr:
    #     print name 
    #     process_stat_no(group, 'status', 'post', 'pre', 'eui', no_invest, name, lines)
    # with open(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_abs_wno_{0}.csv'.format(plot_set), 'w+') as wt:
    #     wt.write('\n'.join(lines))
    return

def test_hypo(theme, plot_set, shape):
    ori = time.time()
    df_all = compute_saving(theme, plot_set, shape, pre_years, post_years)
    ori = timing(ori, time.time(), 'compute_saving')
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    ori = timing(ori, time.time(), 'get_invest_set')
    df_all['Investment'] = df_all.index.map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    df_all['Investment Type'] = df_all.index.map(lambda x: classify_fullname(x, cap_only, op_only, cap_and_op, cap_or_op))
    ori = timing(ori, time.time(), 'classify')
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.5)
    sns.set_palette(sns.color_palette('Set2'))
    lines = []
    lines.append('variable,group A,group B,mean A,mean B,t,p')
    # def process_stat(df, col, label_yes, label_no, var):
    #     df_all = df.reset_index()
    #     df_yes = df_all[df_all[col] == label_yes]
    #     df_no = df_all[df_all[col] == label_no]
    #     df_yes['Invest'] = label_yes
    #     df_no['Invest'] = label_no
    #     a = np.array(df_yes[var])
    #     b = np.array(df_no[var])
    #     ave_yes = np.average(a)
    #     ave_no = np.average(b)
    #     print ave_yes, ave_no
    #     result = stats.ttest_ind(a, b, equal_var=False)
    #     t = result[0]
    #     p = result[-1]
    #     line = '{0},{1},{2},{3},{4},{5},{6}'.format(var,label_yes, label_no, ave_yes, ave_no, t, p)
    #     print line
    #     lines.append(line)
    #     df_con = pd.concat([df_yes, df_no], ignore_index=True)
    #     df_plot = df_con.replace({'Invest': {label_yes: '{1}\nmean = {0}'.format(round(ave_yes, 2), label_yes), label_no: '{1}\nmean={0}'.format(round(ave_no, 2), label_no)}})
    #     # sns.boxplot(x='Invest', y=var, data=df_plot)
    #     sns.violinplot(x='Invest', y=var, data=df_plot)
    #     plt.ylim((-200, 200))
    #     plt.title('{0} Building Average {1} Distribution\n(t = {2}, p = {3})'.format(plot_set_label[plot_set], var.title(), round(t, 2), round(p, 4)))
    #     plt.subplots_adjust(top=0.85)
    #     plt.suptitle('plot set: building {0}'.format(eng_str))
    #     # plt.show()
    #     filename = 'ave_{0}_{1}_{2}.png'.format(plot_set, var,
    #                                             label_yes)
    #     if label_no != 'No Known Investment':
    #         filename = 'ave_{0}_{1}_{2}_{3}.png'.format(plot_set, var,
    #                                                     label_yes,
    #                                                     label_no)
    #     print 'write to ' + filename
    #     P.savefig(os.getcwd() + '/plot_FY_annual/quant/{0}'.format(filename.replace(' ', '_'), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight'))
    #     plt.close()

    # df_all.info()
    # for var in ['', 'saving percent']:
    #     process_stat(df_all, 'Investment', 'With Investment', 'No Known Investment', var, name, lines)
    # # for var in ['saving amount', 'saving percent']:
    # #     process_stat(df_all, 'Investment', 'With Investment', 'No Known Investment', var)
    # #     process_stat(df_all, 'Investment Type', 'Capital and Operational', 'No Known Investment', var)
    # #     process_stat(df_all, 'Investment Type', 'Capital Only', 'No Known Investment', var)
    # #     process_stat(df_all, 'Investment Type', 'Operational Only', 'No Known Investment', var)
    # #     process_stat(df_all, 'Investment Type', 'Operational Only', 'Capital and Operational', var)
    # #     process_stat(df_all, 'Investment Type', 'Capital and Operational', 'Capital Only', var)
    # #     process_stat(df_all, 'Investment Type', 'Operational Only', 'Capital Only', var)
    # # with open(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_{0}.csv'.format(plot_set), 'w+') as wt:
    # #     wt.write('\n'.join(lines))
    return

def bar_plot_before_after():
    df = pd.read_csv(os.getcwd() + '/plot_FY_annual/quant_data/hypotest_2.csv')
    # df1 = df[['name', 'group A', 'mean A']]
    # df1.rename(columns=lambda x: x.replace(' A', ''), inplace=True)
    # df2 = df[['name', 'group B', 'mean B']]
    # df2.rename(columns=lambda x: x.replace(' B', ''), inplace=True)
    # df3 = pd.concat([df1, df2], ignore_index=True)
    # print
    # print df3
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    sns.set_palette(sns.color_palette('Set3'))
    ax1.bar(np.arange(10), interleave(df['mean A'], df['mean B']))
    plt.show()

def year_of_data(df, n, low, high):
    df = df[df['Fiscal_Year'].map(lambda x: low <= x and x <= high)]
    df2 = df.groupby(['Building_Number']).filter(lambda x: len(x) > 5)
    return df2['Building_Number'].unique()

def flow_chart():
    conn = uo.connect('all')
    aci_set = gbs.get_cat_set(['A', 'C', 'I'], conn)
    ai_set = gbs.get_cat_set(['A', 'C'], conn)
    with conn:
        df_eui = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy', conn)
    df_eui = df_eui[df_eui['eui'] != np.inf]
    df1 = df_eui.groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    df_result = pd.DataFrame({'Fiscal_Year': map(str, range(2003,
                                                            2017)),
                              'All': df1['Building_Number'].tolist()})
    # print df1
    # print 'at least 6 years', len(year_of_data(df_eui, 6, 2007, 2015))
    # df_1 = df_all.groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})

    # print 'A + C + I Building'
    df2 = df_eui[df_eui['Building_Number'].isin(aci_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    df_result['A + C + I'] = df2['Building_Number'].tolist()
    # print df2

    # print 'at least 6 years', len(year_of_data(df_eui[df_eui['Building_Number'].isin(aci_set)], 6, 2007, 2015))
    # df_2 = df_all[df_all['Building_Number'].isin(aci_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    # df_2['intensity'] = df_2['Total_kBtu']/df_2['Gross_Sq.Ft']
    # print df_2[['intensity', 'Building_Number']]
    # print 'A + I Building'
    df3 = df_eui[df_eui['Building_Number'].isin(ai_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    # print df3
    df_result['A + I'] = df3['Building_Number'].tolist()
    # print 'at least 6 years', len(year_of_data(df_eui[df_eui['Building_Number'].isin(ai_set)], 6, 2007, 2015))
    # df_3 = df_all[df_all['Building_Number'].isin(ai_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    # df_3['intensity'] = df_3['Total_kBtu']/df_3['Gross_Sq.Ft']
    # print df_3[['intensity', 'Building_Number']]
    print 'good_elec'
    df4 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    df_result['A + I, good elec'] = df4['Building_Number'].tolist()
    # print df4
    # print 'at least 6 years', len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))
    # df_all = df_all[df_all['eui_elec'] != np.inf]
    # df_4 = df_all[(df_all['Building_Number'].isin(ai_set)) & (df_all['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    # df_4['intensity'] = df_4['Total_kBtu']/df_4['Gross_Sq.Ft']
    # print df_4[['intensity', 'Building_Number']]
    print 'good_gas'
    df5 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_gas'] >= 3)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    df_result['A + I, good gas'] = df5['Building_Number'].tolist()
    # print df5
    # print 'at least 6 years', len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3)], 6, 2007, 2015))
    # df_5 = df_all[(df_all['Building_Number'].isin(ai_set)) & (df_all['eui_gas'] >= 3)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    # df_5['intensity'] = df_5['Total_kBtu']/df_5['Gross_Sq.Ft']
    # print df_5[['intensity', 'Building_Number']]
    print 'good_both'
    df6 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_gas'] >= 3) & (df_eui['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})

    df_result['A + I, good both'] = df6['Building_Number'].tolist()
    df_result.ix['6 year', 'All'] = len(year_of_data(df_eui, 6, 2007,
                                                     2015))
    df_result.ix['6 year', 'A + C + I'] = len(year_of_data(df_eui[df_eui['Building_Number'].isin(aci_set)], 6, 2007, 2015))
    df_result.ix['6 year', 'A + I'] = len(year_of_data(df_eui[df_eui['Building_Number'].isin(ai_set)], 6, 2007, 2015))
    df_result.ix['6 year', 'A + I, good elec'] = len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))
    df_result.ix['6 year', 'A + I, good gas'] = len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3)], 6, 2007, 2015))
    df_result.ix['6 year', 'A + I, good both'] = len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))
    cols = list(df_result)
    cols.remove('Fiscal_Year')
    cols.insert(0, 'Fiscal_Year')
    df_result = df_result[cols]
    df_result.to_csv(homedir + 'temp/flow_chart.csv', index=False)
    return

def flow_chart_verbo():
    # conn = uo.connect('backup/all_old')
    conn = uo.connect('all')
    aci_set = gbs.get_cat_set(['A', 'C', 'I'], conn)
    ai_set = gbs.get_cat_set(['A', 'C'], conn)
    with conn:
        df_eui = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy', conn)
        df_eng = pd.read_sql('SELECT Building_Number, Fiscal_Year, [Electric_(kBtu)], [Gas_(kBtu)], eui_elec, eui_gas FROM EUAS_monthly', conn)
        df_area = pd.read_sql('SELECT * FROM EUAS_area', conn)
    df_eng['Total_kBtu'] = df_eng['Electric_(kBtu)'] + df_eng['Gas_(kBtu)']
    df_total = df_eng.groupby(['Building_Number', 'Fiscal_Year']).sum()
    df_total.reset_index(inplace=True)
    df_all = pd.merge(df_total, df_area, on=['Building_Number', 'Fiscal_Year'], how='inner')
    df_eui = df_eui[df_eui['eui'] != np.inf]
    df1 = df_eui.groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print 'All Building'
    print df1
    print 'at least 6 years', len(year_of_data(df_eui, 6, 2007, 2015))
    df_1 = df_all.groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_1['intensity'] = df_1['Total_kBtu']/df_1['Gross_Sq.Ft']
    print df_1[['intensity', 'Building_Number']]

    print 'A + C + I Building'
    df2 = df_eui[df_eui['Building_Number'].isin(aci_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print df2
    print 'at least 6 years', len(year_of_data(df_eui[df_eui['Building_Number'].isin(aci_set)], 6, 2007, 2015))
    df_2 = df_all[df_all['Building_Number'].isin(aci_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_2['intensity'] = df_2['Total_kBtu']/df_2['Gross_Sq.Ft']
    print df_2[['intensity', 'Building_Number']]
    print 'A + I Building'
    df3 = df_eui[df_eui['Building_Number'].isin(ai_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print df3
    print 'at least 6 years', len(year_of_data(df_eui[df_eui['Building_Number'].isin(ai_set)], 6, 2007, 2015))
    df_3 = df_all[df_all['Building_Number'].isin(ai_set)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_3['intensity'] = df_3['Total_kBtu']/df_3['Gross_Sq.Ft']
    print df_3[['intensity', 'Building_Number']]
    print 'good_elec'
    df4 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print df4
    print 'at least 6 years', len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))
    df_all = df_all[df_all['eui_elec'] != np.inf]
    df_4 = df_all[(df_all['Building_Number'].isin(ai_set)) & (df_all['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_4['intensity'] = df_4['Total_kBtu']/df_4['Gross_Sq.Ft']
    print df_4[['intensity', 'Building_Number']]
    print 'good_gas'
    df5 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_gas'] >= 3)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print df5
    print 'at least 6 years', len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3)], 6, 2007, 2015))
    df_5 = df_all[(df_all['Building_Number'].isin(ai_set)) & (df_all['eui_gas'] >= 3)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_5['intensity'] = df_5['Total_kBtu']/df_5['Gross_Sq.Ft']
    print df_5[['intensity', 'Building_Number']]
    print 'good_both'
    df6 = df_eui[(df_eui['Building_Number'].isin(ai_set)) & 
                 (df_eui['eui_gas'] >= 3) & (df_eui['eui_elec'] >= 12)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'eui': 'mean'})
    print df6
    print 'at least 6 years', len(year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))
    df_study = pd.DataFrame({'Building_Number': (year_of_data(df_eui[(df_eui['Building_Number'].isin(ai_set)) & (df_eui['eui_gas'] >= 3) & (df_eui['eui_elec'] >= 12)], 6, 2007, 2015))})
    df_study.to_csv(homedir + 'temp/to_check_square_footage.csv', index=False)
    df_6 = df_all[(df_all['Building_Number'].isin(ai_set)) & (df_all['eui_elec'] >= 12) & (df_all['eui_gas'] >= 3)].groupby('Fiscal_Year').agg({'Building_Number': 'count', 'Gross_Sq.Ft': 'sum', 'Total_kBtu': 'sum'})
    df_6['intensity'] = df_6['Total_kBtu']/df_6['Gross_Sq.Ft']
    print df_6[['intensity', 'Building_Number']]
    return

def plot_trend_ave(theme, summary_step):
    sns.set_style("whitegrid")
    # colors = sns.color_palette("Blues", 5)
    # colors2 = colors + list(reversed(colors)) 
    blues = sns.color_palette("Blues", 5)
    colors2= [blues[0], blues[1], "red", blues[1], blues[0]]
    sns.set_palette(sns.color_palette(colors2))
    sns.set_context("talk", font_scale=1)
    conn = uo.connect('all')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    covered_set = gbs.get_covered_set()
    energy_set = gbs.get_energy_set('eui')
    elec_set = gbs.get_energy_set('eui_elec')
    gas_set = gbs.get_energy_set('eui_gas')
    study_set = elec_set.intersection(covered_set.intersection(ai_set))
    with conn:
        df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy', conn)
    print len(df_energy)
    df_energy = df_energy[df_energy['Building_Number'].isin(study_set)]
    print len(df_energy)
    num = len(set(df_energy['Building_Number']))
    df_energy = df_energy[['Building_Number', 'Fiscal_Year', theme]]
    df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['Fiscal_Year']), 1, 1), axis=1)
    quantiles = np.arange(0, 1.25, 0.25)
    print quantiles
    lines = []
    labels = ['{0}%'.format(x) for x in range(0, 125, 25)]
    bx = plt.axes()
    ys = []
    for q in quantiles:
        df = df_energy.groupby(['Date']).quantile(q)
        df.reset_index(inplace=True)
        df.set_index('Date', inplace=True)
        ys.append(df[theme])
        if q == 0.5:
            # line, = plt.plot(df.index, df[theme], lw=2)
            df_temp = df_energy.groupby(['Date']).mean()
            line, = plt.plot(df_temp.index, df[theme], lw=2)
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
    plt.title('{0} Trend: A + I Covered Facility (n = {1})'.format(lb.title_dict[theme], num))
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('Building {0}'.format(eng_str))
    plt.xlabel('Fiscal Year')
    plt.ylabel(lb.ylabel_dict[theme])
    plt.ylim((0, 200))
    plt.xlim((datetime.datetime(2003, 1, 1),
              datetime.datetime(2015, 1, 2)))
    plt.show()
    # P.savefig(os.getcwd() + '/plot_FY_annual/quant/fan_{0}_{1}.png'.format(theme, summary_step), dpi = 150)
    plt.close()

def plot_box_sets(theme, summary_step):
    conn = uo.connect('all')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    energy_set = gbs.get_energy_set('eui')
    study_set = energy_set.intersection(ai_set)
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    # df['Have any investment'] = df['Building_Number'].map(lambda x: 'With Investment' if x in cap_or_op else 'No Known Investment')
    ecm_set = gbs.get_ecm_set()
    covered_set = gbs.get_covered_set()
    with conn:
        df = pd.read_sql('SELECT Building_Number, {0}, Fiscal_Year FROM eui_by_fy'.format(theme), conn)
    def n_and_median_reduct(df):
        before = df[df['Fiscal_Year'] == 2003][theme].median()
        after = df[df['Fiscal_Year'] == 2015][theme].median()
        return len(df[df['Fiscal_Year'] == 2003]), round((after - before)/before*100, 0)
    df['Fiscal_Year'] = df['Fiscal_Year'].map(int)
    df = df[df['Fiscal_Year'].isin([2003, 2015])]
    df = df[df['Building_Number'].isin(study_set)]
    print len(df)
    df = df.groupby('Building_Number').filter(lambda x: len(x) == 2)
    print len(df)
    # df_ecm = df[df['Building_Number'].isin(ecm_set)]
    df_ecm = df[df['Building_Number'].map(lambda x: x in cap_or_op)]
    # print len(df_ecm), '11111111'
    n_ecm, percent = n_and_median_reduct(df_ecm)
    df_ecm['Status'] = 'With Energy Investment\nn = {0}\n{1}%'.format(n_ecm, percent)
    df_woutecm = df[~df['Building_Number'].isin(cap_or_op)]
    n, percent = n_and_median_reduct(df_woutecm)
    df_woutecm['Status'] = 'Without Energy Investment\nn = {0}\n{1}%'.format(n, percent)
    df_covered = df[df['Building_Number'].isin(covered_set)]
    n, percent = n_and_median_reduct(df_covered)
    df_covered['Status'] = 'Covered Facility\nn = {0} (of the {2})\n{1}%'.format(n, percent, n_ecm)
    df_eng = df.copy()
    df_eng['Status'] = 'All'
    n, percent = n_and_median_reduct(df_eng)
    df_eng['Status'] = 'All\nn = {0}\n{1}%'.format(n, percent)
    df_all = pd.concat([df_eng, df_ecm, df_woutecm, df_covered], ignore_index=True)
    print df_all.head()
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=0.9)
    sns.set_palette(sns.color_palette('Set2'))
    sns.boxplot(x='Status', y=theme, hue='Fiscal_Year', data=df_all, fliersize=0)
    plt.title('EUI compare: FY2003 vs FY2015')
    eng_str = '\n'.join(tw.wrap('appearing in both FY2003 and FY2015, with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.subplots_adjust(top=0.85)
    plt.suptitle('A + I Building ' + eng_str)
    plt.ylim((0, 250))
    plt.ylabel(lb.ylabel_dict[theme])
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/eui_box_set.png'.format(theme, summary_step), dpi = 150)
    plt.close()

def plot_median_trend_perdd(theme, summary_step, anno=False, agg='median'):
    conn = uo.connect('all')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    energy_set = gbs.get_energy_set('eui')
    study_set = energy_set.intersection(ai_set)
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    covered_set = gbs.get_covered_set()
    no_invest = gbs.get_no_invest_set()
    conn = uo.connect('all')
    with conn:
        if summary_step == 'Y':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, {0} FROM eui_by_fy_weather WHERE Fiscal_Year < 2016'.format(theme), conn)
        # M is not right
        elif summary_step == 'M':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, year, month, eui_elec, eui_gas, eui FROM EUAS_monthly_weather WHERE Fiscal_Year < 2016', conn)
    df_energy = df_energy[df_energy['Building_Number'].isin(study_set)]
    print len(df_energy)
    num = len(set(df_energy['Building_Number']))
    if summary_step == 'M':
        df_energy = df_energy[['Building_Number', 'year', 'month', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1), axis=1)
    elif summary_step == 'Y':
        df_energy = df_energy[['Building_Number', 'Fiscal_Year', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['Fiscal_Year']), 1, 1), axis=1)
    dfs = []
    labels = ['Capital_Only', 'Operational_Only',
              'Capital_and_Operational', 'No_Known_Investment', 
              'A + I', 'Covered A + I']
    sets = [cap_only, op_only, cap_and_op, no_invest, study_set,
            energy_set.intersection(covered_set)]
    for x, l in zip(sets, labels):
        df_cap = df_energy[df_energy['Building_Number'].isin(x)]
        df = df_cap.groupby(['Date']).agg({'Building_Number': 'count', theme: agg, 'Fiscal_Year': 'first'})
        # df = df_cap.groupby(['Date']).median()
        df['status'] = l
        dfs.append(df)
        plt.plot(df.index, df[theme])
    pd.concat(dfs, ignore_index=True).to_csv(r_input + \
                                             'all_{0}_trend_perdd.csv'.format(agg))
    plt.show()
    return

def plot_median_trend(theme, summary_step, anno=False, agg='median'):
    conn = uo.connect('all')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    energy_set = gbs.get_energy_set('eui')
    study_set = energy_set.intersection(ai_set)
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    covered_set = gbs.get_covered_set()
    no_invest = gbs.get_no_invest_set()
    conn = uo.connect('all')
    with conn:
        if summary_step == 'Y':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy WHERE Fiscal_Year < 2016', conn)
        elif summary_step == 'M':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, year, month, eui_elec, eui_gas, eui FROM EUAS_monthly WHERE Fiscal_Year < 2016', conn)
    df_energy = df_energy[df_energy['Building_Number'].isin(study_set)]
    print len(df_energy)
    num = len(set(df_energy['Building_Number']))
    if summary_step == 'M':
        df_energy = df_energy[['Building_Number', 'year', 'month', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1), axis=1)
    elif summary_step == 'Y':
        df_energy = df_energy[['Building_Number', 'Fiscal_Year', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['Fiscal_Year']), 1, 1), axis=1)
    dfs = []
    labels = ['Capital_Only', 'Operational_Only',
              'Capital_and_Operational', 'No_Known_Investment', 
              'A + I', 'Covered A + I']
    sets = [cap_only, op_only, cap_and_op, no_invest, study_set,
            energy_set.intersection(covered_set)]
    for x, l in zip(sets, labels):
        df_cap = df_energy[df_energy['Building_Number'].isin(x)]
        # df = df_cap.groupby(['Date']).median()
        df = df_cap.groupby(['Date']).agg({'Building_Number': 'count', theme: agg, 'Fiscal_Year': 'first'})
        df['status'] = l
        dfs.append(df)
        plt.plot(df.index, df[theme])
    pd.concat(dfs, ignore_index=True).to_csv(r_input + \
                                             'all_{0}_trend.csv'.format(agg))
    plt.show()
    return

def plot_trend_fan_sets(theme, summary_step, anno=False):
    conn = uo.connect('all')
    ai_set = gbs.get_cat_set(['A', 'I'], conn)
    energy_set = gbs.get_energy_set('eui')
    study_set = energy_set.intersection(ai_set)
    (cap_only, op_only, cap_and_op, cap_or_op) = gbs.get_invest_set()
    covered_set = gbs.get_covered_set()
    no_invest = gbs.get_no_invest_set()
    quantiles = np.arange(0.1, 1.0, 0.1)
    plot_trend_fan('eui', summary_step, study_set, '', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(cap_or_op), 'With Investment', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(covered_set), 'Covered Facility', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(cap_only), 'Capital Only', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(op_only), 'Operational Only', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(cap_and_op), 'Capital and Operational', quantiles, anno=anno)
    plot_trend_fan('eui', summary_step, study_set.intersection(no_invest), 'No Known Investment', quantiles, anno=anno)
    return

def plot_fan(df_energy, ycol, alpha=0.7, palette="Blues", median_color="red", quantiles=np.arange(0, 1.1, 0.10), anno=False):
    print df_energy[df_energy[ycol] > 500]
    length = len(quantiles) 
    half = length/2
    blues = sns.color_palette(palette, half)
    colors2= blues[:half] + [median_color] + blues[:half][::-1]
    sns.set_palette(sns.color_palette(colors2))
    sns.set_context("talk", font_scale=1)
    print quantiles
    lines = []
    labels = ['{0:.0%}'.format(x) for x in quantiles]
    bx = plt.axes()
    ys = []
    for q in quantiles:
        df = df_energy.groupby(['Date']).quantile(q)
        df.reset_index(inplace=True)
        df.set_index('Date', inplace=True)
        ys.append(df[ycol])
        if q == 0.5:
            line, = plt.plot(df.index, df[ycol], lw=2)
            if anno:
                for x, y in zip(df.index, df[ycol]):
                    print x, y
                    plt.annotate('{:,.1f}'.format(y), xy = (x, y), fontsize=12, weight='semibold', color='black')
            df_median = df
        else:
            line, = plt.plot(df.index, df[ycol], lw=1)
        lines.append(line)
        idx = df.index
    for i in range(half):
        plt.fill_between(idx, ys[i], ys[i + 1], facecolor=blues[i], alpha=alpha)
        plt.fill_between(idx, ys[length - (i + 1) - 1], ys[length - i - 1], facecolor=blues[i], alpha=alpha)
    df_median.reset_index(inplace=True)
    plt.legend(list(reversed(lines)), list(reversed(labels)),
               loc='center left', bbox_to_anchor=(1, 0.5),
               prop={'size':10})

# anno: whether to annotate each point
def plot_trend_fan(theme, summary_step, study_set, set_label, quantiles=None, anno=False, ax=None):
    conn = uo.connect('all')
    with conn:
        if summary_step == 'Y':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_elec, eui_gas, eui FROM eui_by_fy WHERE Fiscal_Year < 2016', conn)
        elif summary_step == 'M':
            df_energy= pd.read_sql('SELECT Building_Number, Fiscal_Year, year, month, eui_elec, eui_gas, eui FROM EUAS_monthly WHERE Fiscal_Year < 2016', conn)
    print len(df_energy)
    df_energy = df_energy[df_energy['Building_Number'].isin(study_set)]
    print len(df_energy)
    num = len(set(df_energy['Building_Number']))
    if summary_step == 'M':
        df_energy = df_energy[['Building_Number', 'year', 'month', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['year']), int(r['month']), 1), axis=1)
    elif summary_step == 'Y':
        df_energy = df_energy[['Building_Number', 'Fiscal_Year', theme]]
        df_energy['Date'] = df_energy.apply(lambda r: datetime.datetime(int(r['Fiscal_Year']), 1, 1), axis=1)

    if quantiles is None:
        plot_fan(df_energy, theme, median_color="red", anno=anno)
    else:
        plot_fan(df_energy, theme, median_color="red", quantiles=quantiles, anno=anno)

    plt.title('{0} Trend: A + I, {1} (n = {2})'.format(lb.title_dict[theme], set_label, num))
    eng_str = '\n'.join(tw.wrap('with at least 6 years of Electric EUI >= 12 and Gas EUI >= 3 from FY2007 to FY2015', 50))
    plt.suptitle('Building {0}'.format(eng_str))
    if summary_step == 'Y':
        plt.xlabel('Fiscal Year')
        plt.ylim((0, 150))
    elif summary_step == 'M':
        plt.xlabel('Date')
    plt.ylabel(lb.ylabel_dict[theme])
    plt.xlim((datetime.datetime(2003, 1, 1),
              datetime.datetime(2015, 1, 2)))
    # plt.show()
    P.savefig(os.getcwd() + '/plot_FY_annual/quant/fan_{0}_{1}_{2}.png'.format(theme, summary_step, set_label), dpi = 150)
    plt.close()
    
def plot_trend_all_possible():
    counter = 0
    for s in ['All', 'ACI', 'AI']:
        for t in ['all_type', 'elec_gas']:
            for energy_filter in [None, 'eui']:
                counter += 1
                print counter
                plot_eui_trend_total_total(s, t, energy_filter)
                plot_eui_trend_simple_mean(s, t, energy_filter)
                plot_eui_trend_weighted_mean(s, t, energy_filter)
    return
    
def plot_scatter():
    conn = uo.connect('all')
    with conn:
        df_eng = pd.read_sql('SELECT Building_Number, Fiscal_Year, [Electric_(kBtu)], [Gas_(kBtu)], [Oil_(kBtu)], [Steam_(kBtu)] FROM EUAS_monthly', conn)
        df_area = pd.read_sql('SELECT * FROM EUAS_area', conn)
    conn.close()
    df_eng2 = df_eng.groupby(['Building_Number', 'Fiscal_Year']).sum()
    df_eng2.reset_index(inplace=True)
    # print df_eng2.head()
    df = pd.merge(df_eng2, df_area, on=['Building_Number', 'Fiscal_Year'], how='inner')
    df['Fiscal_Year'] = df['Fiscal_Year'].map(int)
    df = df[df['Fiscal_Year'] != 2016]
    df['Gross_Sq.Ft (Million)'] = df['Gross_Sq.Ft'] * 1e-6
    for x in ['Electric_(kBtu)', 'Gas_(kBtu)', 'Oil_(kBtu)',
              'Steam_(kBtu)']:
        ytitle = x.replace('kBtu', 'MMBtu')
        df[ytitle] = df[x] * 1e-3
        sns.lmplot(x='Gross_Sq.Ft (Million)', y=ytitle,
                hue='Fiscal_Year', col='Fiscal_Year', col_wrap=5,
                fit_reg=False, data=df, size=3)
        plt.gca().set_ylim(bottom=0)
        plt.gca().set_xlim(left=0)
        P.savefig(os.getcwd() + '/plot_FY_annual/quant/{0}_area.png'.format(ytitle), dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
        plt.close()

def process_html_area(b, unit, ylimit):
    with open(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/template_area.html', 'r') as rd:
        lines = rd.readlines()
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("building_number", b)
        lines[i] = lines[i].replace("column", "Gross_Sq.Ft")
        lines[i] = lines[i].replace("unit", unit)
        lines[i] = lines[i].replace("valueRange: [0, max]", "valueRange: [0, {0}]".format(ylimit))
    with open(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/{1}/{0}.html'.format(b, "Gross_Sq_Ft"), 'w+') as wt:
        wt.write(''.join(lines))
        
def trend_single_building_area():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_area', conn)
    df.rename(columns=lambda x: x.replace('.', '_'), inplace=True)
    df.info()
    gr = df.groupby('Building_Number')
    for name, group in list(gr):
        print name
        ylimit = group['Gross_Sq_Ft'].max(axis=1) * 1.2
        out = group.copy()
        out['Date'] = out['Fiscal_Year'].map(lambda x: '{0}0101'.format(int(x)))
        out = out[['Date', 'Gross_Sq_Ft']]
        out.to_csv(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/{1}/{0}.csv'.format(name, 'Gross_Sq_Ft'), index=False)
        process_html_area(name, 'sq.ft', ylimit)

def process_html_trend_single(b, col, unit, years, ylimit):
    colors = sns.color_palette("Spectral", len(years))
    pal_str = util.pal2rgb(colors)
    with open(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/template.html', 'r') as rd:
        lines = rd.readlines()
    with open(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/checkbox.html', 'r') as rd:
        checklines = rd.readlines()
    def substitute_year(string, i, year):
        string = string.replace('0', str(i))
        string = string.replace('year', str(int(year)))
        return string
    for i, line in enumerate(lines):
        lines[i] = lines[i].replace("building_number", b)
        lines[i] = lines[i].replace("column", col)
        lines[i] = lines[i].replace("unit", unit)
        lines[i] = lines[i].replace("colors: []", "colors: {0}".format(pal_str))
        lines[i] = lines[i].replace("valueRange: [0, max]", "valueRange: [0, {0}]".format(ylimit))
        lines[i] = lines[i].replace("visibility: []", "visibility: " + str(["true"] * len(years)))
        if '<!-- checkbox here -->' in lines[i]:
            newlines = []
            for j, y in enumerate(years):
                newlines.append(substitute_year(''.join(checklines),
                                                j, y))
            lines[i] = ''.join(newlines)
    with open(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/{1}/{0}.html'.format(b, col), 'w+') as wt:
        wt.write(''.join(lines))
    return

def trend_single_building_all():
    raise NotImplemented
    return

def trend_single_building_monthly(col, unit):
    infile = os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/dygraph-combined-dev.js'
    outfile = infile.replace('dynamic_trend', 'dynamic_trend/' + col)
    shutil.copyfile(infile, outfile)
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, year, ' +
            'month, [{0}] FROM EUAS_monthly_weather'.format(col), conn)
    df['year'] = df['year'].map(int)
    df = df[df['year'] != 2016]
    gr = df.groupby('Building_Number')
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1.0)
    sns.set_palette(sns.cubehelix_palette(14))
    for i, (name, group) in enumerate(list(gr)):
        print i, name
        temp = group[['year', 'month', col]]
        ylimit = group[col].max(axis=1) * 1.2
        pv = temp.pivot(index='month', columns='year', values=col)
        pv.reset_index(inplace=True)
        pv.rename(columns={'month': 'Date'}, inplace=True)
        years = list(pv)[1:]
        pv.to_csv(os.getcwd() + '/plot_FY_weather/html/single_building/dynamic_trend/{1}/{0}.csv'.format(name, col), index=False)
        process_html_trend_single(name, col, unit, years, ylimit)
    
def building_programstr():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT Building_Number, ECM_program FROM EUAS_ecm_program', conn)
    conn.close()
    df = df[df['ECM_program'].notnull()]
    agg = df.groupby('Building_Number').agg({'ECM_program': lambda x: ';'.join(set(x))})
    agg.reset_index(inplace=True)
    return dict(zip(agg['Building_Number'], agg['ECM_program']))

# BOOKMARK
def get_ecm_analysis_group_count():
    conn = uo.connect('all')
    study_set = gbs.get_energy_set('eui').intersection(gbs.get_cat_set(['A', 'I'], conn))
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
        df_high = pd.read_sql('SELECT DISTINCT Building_Number, high_level_ECM FROM EUAS_ecm', conn)
        df_detail = pd.read_sql('SELECT DISTINCT Building_Number, detail_level_ECM FROM EUAS_ecm', conn)
    df_high = df_high[df_high['Building_Number'].isin(study_set)]
    df_one_high = df_high.groupby('Building_Number').filter(lambda x: len(x) == 1)
    print len(df_one_high)
    print df_one_high['high_level_ECM'].value_counts()
    # df_two_high = df_high.groupby('Building_Number').filter(lambda x: len(x) == 2)
    # print df_two_high['high_level_ECM'].value_counts()
    # df_detail = df_detail[df_detail['Building_Number'].isin(study_set)]
    # df_one_detail = df_detail.groupby('Building_Number').filter(lambda x: len(x) == 1)
    # print len(df_one_detail)
    # print df_one_detail['detail_level_ECM'].value_counts()
    return

def create_index(eng_filter='eui'):
    conn = uo.connect('all')
    study_set = gbs.get_energy_set(eng_filter).intersection(gbs.get_cat_set(['A', 'I'], conn))
    df = pd.DataFrame({'Building_Number': list(study_set)})
    outdir = os.getcwd() + '/plot_FY_weather/html/'
    df['Electric (kBtu) Trend'] = \
        df['Building_Number'].map(lambda x: x + '_Electric_(kBtu)')
    df['Gas (kBtu) Trend'] = \
        df['Building_Number'].map(lambda x: x + '_Gas_(kBtu)')
    df['Gross Sq_Ft Trend'] = \
        df['Building_Number'].map(lambda x: x + '_Gross_Sq_Ft')
    df['HDD 65F Trend'] = \
        df['Building_Number'].map(lambda x: x + '_hdd65')
    df['CDD 65F Trend'] = \
        df['Building_Number'].map(lambda x: x + '_cdd65')
    df['ECM Action Saving'] = \
        df['Building_Number'].map(lambda x: x + '_saving')
    dict_pro = building_programstr()
    df['Energy Program'] = df['Building_Number'].map(lambda x: dict_pro[x] if x in dict_pro else 'No_program')
    action_files = glob.glob(os.getcwd() + '/plot_FY_weather/html/single_building/*.html')
    action_buildings = [x[x.rfind('/') + 1: -5] for x in action_files]

    with conn:
        df_area = pd.read_sql('SELECT DISTINCT Building_Number, [Gross_Sq.Ft] FROM EUAS_monthly', conn)
    df_cnt = df_area.groupby('Building_Number').count()[['Gross_Sq.Ft']]
    df_cnt.reset_index(inplace=True)
    df_cnt.rename(columns={'Gross_Sq.Ft': 'Change sqft'}, inplace=True)
    df_cnt['Change sqft'] = df_cnt['Change sqft'].map(lambda x: x > 1)
    print df_cnt.head()
    df_all = pd.merge(df, df_cnt, on='Building_Number', how='left')
    print len(df_all[df_all['Change sqft']])
    print len(df_all)
    df_all.sort('Building_Number', inplace=True)
    print df_all.head()
    df_all.to_csv(homedir + 'temp/good_buildings.csv')
    def replace_line(line):
        idx_elec = line.find('_Electric_(kBtu)')
        idx_gas = line.find('_Gas_(kBtu)')
        idx_sqft = line.find('_Gross_Sq_Ft')
        idx_cdd = line.find('_cdd65')
        idx_hdd = line.find('_hdd65')
        idx_act = line.find('_saving')
        if idx_elec > 0:
            b = line[idx_elec - 8: idx_elec]
            return '<td><a href=single_building/dynamic_trend/{0}/{1}.html>{1}_{0}</a></td>'.format('Electric_(kBtu)', b)
        elif idx_gas > 0:
            b = line[idx_gas - 8: idx_gas]
            return '<td><a href=single_building/dynamic_trend/{0}/{1}.html>{1}_{0}</a></td>'.format('Gas_(kBtu)', b)
        elif idx_sqft > 0:
            b = line[idx_sqft - 8: idx_sqft]
            return '<td><a href=single_building/dynamic_trend/{0}/{1}.html>{1}_{0}</a></td>'.format('Gross_Sq_Ft', b)
        elif idx_cdd > 0:
            b = line[idx_cdd - 8: idx_cdd]
            return '<td><a href=single_building/dynamic_trend/{0}/{1}.html>{1}_{0}</a></td>'.format('cdd65', b)
        elif idx_hdd > 0:
            b = line[idx_hdd - 8: idx_hdd]
            return '<td><a href=single_building/dynamic_trend/{0}/{1}.html>{1}_{0}</a></td>'.format('hdd65', b)
        elif idx_act > 0:
            b = line[idx_act - 8: idx_act]
            if b in action_buildings:
                return '<td><a href=single_building/{0}.html>{0}_action</a></td>'.format(b)
            else:
                return '<td>No_action</td>'.format(b)
        else:
            return line
    if eng_filter == 'eui':
        tablefile = outdir + 'robust_energy.html'
        linkfile = 'robust_energy_link.html'
    elif eng_filter == 'eui_elec':
        tablefile = outdir + 'robust_electric.html'
        linkfile = 'robust_electric_link.html'
    with open(tablefile, 'w+') as wt:
        df_all.reset_index(inplace=True)
        df_all.drop('index', axis=1, inplace=True)
        df_all.to_html(wt, index=True, justify='left')
    with open(tablefile, 'r') as rd:
        lines = rd.readlines()
    for i, line in enumerate(lines):
        lines[i] = replace_line(lines[i])
    if eng_filter == 'eui':
        lines.insert(0, '<h1>Energy Trend of A + I Buildings</h1> <h2>with at least 6 years of Electric EUI >= 12 kBtu/sq.ft/year and Gas >= 3 kBtu/sq.ft/year</h2>')
    elif eng_filter == 'eui_elec':
        lines.insert(0, '<h1>Energy Trend of A + I Buildings</h1> <h2>with at least 6 years of Electric EUI >= 12 kBtu/sq.ft/year</h2>')
    with open(outdir + linkfile, 'w+') as wt:
        wt.write(''.join(lines))
    print 'end'
    return

def check_0706():
    get_ecm_analysis_group_count()
    return
    
def get_one_action():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
        df_high = pd.read_sql('SELECT DISTINCT Building_Number, high_level_ECM FROM EUAS_ecm', conn)
        df_detail = pd.read_sql('SELECT DISTINCT Building_Number, detail_level_ECM FROM EUAS_ecm', conn)
    df_one_high = df_high.groupby('Building_Number').filter(lambda x: len(x) == 1)
    print len(df_one_high)
    df_out = df_one_high.dropna(subset=['high_level_ECM'])
    print len(df_out)
    return df_out

def code_0712():
    # study_set = gbs.get_energy_set('eui').intersection(gbs.get_cat_set(['A', 'I'], conn))
    df_save = pd.read_csv(os.getcwd() + '/plot_FY_weather/html/table/action_saving_robustset.csv')
    df_save.info()
    print df_save.head()
    # key_set = df_save['Building_Number'].unique()
    # df = get_one_action()
    # df = df[df['Building_Number'].isin(key_set)]
    # print df.groupby('high_level_ECM').count()
    # df_all = pd.merge(df, df_save, on='Building_Number', how='left')
    # df_all.groupby('high_level_ECM').mean().to_csv(homedir + 'temp/save_ECM_one_high.csv', index=False)
    return
    
def dynamic_trend():
    # trend_single_building_area()
    # not good
    # trend_single_building_all()
    # trend_single_building_monthly('hdd65', 'F')
    # trend_single_building_monthly('cdd65', 'F')
    # trend_single_building_monthly('Electric_(kBtu)', 'kBtu')
    # trend_single_building_monthly('Gas_(kBtu)', 'kBtu')
    # create_index('eui')
    create_index('elec')
    return
    
def filter_summary(theme):
    df = pd.read_csv(os.getcwd() + '/plot_FY_weather/html/table/action_saving.csv')
    # df.info()
    def get_highlevel(string):
        tokens = string.split(';')
        high_tokens = [x[:x.find(' -')] for x in tokens]
        result = list(set(high_tokens))
        return ';'.join(sorted(result))
    df['high_level_ecm'] = df['Action'].map(get_highlevel)
    df.drop_duplicates(cols=['Building_Number', 'Time',
                             'high_level_ecm'], inplace=True)
    if theme == 'elec':
        df = df[['Building_Number', 'Time', 'high_level_ecm',
                 'Electric_Saving', 'Electric_Before',
                 'Electric_After', 'Electric_CVRMSE']]
        df = df[df['Electric_Saving'] != 'None']
        df = df[df['Electric_CVRMSE'].map(float) < 0.35]
        df['Electric_abs'] = df['Electric_Before'].map(float) - \
            df['Electric_After'].map(float)
    elif theme == 'gas':
        df = df[['Building_Number', 'Time', 'high_level_ecm',
                 'Gas_Before', 'Gas_After', 'Gas_Saving',
                 'Gas_CVRMSE']]
        df = df[df['Gas_Saving'] != 'None']
        df = df[df['Gas_CVRMSE'].map(float) < 0.35]
        df['Gas_abs'] = df['Gas_Before'].map(float) - \
            df['Gas_After'].map(float)
    set_eng = gbs.get_energy_set(theme)
    df = df[df['Building_Number'].isin(set_eng)]
    df['number'] = df['high_level_ecm'].map(lambda x:
                                            len(x.split(';')))
    df['high_level_ecm'] = df['high_level_ecm'].map(lambda x: x.replace("Building Tuneup or Utility Improvements", "Commissioning"))
    return df

def lean_summary(theme):
    df = filter_summary(theme)
    print len(df)
    df = df.groupby('Building_Number').filter(lambda x: len(x) < 2)
    df.to_csv(r_input + '{0}_action_save.csv'.format(theme),
              index=False)
    print 'end'
    print len(df)
    # lst = df['high_level_ecm'].unique()
    # for x in lst:
    #     print x
    # df['number'] = df['high_level_ecm'].map(lambda x:
    #                                         len(x.split(';')))
    # print 'number of action, number of building'
    # print df['number'].value_counts()
    # print 'action, number of building'
    # # print df.groupby('high_level_ecm').agg()
    # print 'For buildings with solo action:'
    # print df[df['number'] == 1]['high_level_ecm'].value_counts()
    return

def main():
    # box_cap_op('eui', 'AI', 'long', [2003], [2014, 2015])
    # lean_summary('gas')
    # lean_summary('elec')
    # plot_median_trend_perdd('eui_perdd', 'Y', anno=True, agg='mean')
    # plot_median_trend('eui', 'Y', anno=True, agg='mean')
    # create_index('eui_elec')
    # create_index('eui')
    # code_0712()
    # check_0706()
    # This is the closest to the PNNL study
    # plot_eui_trend_total_total('ACI', 'all_type', None, True)
    # plot_eui_trend_total_total('AI', 'elec_gas', 'eui', False)
    # plot_eui_trend_total_total('select', 'elec_gas', None, False)
    # area_distribution()
    # cat_distribution()
    # eui_distribution()
    # plot_scatter()
    # plot_trend_fan_sets('eui', 'M', anno=False)
    # plot_trend_fan_sets('eui', 'Y', anno=True)
    # plot_trend_fan_sets('eui', 'Y')
    # plot_box_sets('eui', 'Y')
    # plot_trend_fan('eui', 'Y')
    # plot_trend_ave('eui', 'Y')
    # temp()
    # flow_chart()
    # plot_trend_all_possible()
    # energy_set = gbs.get_energy_set('eui')
    # ai_set = gbs.get_cat_set(['A', 'I'])
    # print len(gbs.get_energy_set('none').intersection(ai_set)), 'no restrict'
    # print len(gbs.get_energy_set('eui').intersection(ai_set)), 'eui'
    # print len(gbs.get_energy_set('eui_elec').intersection(ai_set)), 'elec'
    # print len(gbs.get_energy_set('eui_gas').intersection(ai_set)), 'gas'
    # get_stat('AI', None)
    # get_stat('AI', energy_set)
    # building_count_plot('AI')
    # building_count_plot('ACI')
    # building_count_plot('All')
    # building_count_plot('AI')
    # building_count_plot('ECM')
    # building_count_plot('AIECM')
    # building_count_plot('AllInvest')
    # building_count_plot('AIInvest')
    # good_elec_gas()
    # agg_yearlist = range(2003, 2017)
    # fuel_type_plot(agg_yearlist, 'AI')
    # fuel_type_plot(agg_yearlist, 'All')
    # plot_type('All')
    # plot_type('AI')
    # plot_co_count('All')
    # plot_co_count('AI')
    # plot_co_count('All', energy_set=gbs.get_energy_set('eui'))
    # plot_co_count('AI', energy_set=gbs.get_energy_set('eui'))
    # plot_c_or_o('All', 'Capital', energy_set)
    # plot_c_or_o('All', 'Operational', energy_set)
    # plot_c_or_o('AI', 'Capital', energy_set)
    # plot_c_or_o('AI', 'Operational', energy_set)
    # for theme in ['eui', 'eui_elec', 'eui_gas']:
    # for theme in ['eui']:
    #     for plot_set in ['AI']:
    #     # for plot_set in ['All', 'AI']:
    #         plot_co_boxtrend(plot_set, theme, 140, 'ori')
            # plot_co_boxtrend(plot_set, theme, 140, 'ave')
            # plot_co_type_boxtrend(plot_set, theme, 140, 'ori')
            # plot_co_type_boxtrend(plot_set, theme, 140, 'ave')
    # ecm_time_dist('All')
    # ecm_time_dist('AI')
    # test_hypo('eui', 'All')
    # test_hypo_absolute('eui', 'AIcovered', 'long', [2003], [2014, 2015])
    test_hypo_absolute('eui', 'AI', 'long', [2003], [2014, 2015])
    # test_hypo_absolute('eui', 'All', 'long', [2003], [2014, 2015])
    # bar_plot_before_after()
    return
    
main()
