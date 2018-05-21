import pandas as pd
import os
import sqlite3
homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/'
    
def get_650_set(conn):
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn)
        df2 = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    df = df[df['Fiscal_Year'] > 2006]
    df = df[df['Fiscal_Year'] < 2016]
    df3 = df.groupby('Building_Number').filter(lambda x: len(x) > 5)
    # print 'all building > 5 years of data: {0}'.format(df3['Building_Number'].nunique())
    df4 = df[df['Cat'].isin(['A', 'I'])].groupby('Building_Number').filter(lambda x: len(x) > 5)
    # print 'A + I building > 5 years of data: {0}'.format(df4['Building_Number'].nunique())
    return set(df4['Building_Number'].tolist())

def get_covered_set():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number FROM covered_facility', conn)
    return set(df['Building_Number'].tolist())
    
def get_ecm_set():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
    # df = pd.read_csv(master_dir + 'ECM/EUAS_ecm.csv')
    df = df[df['high_level_ECM'].notnull()]
    return set(df['Building_Number'].tolist())
    
def get_action_set(col, lst):
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
    df = df[df[col].isin(lst)]
    return set(df['Building_Number'].tolist())

def get_ecm_highlevel():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
    df = df[df['high_level_ECM'].notnull()]
    df = df[df['high_level_ECM'] != 'GSALink']
    return set(df['Building_Number'].tolist())

def get_all_building_set():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_category', conn)
    # df = pd.read_csv(master_dir + 'EUAS_static_tidy.csv')
    result = set(df['Building_Number'].tolist())
    return result

def get_cat_set(cat_list, conn):
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, Cat FROM EUAS_category', conn)
        df = df[df['Cat'].isin(cat_list)]
    return set(df['Building_Number'].tolist())

# BOOKMARK
def get_energy_set(theme):
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM eui_by_fy', conn)
    # df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    df = df[df['Fiscal_Year'] > 2006]
    df = df[df['Fiscal_Year'] < 2016]
    if theme == 'eui':
        df['good'] = df.apply(lambda r: 1 if (r['eui_elec'] >= 12 and
                                              r['eui_gas'] >= 3) else 0,
                              axis=1)
    elif theme == 'eui_gas':
        df['good'] = df.apply(lambda r: 1 if r['eui_gas'] >= 3 and
                              r['eui_elec'] < 12 else 0, axis=1)
    elif theme == 'gas':
        df['good'] = df.apply(lambda r: 1 if r['eui_gas'] >= 3 else 0,
                              axis=1)
    elif theme == 'eui_elec':
        df['good'] = df.apply(lambda r: 1 if r['eui_elec'] >= 12 and
                              r['eui_gas'] < 3 else 0, axis=1)
    elif theme == 'elec':
        df['good'] = df.apply(lambda r: 1 if r['eui_elec'] >= 12 else
                              0, axis=1)
    elif theme is None:
        df['good'] = 1
    df = df[['Building_Number', 'good']]
    df2 = df.groupby('Building_Number').sum()
    df2 = df2[df2['good'] > 5]
    df2.reset_index(inplace=True)
    if conn:
        conn.close()
    return set(df2['Building_Number'].tolist())

def get_study_set():
    conn = sqlite3.connect(homedir + 'db/all.db')
    eng = get_energy_set('eui')
    ai_set = get_cat_set(['A', 'I'], conn)
    return eng.intersection(ai_set)

def get_program_set(lst):
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_ecm_program', conn)
    df = df[df['ECM_program'].isin(lst)]
    return set(df['Building_Number'].tolist())
    
# programs = ['E4', 'Shave Energy', 'GSALink', 'first fuel', 'LEED_EB', 'GP', 'LEED_NC']
# print
# for p in programs:
#     print len(get_program_set([p])), p 
# actions = ['Advanced Metering', 'Building Tuneup or Utility Improvements', 'Building Envelope', 'Lighting', 'HVAC']
# print
# for a in actions:
#     print len(get_action_set('high_level_ECM', [a])), a 

def get_co_set(co):
    ss = []
    if co == 'Capital':
        ss.append(get_program_set(['LEED_NC']))
        ss.append(get_action_set('high_level_ECM', 
                  ['Lighting', 'HVAC', 'Building Envelope', 
                   'Building Tuneup or Utility Improvements']).difference(set(['DC0031ZZ'])))
    elif co == 'Operational':
        # First Fuel, Shave Energy, E4, LEED_EB
        ss.append(get_program_set(['first fuel', 'Shave Energy', 'E4',
                                   'LEED_EB', 'GP']))
        # GSALink, advanced metering
        ss.append(get_action_set('high_level_ECM', ['GSALink', 'Advanced Metering']))
        # Stand Alone Commissioning
        ss.append(set(['DC0031ZZ']))
        # GP EB
        # AMI real time monitoring
    return reduce(lambda x, y: x.union(y), ss)
    
def get_invest_set():
    s1 = get_co_set('Capital')
    s2 = get_co_set('Operational')
    cap_only = s1.difference(s2)
    op_only = s2.difference(s1)
    cap_and_op = s1.intersection(s2)
    cap_or_op = s1.union(s2)
    return (cap_only, op_only, cap_and_op, cap_or_op)

def get_no_invest_set():
    s1 = get_all_building_set()
    s2 = get_invest_set()[-1]
    return s1.difference(s2)
    
def intersect_EUAS(df):
    euas = get_all_building_set()
    return df[df['Building_Number'].isin(euas)]

result = pd.DataFrame({'good_eui': list(get_energy_set("eui"))})
result.to_csv("~/Dropbox/gsa_2017/csv_FY/good_eui.csv")
