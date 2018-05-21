import sqlite3
import pandas as pd
import numpy as np
import os
import geocoder
import time

import util_io as uo
homedir = os.getcwd() + '/csv_FY/'
# project_dir = '/media/yujiex/work/project/data/'

project_dir = os.path.abspath(os.path.join(os.getcwd(), os.pardir)) + '/data/'

conn = uo.connect('interval_ion')
with conn:
    df1 = pd.read_sql('SELECT * FROM electric_id', conn)
    df2 = pd.read_sql('SELECT * FROM gas_id', conn)
df1['has_electric'] = 1
df2['has_gas'] = 1
df = pd.merge(df1, df2, on='id', how='outer')
df.rename(columns={'id': 'Building_Number'}, inplace=True)
conn.close()

conn = uo.connect('other_input')
with conn:
    df3 = pd.read_sql('SELECT * FROM Entire_GSA_Building_Portfolio_input', conn)
df = pd.merge(df, df3, on='Building_Number', how='left')
df['in_facility'] = df.apply(lambda r: 1 if r['Building_Number'] == r['Facility_ID'] else np.nan, axis=1)
df['Region'] = df['Region'].map(lambda x: int(x[:2]))
def replace_year(string):
    if string is None:
        return string
    elif '-' not in string:
        return string
    else:
        return '19' + string[-2:]
df['Year_Built'] = df['Year_Built'].map(replace_year)
df['Owned'] = df['Owned_or_Leased_Indicator'].map(lambda x: 1 if x == 'F' else np.nan)
df = df[['Building_Number', 'Building_Name', 'City', 'State',
         'Zip_Code', 'Street_Address', 'Region', 'Year_Built',
         'Gross_Square_Feet_(GSF)', 'Owned', 'in_facility',
         'has_electric', 'has_gas']]
conn.close()

conn = uo.connect('all')
with conn:
    df4 = pd.read_sql('SELECT * FROM EUAS_ecm', conn)
    df5 = pd.read_sql('SELECT * FROM EUAS_ecm_program', conn)
    df6 = pd.read_sql('SELECT * FROM EUAS_type', conn)
    df7 = pd.read_sql('SELECT Building_Number, year, [Electric_(kBtu)], [Gas_(kBtu)], [Oil_(kBtu)], [Steam_(kBtu)] FROM EUAS_monthly WHERE year in (2013, 2014, 2015)', conn)
conn.close()
df7 = df7.groupby(['Building_Number']).agg({'Electric_(kBtu)': 'sum', 'Gas_(kBtu)': 'sum', 'Oil_(kBtu)': 'sum', 'Steam_(kBtu)': 'sum'})
for c in df7:
    df7[c] = df7[c].map(lambda x: 1 if x > 0 else 0)
df7.rename(columns=lambda x: 'use_{0}_EUAS'.format(x[:x.find('_')]), inplace=True)
df7.reset_index(inplace=True)
# print df7.head()
df = pd.merge(df, df6, on='Building_Number', how='left')
df = pd.merge(df, df7, on='Building_Number', how='left')

df_ecm = pd.merge(df, df4, on='Building_Number', how='left')
df_ecm = df_ecm[['Building_Number', 'high_level_ECM',
                 'detail_level_ECM', 'Substantial_Completion_Date']]
df_ecm = df_ecm[df_ecm['high_level_ECM'] != 'GSALink']
df_ecm.dropna(subset=['Substantial_Completion_Date'], inplace=True)
df_ecm.drop_duplicates(inplace=True)
df_ecm.sort(columns=['Building_Number', 'high_level_ECM',
                     'detail_level_ECM'], inplace=True)
df_ecm['Substantial_Completion_Date'] = df_ecm['Substantial_Completion_Date'].map(lambda x: x.replace(' 00:00:00', ''))
df_ecm['year'] = df_ecm['Substantial_Completion_Date'].map(lambda x: x[:4])
df_energy = pd.read_csv(project_dir + 'energy_start_stop.csv')
df_ecm.to_csv(project_dir + 'interval_building_action.csv',
              index=False)
df_ecm_eng = pd.merge(df_ecm, df_energy, on='Building_Number', how='left')
for col in ['Substantial_Completion_Date', 'energy_start', 'energy_stop']:
    df_ecm_eng[col] = pd.to_datetime(df_ecm_eng[col])
df_ecm_eng ['with_retrofit'] = (df_ecm_eng['Substantial_Completion_Date'] < df_ecm_eng['energy_stop']) & (df_ecm_eng['Substantial_Completion_Date'] > df_ecm_eng['energy_start'])
df_ecm_eng.to_csv(project_dir + 'interval_indicator_wretrofit.csv',
              index=False)

# print df.head()
df.to_csv(project_dir + 'interval_building_info.csv', index=False)

def get_lat_long(canonical_add, address):
    g = geocoder.google(address)
    if not (g.json['ok']):
        print '{0},Address not found,Address not found'.format(canonical_add)
        return None, None
    else:
        latlng = g.latlng
        print '"{0}",{1},{2}'.format(canonical_add, latlng[0], latlng[1])
        return latlng

# df = pd.read_csv(project_dir + 'interval_building_info.csv')
# df = df[['Building_Number', 'City', 'State', 'Zip_Code',
#         'Street_Address']]
# df['geocoding_input'] = df.apply(lambda r: '{0},{1},{2},{3}'.format(r['Street_Address'], r['City'], r['State'], r['Zip_Code']), axis=1)
# df.to_csv(project_dir + 'interval_building_geoinput.csv', index=False)

# addresses = df['geocoding_input'].unique().tolist()
# for a in addresses:
#     get_lat_long(a, a)
#     time.sleep(0.1)

# df1 = pd.read_csv(project_dir + 'geocoding_result.csv')
# df2 = pd.read_csv(project_dir + 'interval_building_geoinput.csv')
# df = pd.merge(df2, df1, on='geocoding_input', how='left')
# df.to_csv(project_dir + 'interval_building_latlng.csv', index=False)
