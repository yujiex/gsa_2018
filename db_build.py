import sqlite3
import pandas as pd
import numpy as np
import os
import glob
import re
import ast
import time
import seaborn as sns
import matplotlib.pyplot as plt

import util
import util_io as uo
import lean_temperature_monthly as ltm
import get_building_set as gbs
homedir = os.getcwd() + '/csv_FY/'
tempdir = homedir + 'temp_data_view/'
master_dir = homedir + 'master_table/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
pro_dir = '/media/yujiex/work/project/data/'

def to_sql(excel, sheet_ids, conn):
    filename = excel[excel.find('FY1'):]
    for i in sheet_ids:
        df = pd.read_excel(excel, sheetname=i)
        # filter out records with empty name
        df = df[pd.notnull(df['Building Number'])]
        outfile = '{0}_{1}'.format(filename[:4], i + 1)
        print 'write to file: ' + outfile
        df.to_sql(outfile, conn, if_exists='replace', index=False)
    return

def excel2csv():
    print 'excel2csv...'
    conn = sqlite3.connect(homedir + 'db/energy_input.db')
    c = conn.cursor()
    filelist = glob.glob(os.getcwd() + '/input/FY/EUAS/' + '*.xlsx')
    print filelist
    for excel in filelist:
        filename = excel[excel.find('FY1'):]
        print 'processing {0}'.format(filename)
        # check_sheetname(excel, False)
        to_sql(excel, range(11), conn)
    conn.close()
    return

def check_small_area(cutoff):
    conn = uo.connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, [Gross_Sq.Ft] FROM EUAS_area WHERE [Gross_Sq.Ft] < {0} ORDER BY [Gross_Sq.Ft] DESC'.format(cutoff), conn)
    # print df
    return df

def check_change_area():
    conn = uo.connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, [Gross_Sq.Ft] FROM EUAS_area', conn)
    print df.head(n=20)
    print len(df)
    df2 = df.groupby('Building_Number').filter(lambda x: len(x) > 1)
    df2.to_csv(homedir + 'temp/change_area.csv', index=False)
    return

def check_change_cat():
    conn = uo.connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Cat FROM EUAS_monthly', conn)
    print len(df)
    df2 = df.groupby('Building_Number').filter(lambda x: len(x) > 1)
    print len(df2)
    df2.to_csv(homedir + 'temp/change_cat.csv', index=False)
    return

def view_building(b, col):
    conn = uo.connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year, Fiscal_Month, year, month, [Gross_Sq.Ft], [Region_No.], Cat, [{1}] FROM EUAS_monthly WHERE Building_Number = \'{0}\''.format(b, col), conn)
    return df

def dump_file(f, ext, conn):
    filename = f[f.rfind('/') + 1:]
    year_pattern = re.compile("20[0-9]{2}")
    year = year_pattern.search(filename).group(0)
    region_pattern = re.compile("[0-9]{1,2}")
    region = region_pattern.search(filename).group(0)
    if ext == 'excel':
        df = pd.read_excel(f, sheetname=0)
    elif ext == 'csv':
        df = pd.read_csv(f)
    df = df[df['Building Number'].notnull()]
    outfile = 'FY{0}_{1}'.format(year[-2:], region)
    print 'write to file: ' + outfile
    df.to_sql(outfile, conn, if_exists='replace', index=False)
    return

def excel2csv_singlesheet():
    conn = sqlite3.connect(homedir + 'db/energy_input.db')
    files = glob.glob(os.getcwd() + \
                      '/input/FY/EUAS/*/*.xlsx')
    for i, f in enumerate(files):
        # print i, f[f.rfind('/') + 1:]
        dump_file(f, 'excel', conn)
    # need to manually save .xls to .csv
    files = glob.glob(os.getcwd() + '/input/FY/EUAS/*/*.csv')
    for i, f in enumerate(files):
        # print i, f[f.rfind('/') + 1:]
        dump_file(f, 'csv', conn)
    conn.close()
        
def copy_table_helper(conn, conn2, table):
    with conn:
        df = pd.read_sql('SELECT * FROM {0}'.format(table), conn)
    with conn2:
        df.to_sql(table, conn2, if_exists='replace')

def copy_table():
    conn = uo.connect('all')
    conn2 = uo.connect('all_back')
    # copy_table_helper(conn, conn2, 'EUAS_monthly')
    copy_table_helper(conn, conn2, 'eui_by_fy')
    print 'end'
    return

def copy_tables():
    conn = uo.connect('backup/all')
    cursor = conn.cursor()
    tables = util.get_list_tables(cursor)
    conn2 = uo.connect('all_back')
    tables = [x for x in tables if '_test' not in x]
    tables = [x for x in tables if x != 'Temperature_Hour_UTC']
    for t in tables:
        print 'copy table: {0}'.format(t)
        copy_table_helper(conn, conn2, t)
    print 'end'
    return

def compare():
    conn = uo.connect('all')
    conn2 = uo.connect('all_back')
    df1 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn).groupby('Fiscal_Year').count()[['Building_Number']]
    df2 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn2).groupby('Fiscal_Year').count()[['Building_Number']]
    df = pd.merge(df1, df2, left_index=True, right_index=True, suffixes=['_new', '_old'])
    print
    print df
    # with conn:
    #     df1 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn)
    # with conn2:
    #     df2 = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn2)
    # df1['status'] = 'new'
    # df2['status'] = 'old'
    # df_all = pd.concat([df1, f2], ignore_index=True)
    # df_all.groupby('Building_Number').filter(lambda x: len(x) < 2)
    # df_all.to_csv(homedir + 'temp/euas_building_cmp.csv')
    # with conn:
    #     df1 = pd.read_sql('SELECT * FROM EUAS_category', conn)
    #     df2 = pd.read_sql('SELECT * FROM EUAS_category', conn2)
    # df = pd.merge(df1, df2, on='Building_Number', how='outer', suffixes=['_new', '_old'])
    # df['equal'] = df.apply(lambda r: r['Cat_new'] == r['Cat_old'], axis=1)
    # print df.head()
    # df = df[df['equal'] == False]
    # print df.head()
    # df.to_csv(homedir + 'temp/euas_cat_cmp.csv', index=False)
    # return
    # with conn:
    #     df1 = pd.read_sql('SELECT Fiscal_Year, eui FROM eui_by_fy', conn)
    #     df2 = pd.read_sql('SELECT Fiscal_Year, eui FROM eui_by_fy', conn2)
    # df1 = df1[df1['eui'] != np.inf]
    # df2 = df2[df2['eui'] != np.inf]
    # print 'new'
    # print df1.groupby('Fiscal_Year').count()
    # print 'old'
    # print df2.groupby('Fiscal_Year').count()

def clean_energy(df_all):
    df_all['year'] = df_all.apply(lambda row:
                                  util.fiscal2calyear(row['Fiscal' +
                                  '_Year'], row['Fiscal_Month']),
                                  axis=1)
    df_all['month'] = df_all['Fiscal_Month'].map(util.fiscal2calmonth)
    df_all['Electric_(kBtu)'] = df_all['Electricity_(KWH)'] * 3.412
    df_all['Gas_(kBtu)'] = df_all['Gas_(Cubic_Ft)'] * 1.026
    m_oil = (139 + 138 + 146 + 150)/4
    df_all['Oil_(kBtu)'] = df_all['Oil_(Gallon)'] * m_oil
    df_all['Steam_(kBtu)'] = df_all['Steam_(Thou._lbs)'] * 1194
    df_all['Electric_(kBtu)'] = df_all['Electric_(kBtu)'].map(np.float64)
    df_all['Gas_(kBtu)'] = df_all['Gas_(kBtu)'].map(np.float64)
    df_all['Oil_(kBtu)'] = df_all['Oil_(kBtu)'].map(np.float64)
    df_all['Steam_(kBtu)'] = df_all['Steam_(kBtu)'].map(np.float64)
    df_all['Water_(Gallon)'] = df_all['Water_(Gallon)'].map(np.float64)
    df_all['Gross_Sq.Ft'] = df_all['Gross_Sq.Ft'].map(np.float64)
    df_all['eui_elec'] = \
        df_all['Electric_(kBtu)']/df_all['Gross_Sq.Ft']
    df_all['Total_(kBtu)'] = df_all[['Electric_(kBtu)', 'Gas_(kBtu)',
                                     'Oil_(kBtu)', 'Steam_(kBtu)']].sum(axis=1)
    df_all['eui_gas'] = df_all['Gas_(kBtu)']/df_all['Gross_Sq.Ft']
    df_all['eui_oil'] = df_all['Oil_(kBtu)']/df_all['Gross_Sq.Ft']
    df_all['eui_steam'] = df_all['Steam_(kBtu)']/df_all['Gross_Sq.Ft']
    df_all['eui_water'] = df_all['Water_(Gallon)']/df_all['Gross_Sq.Ft']
    df_all['eui'] = df_all['eui_elec'] + df_all['eui_gas']
    df_all['eui_total'] = df_all['Total_(kBtu)']/df_all['Gross_Sq.Ft']
    df_all.replace(util.get_state_abbr_dict(), inplace=True)
    df_all.replace('******', np.nan, inplace=True) #take too long 
    print(list(df_all))
    df_f = df_all.sort_values(by=['Building_Number', 'Fiscal_Year', 'Fiscal_Month'])
    return df_f

def concat():
    conn = sqlite3.connect(homedir + 'db/energy_input.db')
    cursor = conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = [x[0] for x in cursor.fetchall()]
    queries = ['SELECT * FROM {0}'.format(t) for t in tables]
    with conn:
        dfs = [pd.read_sql(q, conn) for q in queries]
    conn.close()
    df_all = pd.concat(dfs, join='outer', ignore_index=True)
    df_all.dropna(axis=1, inplace=True, how='all')
    clean_energy(df_all)
    df_temp = df_f[df_f['Building_Number'] == 'PA0600ZZ']
    print df_temp[['Fiscal_Year', 'Fiscal_Month', 'eui_gas']]
    # conn = sqlite3.connect(homedir + 'db/all.db')
    # with conn:
    #     df_f.to_sql('EUAS_monthly', conn, if_exists='replace')
    # conn.close()
    print 'end'
    return

def get_eui_weather():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df1 = pd.read_sql('SELECT * FROM EUAS_monthly_weather', conn)
        df2 = pd.read_sql('SELECT Building_Number, Cat FROM EUAS_category', conn)
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    df = df[df['eui_total'] != np.inf]
    df.info()
    df_f = df.groupby(['Building_Number', 'Fiscal_Year']).agg({'Cat': 'first', 'eui_elec': 'sum', 'eui_gas': 'sum', 'eui': 'sum', 'eui_total': 'sum', 'hdd65':'sum', 'cdd65': 'sum'})
    df_f.reset_index(inplace=True)
    df_f = df_f[['Building_Number', 'Fiscal_Year', 'Cat', 'eui_elec', 'eui_gas', 'eui', 'eui_total', 'hdd65', 'cdd65']]
    df_f['eui_gas_perdd'] = df_f['eui_gas']/df_f['hdd65']
    df_f['eui_elec_perdd'] = df_f['eui_elec']/df_f['cdd65']
    df_f['eui_perdd'] = df_f['eui_gas_perdd'] + df_f['eui_elec_perdd']
    df_f.sort_values(['Building_Number', 'Fiscal_Year'], inplace=True)
    df_f.info()
    # def get_status(e, g):
    #     if e >= 12 and g >= 3:
    #         return 'Electric EUI >= 12 and Gas EUI >= 3'
    #     elif e >= 12 and g < 3:
    #         return 'Low Gas EUI'
    #     elif e < 12 and g >= 3:
    #         return 'Low Electric EUI'
    #     else:
    #         return 'Low Gas and Electric EUI'
    # print df_f.groupby(['Fiscal_Year']).count()[['Building_Number']]
    # df_f['status'] = df_f.apply(lambda r: get_status(r['eui_elec'],
    #                                                  r['eui_gas']),
    #                             axis=1)
    # print df_f['status'].value_counts()
    with conn:
        df_f.to_sql('eui_by_fy_weather', conn, if_exists='replace')
    print 'end'
    return
    
# compute eui of each single building 
# eui_total != np.inf removes records with zero square footage but 
# non-zero gas/oil/steam/electric consumption
def get_eui():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT * FROM EUAS_monthly_weather', conn)
    df = df[df['eui_total'] != np.inf]
    df.info()
    # df_f = df.groupby(['Building_Number', 'Fiscal_Year']).sum()
    df_f = df.groupby(['Building_Number', 'Fiscal_Year']).agg({'Gross_Sq.Ft': 'mean', 'Cat': 'first', 'eui_elec': 'sum', 'eui_gas': 'sum', 'eui_oil': 'sum', 'eui_steam': 'sum', 'eui_water': 'sum', 'eui': 'sum', 'eui_total': 'sum', 'eui':'sum', 'hdd65':'sum', 'cdd65': 'sum'})
    df_f.reset_index(inplace=True)
    df_f = df_f[['Building_Number', 'Fiscal_Year', 'Cat', 'Gross_Sq.Ft', 'eui_elec', 'eui_gas', 'eui_oil', 'eui_steam', 'eui_water', 'eui', 'eui_total', 'hdd65', 'cdd65']]
    df_f.sort_values(['Building_Number', 'Fiscal_Year'], inplace=True)
    df_f.info()
    def get_status(e, g):
        if e >= 12 and g >= 3:
            return 'Electric EUI >= 12 and Gas EUI >= 3'
        elif e >= 12 and g < 3:
            return 'Low Gas EUI'
        elif e < 12 and g >= 3:
            return 'Low Electric EUI'
        else:
            return 'Low Gas and Electric EUI'
    print df_f.groupby(['Fiscal_Year']).count()[['Building_Number']]
    df_f['status'] = df_f.apply(lambda r: get_status(r['eui_elec'],
                                                     r['eui_gas']),
                                axis=1)
    print df_f['status'].value_counts()
    # with conn:
    #     df_f.to_sql('eui_by_fy', conn, if_exists='replace')
    print 'end'
    return

def check_num():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly', conn)
    print df.groupby(['Fiscal_Year']).count()
    return

def get_geo_input(d):
    tokens = [d['Street_Address'], d['City'], d['State'],
              d['Zip_Code']]
    zipcode = tokens[-1]
    # print zipcode, type(zipcode)
    if (type (zipcode) != float) and (len(zipcode) == 9):
        tokens[-1] = '{0}-{1}'.format(zipcode[:5], zipcode[-4:])
    tokens = [x for x in tokens if (type(x) != float) and (x != None)]
    return ','.join(tokens)

def join_static():
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    df2 = pd.read_sql('SELECT DISTINCT Building_Number, Street_Address, City, Zip_Code FROM Entire_GSA_Building_Portfolio_input', conn)
    df2['source'] = 'Entire_GSA_Building_Portfolio_input'
    df_use = pd.read_sql('SELECT DISTINCT Building_Number, Street_Address, City, Zip_Code FROM PortfolioManager_sheet0_input', conn)
    df3 = pd.read_sql('SELECT DISTINCT Building_Number, Street_Address, City FROM euas_database_of_buildings_cmu', conn)
    conn.close()
    df3['source'] = 'euas_database_of_buildings_cmu'
    df_use['source'] = 'PortfolioManager_sheet0_input'
    df_loc = pd.concat([df2, df_use, df3], ignore_index=True)
    df_loc.sort_values(by=['Building_Number', 'source'], inplace=True)

    conn = sqlite3.connect(homedir + 'db/all.db')
    df1 = pd.read_sql('SELECT DISTINCT Building_Number, State FROM EUAS_monthly', conn)
    df_loc.to_sql('building_address_source', conn, if_exists='replace')
    df_loc.drop_duplicates(subset=['Building_Number', 'Street_Address', 'City',
                                   'Zip_Code'], inplace=True)
    df_loc.to_sql('building_address_source_unique', conn, if_exists='replace')
    df_all = pd.merge(df1, df_loc, how='left', on='Building_Number')
    df_all['geocoding_input'] = df_all.apply(get_geo_input, axis=1)
    df_all.to_sql('EUAS_address', conn, if_exists='replace')
    # df_all.to_csv(tempdir + 'EUAS_address.csv')
    conn.close()
    print 'end'
    return

def gsalink_address_geocoding():
    df = pd.read_csv(os.getcwd() + '/input/FY/GSAlink 81 Buildings Updated 9_22_15.csv')
    df.rename(columns={'Building ID': 'Building_Number', 'Street': 'Street_Address', 'Zip Code': 'Zip_Code'}, inplace=True)
    df = df[['Building_Number', 'Street_Address', 'City', 'State',
             'Zip_Code']]
    df['geocoding_input'] = df.apply(get_geo_input, axis=1)
    keys = (df['geocoding_input'].unique())
    d = geocoding_cache(keys)
    df2 = pd.DataFrame({'geocoding_input': d.keys(), 'latlng':
                        d.values()})
    df_all = pd.merge(df, df2, on='geocoding_input', how='left')
    df_all.to_csv(homedir + 'temp/geocoding_gsalink.csv')
    conn = uo.connect('all')
    with conn:
        df_all.to_sql('gsalink_address', conn, if_exists='replace')
    conn.close()
    return

def geocoding_cache(keys):
    d = {}
    for i, k in enumerate(keys):
        print i
        if not k in d:
            d[k] = str(util.get_lat_long(k))
            time.sleep(0.03)
    return d

def geocoding():
    conn = sqlite3connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT DISTINCT Building_Number, geocoding_input FROM EUAS_address', conn)
    keys = (df['geocoding_input'].unique())
    d = geocoding_cache(keys)
    df2 = pd.DataFrame({'geocoding_input': d.keys(), 'latlng':
                        d.values()})
    df_all = pd.merge(df, df2, on='geocoding_input', how='left')
    # print df_all.head()
    df_all.to_sql('EUAS_latlng', conn, if_exists='replace')
    conn.close()
    print 'end'
    return

def get_start_end():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT year, month, Building_Number FROM EUAS_monthly', conn)
    df['year'] = df['year'].map(int)
    df['month'] = df['month'].map(int)
    df.sort_values(['Building_Number', 'year', 'month'], inplace=True)
    df_min = df.groupby('Building_Number').first()
    df_max = df.groupby('Building_Number').last()
    df_min['Date_min'] = df_min.apply(lambda r: '{0}-{1}-1 00:00:00'.format(r['year'], r['month']), axis=1)
    df_min = df_min[['Date_min']]
    print df_min.head()
    df_max['Date_max'] = df_max.apply(lambda r: '{0}-{1}-{2} 23:59:59'.format(r['year'], r['month'], util.get_month_lastday(r['year'], r['month'])), axis=1)
    df_max = df_max[['Date_max']]
    print df_max.head()
    df_all = pd.merge(df_min, df_max, left_index=True, right_index=True, how='inner')
    print df_all.head()
    df_all.reset_index(inplace=True)
    df_all.to_sql('EUAS_energy_daterange', conn, if_exists='replace')
    return

# download hourly temperature station into a separate database file
# containing a table of downloaded stations
def download_weather_sep_db_gsa(test):
    if test:
        conn_w = sqlite3.connect(homedir + 'db/gsalink_utc_test.db')
        c_w = conn_w.cursor()
    else:
        conn_w = sqlite3.connect(homedir + 'db/gsalink_utc.db')
        c_w = conn_w.cursor()
    conn = sqlite3.connect(homedir + 'db/all.db')
    c = conn.cursor()
    df = pd.read_csv(homedir + 'temp/geocoding_gsalink.csv')
    df.info()
    # with conn:
    #     df = pd.read_sql('SELECT Building_Number, Latlng FROM gsalink_address', conn)

    buildings = df['Building_Number'].tolist()
    latlngs = df['latlng'].tolist()
    print len(buildings), len(latlngs)
    length = 5
    stations = []
    dists = []
    bs = []
    ll = []
    downloaded = set()
    no_data = set()
    bs_list = zip(buildings, latlngs)
    if test:
        bs_list = bs_list[:5]
    for i, (b, loc) in enumerate(bs_list):
        latlng = ast.literal_eval(loc)
        print i, b, latlng
        sd_list = util.get_station_dist(b, latlng, length)
        print sd_list
        mindate = '2010-9-1T00:00:00Z'
        maxdate = '2016-6-27T00:00:00Z'
        station = 'Not Found'
        dist = -1
        for s, d in sd_list:
            if s in downloaded:
                print '{0} exist'.format(s)
                station = s
                dist = d
                break
            elif s in no_data:
                print '{0} has no data'.format(s)
                continue
            else:
                ori = time.time()
                df = ltm.get_weather_data(s, mindate, maxdate, 'H')
                if df is None:
                    print '{0} has no data'.format(s)
                    no_data.add(s)
                    continue
                df['ICAO'] = s
                station = s
                dist = d
                df.rename(columns={s: 'Temperature_F', 'index':
                                   'Timestamp'}, inplace=True)
                df['Timestamp'] = df.index.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
                with conn_w:
                    df.to_sql(s, conn_w, if_exists='replace')
                break
        bs.append(b)
        ll.append(loc)
        stations.append(station)
        dists.append(dist)
        downloaded.add(station)
    # print len(buildings), len(latlngs), len(stations), len(dists), '11111'
    df_station = pd.DataFrame({'Building_Number': bs, 'latlng': ll, 'ICAO': stations, 'Distance_Mile': dists})
    if test:
        df_station.to_sql('gsalink_weather_station_test', conn, if_exists='replace')
    else:
        df_station.to_sql('gsalink_weather_station', conn, if_exists='replace')
    df_downloaded = pd.DataFrame({'ICAO': list(downloaded)})
    df_nodata = pd.DataFrame({'ICAO': list(no_data)})
    with conn_w:
        df_downloaded.to_sql('downloaded', conn_w, if_exists='replace')
        df_nodata.to_sql('nodata', conn_w, if_exists='replace')
    conn_w.close()
    conn.close()
    return

# download hourly temperature station into a separate database file
# containing a table of downloaded stations
def download_weather_sep_db(test, step, db_prefix, variable):
    if test:
        conn_w = sqlite3.connect(homedir + 'db/{}_test_17.db'.format(db_prefix))
        c_w = conn_w.cursor()
    else:
        conn_w = sqlite3.connect(homedir + 'db/{}_17.db'.format(db_prefix))
        c_w = conn_w.cursor()
    conn = sqlite3.connect(homedir + 'db/all.db')
    c = conn.cursor()
    with conn:
        df = pd.read_sql('SELECT Building_Number, Latlng FROM EUAS_latlng_2', conn)
    buildings = df['Building_Number'].tolist()
    latlngs = df['latlng'].tolist()
    print len(buildings), len(latlngs)
    length = 5
    stations = []
    dists = []
    bs = []
    ll = []
    downloaded = set()
    no_data = set()
    bs_list = zip(buildings, latlngs)
    col_rename_dict = {'temperature': 'Temperature_F', 'HDD': 'HDD', 'CDD': 'CDD'}
    if test:
        bs_list = bs_list[:5]
    for i, (b, loc) in enumerate(bs_list):
        try:
            latlng = ast.literal_eval(loc)
        except ValueError:
            print 'malformed latlng'
            continue
        print i, b, latlng
        sd_list = util.get_station_dist(b, latlng, length)
        print sd_list
        mindate = '2002-9-30T00:00:00Z'
        maxdate = '2017-10-1T00:00:00Z'
        station = 'Not Found'
        dist = -1
        for s, d in sd_list:
            if s in downloaded:
                print '{0} exist'.format(s)
                station = s
                dist = d
                break
            elif s in no_data:
                print '{0} has no data'.format(s)
                continue
            else:
                ori = time.time()
                df = ltm.get_weather_data(s, mindate, maxdate, step, variable)
                if df is None:
                    print '{0} has no data'.format(s)
                    no_data.add(s)
                    continue
                df['ICAO'] = s
                station = s
                dist = d
                df.rename(columns={s: col_rename_dict[variable], 'index':
                                   'Timestamp'}, inplace=True)
                df['Timestamp'] = df.index.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
                print df.head()
                with conn_w:
                    df.to_sql(s, conn_w, if_exists='replace', index=False)
                break
        bs.append(b)
        ll.append(loc)
        stations.append(station)
        dists.append(dist)
        downloaded.add(station)
    # print len(buildings), len(latlngs), len(stations), len(dists), '11111'
    df_station = pd.DataFrame({'Building_Number': bs, 'latlng': ll, 'ICAO': stations, 'Distance_Mile': dists})
    if test:
        df_station.to_sql('building_weather_station_test', conn, if_exists='replace')
    else:
        df_station.to_sql('building_weather_station', conn, if_exists='replace')
    df_downloaded = pd.DataFrame({'ICAO': list(downloaded)})
    df_nodata = pd.DataFrame({'ICAO': list(no_data)})
    with conn_w:
        df_downloaded.to_sql('downloaded', conn_w, if_exists='replace')
        df_nodata.to_sql('nodata', conn_w, if_exists='replace')
    conn_w.close()
    conn.close()
    return

def download_weather(test):
    conn = sqlite3.connect(homedir + 'db/all.db')
    c = conn.cursor()
    if test:
        c.execute("DROP TABLE IF EXISTS Temperature_Hour_UTC_test")
        c.execute('CREATE TABLE Temperature_Hour_UTC_test (ICAO text KEY, Timestamp text KEY, Temperature_F real);')
    else:
        c.execute("DROP TABLE IF EXISTS Temperature_Hour_UTC")
        c.execute('CREATE TABLE Temperature_Hour_UTC (ICAO text KEY, Timestamp text KEY, Temperature_F real);')
    df = pd.read_sql('SELECT Building_Number, Latlng FROM EUAS_latlng_', conn)
    buildings = df['Building_Number'].tolist()
    latlngs = df['latlng'].tolist()
    print len(buildings), len(latlngs)
    length = 5
    stations = []
    dists = []
    bs = []
    ll = []
    downloaded = set()
    no_data = set()
    bs_list = zip(buildings, latlngs)
    if test:
        bs_list = bs_list[:5]
    for i, (b, loc) in enumerate(bs_list):
        latlng = ast.literal_eval(loc)
        print i, b, latlng
        sd_list = util.get_station_dist(b, latlng, length)
        print sd_list
        mindate = '2002-9-30T00:00:00Z'
        maxdate = '2016-5-1T00:00:00Z'
        station = 'Not Found'
        dist = -1
        for s, d in sd_list:
            if s in downloaded:
                print '{0} exist'.format(s)
                station = s
                dist = d
                break
            elif s in no_data:
                print '{0} has no data'.format(s)
                continue
            else:
                ori = time.time()
                df = ltm.get_weather_data(s, mindate, maxdate, 'H')
                if df is None:
                    print '{0} has no data'.format(s)
                    no_data.add(s)
                    continue
                df['ICAO'] = s
                station = s
                dist = d
                df.rename(columns={s: 'Temperature_F', 'index':
                                   'Timestamp'}, inplace=True)
                df['Timestamp'] = df.index.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
                if test:
                    df.to_sql('Temperature_Hour_UTC_test', conn,
                            if_exists='append')
                else:
                    df.to_sql('Temperature_Hour_UTC', conn,
                              if_exists='append')
                break
        bs.append(b)
        ll.append(loc)
        stations.append(station)
        dists.append(dist)
        downloaded.add(station)
    # print len(buildings), len(latlngs), len(stations), len(dists), '11111'
    df_station = pd.DataFrame({'Building_Number': bs, 'latlng': ll, 'ICAO': stations, 'Distance_Mile': dists})
    if test:
        df_station.to_sql('building_weather_station_test', conn, if_exists='replace')
    else:
        df_station.to_sql('building_weather_station', conn, if_exists='replace')
    conn.close()
    return

def db_view_alltable(db):
    conn = sqlite3.connect(homedir + 'db/{0}.db'.format(db))
    c = conn.cursor()
    tables = util.get_list_tables(c)
    tables.remove('Temperature_Hour_UTC')
    for t in tables:
        print 
        print t
        print 
        util.describe_table(conn, t, False)
    conn.close()
    return

def db_view(db, header, *args):
    conn = sqlite3.connect(homedir + 'db/{0}.db'.format(db))
    c = conn.cursor()
    tables = util.get_list_tables(c)
    for t in tables:
        print t
    if len(args) == 0:
        t = tables[-1]
    else:
        t = args[0]
    print 
    print t
    print 
    util.describe_table(conn, t, header)
    conn.close()
    return
    
# 'AVG(Temperature_F)'
# print timezone and offset to stdout
def get_bd_timezone_output():
    conn = uo.connect('all')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, ICAO, latlng FROM EUAS_monthly_weather', conn)
    bl = zip(df['Building_Number'], df['latlng'])
    print len(bl)
    for i, (b, l) in enumerate(bl):
        latlng = ast.literal_eval(l)
        util.get_timezone(latlng[0], latlng[1], b, i)
        time.sleep(0.1)
        
def timezone2db():
    df = pd.read_csv(homedir + 'EUAS_timezone.csv')
    conn = uo.connect('all')
    with conn:
        df.to_sql('EUAS_timezone', conn, if_exists='replace')
    print 'end'
    conn.close()
    conn = uo.connect('interval_ion')
    with conn:
        df.to_sql('EUAS_timezone', conn, if_exists='replace')
    print 'end'
    conn.close()

def interval_availability():
    conn = uo.connect('all')
    conn2 = uo.connect('interval_ion')
    with conn:
        df = pd.read_sql('SELECT DISTINCT Building_Number, latlng FROM EUAS_monthly_weather', conn)
    with conn2:
        df2 = pd.read_sql('SELECT DISTINCT id FROM electric_id', conn2)
        df3 = pd.read_sql('SELECT DISTINCT id FROM gas_id', conn2)
    df4 = pd.concat([df2, df3], ignore_index=True)
    print len(df4)
    df4.drop_duplicates(inplace=True)
    print len(df4)
    df4.rename(columns={'id': 'Building_Number'}, inplace=True)
    df_all = pd.merge(df4, df, on='Building_Number')
    df_all.to_csv(pro_dir + 'interval_latlng.csv', index=False)
    conn.close()
    conn2.close()

def aggregate_sep_station(method):
    conn = sqlite3.connect(homedir + 'db/weather_hourly_utc.db')
    c = conn.cursor()
    if method == 'ave':
        agg_fun_str = 'AVG(Temperature_F) '
    elif method == 'hdd65':
        base = float(method[-2:])
        agg_fun_str = 'SUM(MAX(65.0 - Temperature_F, 0.0))/24.0 '.format(base) 
    elif method == 'cdd65':
        base = float(method[-2:])
        agg_fun_str = 'SUM(MAX(Temperature_F - {0}, 0.0))/24.0 '.format(base)
    print '11111111111111111'
    c.execute('DROP TABLE IF EXISTS weather_{0}'.format(method))
    c.execute('CREATE TABLE weather_{0} (month text KEY, year text KEY, ICAO text KEY, {0} real);'.format(method))
    print '11111111111111111'
    with conn:
        downloaded = pd.read_sql('SELECT * FROM downloaded', conn)['ICAO'].tolist()
    print len(downloaded)
    for i, table in enumerate(downloaded):
        print i, table
        query = 'INSERT INTO weather_{0} '.format(method) + \
                'SELECT strftime(\'%m\', Timestamp) as month,' + \
                    'strftime(\'%Y\', Timestamp) as year, ICAO,' + \
                    agg_fun_str + \
                'FROM {0} GROUP BY ICAO, year, month'.format(table)
        with conn:
            c.execute(query)
    conn.close()
    print 'end'
    return

def aggregate(table, method):
    conn = sqlite3.connect(homedir + 'db/all.db')
    c = conn.cursor()
    if method == 'ave':
        agg_fun_str = 'AVG(Temperature_F) '
    elif method == 'hdd65':
        base = float(method[-2:])
        agg_fun_str = 'SUM(MAX(65.0 - Temperature_F, 0.0))/24.0 '.format(base) 
    elif method == 'cdd65':
        base = float(method[-2:])
        agg_fun_str = 'SUM(MAX(Temperature_F - {0}, 0.0))/24.0 '.format(base)
    print '11111111111111111'
    c.execute('DROP TABLE IF EXISTS weather_{0}'.format(method))
    c.execute('CREATE TABLE weather_{0} (month text KEY, year text KEY, ICAO text KEY, {0} real);'.format(method))
    print '11111111111111111'
    query = 'INSERT INTO weather_{0} '.format(method) + \
            'SELECT strftime(\'%m\', Timestamp) as month,' + \
                   'strftime(\'%Y\', Timestamp) as year, ICAO,' + \
                   agg_fun_str + \
            'FROM {0} GROUP BY ICAO, year, month'.format(table)
    print '11111111111111111'
    c.execute(query)
    conn.commit()
    print '11111111111111111'
    conn.close()
    print 'end'
    return

def get_cat():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year, Cat FROM EUAS_monthly ORDER BY Building_Number ASC, Fiscal_Year ASC;', conn)
    df2 = df.groupby('Building_Number').last()
    df2.reset_index(inplace=True)
    df2.rename(columns={'Fiscal_Year': 'Record_Year'}, inplace=True)
    df2.to_sql('EUAS_category', conn, if_exists='replace')
    conn.close()
    print 'end'
    return

# excel serial date to actual time
# source: https://gist.github.com/oag335/9959241
def convert_excel_time(excel_time):
    '''
    converts excel float format to pandas datetime object
    round to '1min' with 
    .dt.round('1min') to correct floating point conversion innaccuracy
    '''
    return pd.to_datetime('1899-12-30') + pd.to_timedelta(excel_time,'D')

def dump_static():
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    # df2 = pd.read_csv(os.getcwd() + '/input/FY/static info/Entire GSA Building Portfolio.csv')
    # df2.rename(columns={'Building ID': 'Building Number', 'Street':
    #                     'Street Address'}, inplace=True)
    # df2.to_sql('Entire_GSA_Building_Portfolio_input', conn, if_exists='replace')
    # filename = os.getcwd() + '/csv/all_column/sheet-0-all_col.csv'
    # df_use = pd.read_csv(filename)
    # df_use['Property Name'] = df_use['Property Name'].map(lambda x: x.partition(' ')[0][:8])
    # df_use.rename(columns={'Property Name': 'Building Number',
    #                        'City/Municipality': 'City', 'Postal Code':
    #                        'Zip Code'}, inplace=True)
    # df_use.to_sql('PortfolioManager_sheet0_input', conn,
    #               if_exists='replace')
    # df3 = pd.read_csv(os.getcwd() + '/input/FY/static info/buildings_in_facility_fy15.csv', header=None, skiprows=6, names=['Region Number', 'Facility Number', 'Building Number', 'Facility total gsf', 'Building gsf'])
    # with conn:
    #     df3.to_sql('buildings_in_facility_fy15', conn,
    #                if_exists='replace')
    df4 = pd.read_csv(os.getcwd() + '/input/FY/static info/euas_database_of_buildings_cmu.csv')
    df4.drop(['Building ID', 'Historical Status Desc'], axis=1, inplace=True)
    df4['Building Date - Construction Completed'] = df4['Building Date - Construction Completed'].map(convert_excel_time)
    df4['Building Date - Last Modernization'] = df4['Building Date - Last Modernization'].map(convert_excel_time)
    df4.rename(index=str, columns={'Location Facility Code': 'Building_Number',
                                   'Street Address': 'Street_Address', 'State Code': 'State'},
               inplace=True)
    print df4.head()
    with conn:
        df4.to_sql('euas_database_of_buildings_cmu', conn, if_exists='replace')
    conn.close()
    print 'end'
    return

def get_use():
    conn = sqlite3.connect(homedir + 'db/all.db')
    print '    creating EUAS_type.csv master table ...'
    with conn:
        df1 = pd.read_sql('SELECT * FROM EUAS_category', conn)
    conn_other = sqlite3.connect(homedir + 'db/other_input.db')
    with conn_other:
        df2 = pd.read_sql('SELECT Building_Number, [Self-Selected_Primary_Function] FROM PortfolioManager_sheet0_input', conn_other)
    duplicated = df2[df2.duplicated(cols='Building_Number')]
    print duplicated
    print df2[df2['Building_Number'].isin(duplicated['Building_Number'].tolist())]
    df2.drop_duplicates(cols='Building_Number', take_last=True, inplace=True)
    print len(df1)
    df = pd.merge(df1, df2, on='Building_Number', how='left')
    print len(df)
    df = df[['Building_Number', 'Self-Selected_Primary_Function']]
    print df.head()
    df.to_sql('EUAS_type', conn, if_exists='replace')
    return

def check():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT * FROM EUAS_monthly', conn)
    # conn2 = sqlite3.connect(homedir + 'db/other_input.db')
    with conn2:
        df3 = pd.read_sql('SELECT * FROM buildings_in_facility_fy15', conn2)
    # df4 = df.groupby(['Building_Number', 'Fiscal_Year']).mean()[['Gross_Sq.Ft']]
    # df4.reset_index(inplace=True)
    # zeros = set(df4[df4['Gross_Sq.Ft'] == 0]['Building_Number'].tolist())
    # df_zero = df4[df4['Building_Number'].isin(zeros)][['Building_Number', 'Fiscal_Year', 'Gross_Sq.Ft']]
    # df_zero.sort_values(['Building_Number', 'Fiscal_Year'], inplace=True)
    # print df_zero.to_csv(homedir + '/check/zero_sqft.csv', index=False)
    # col = 'Gas_(Cubic_Ft)'
    # df_oil = df[['Building_Number', 'year', 'month', col]]
    # df_oil.sort_values(['Building_Number', 'year', col], inplace=True)
    # df_oil_nona = df_oil[df_oil[col] > 0]
    # df_oil_max = df_oil_nona.drop_duplicates(cols=['Building_Number', 'year'], take_last=True)
    # print df_oil_max['month'].value_counts()
    # with conn:
    #     df_add = pd.read_sql('SELECT * FROM EUAS_latlng_', conn)
    #     df_add.info()
    #     df_add2 = df_add.groupby(['Building_Number', 'latlng']).first()
    #     df_add2.reset_index(inplace=True)
    #     df_add3 = df_add2.groupby('Building_Number').filter(lambda x: len(x) > 1)
    #     print df_add3.head(n=10)
    #     df_add.drop_duplicates(cols='Building_Number').info()
    # with conn:
    #     df_add = pd.read_sql('SELECT * FROM EUAS_address', conn)
    #     print df_add.head()
    #     df_add2 = df_add.drop_duplicates(cols = 'Building_Number')
    #     print(len(df_add2[df_add2['Street_Address'].isnull()]))
    #     df_add2.info()
    with conn:
        df_dist = pd.read_sql('SELECT Building_Number, Distance_Mile, ICAO FROM building_weather_station', conn)
        df_dist.drop_duplicates(cols=['Building_Number', 'Distance_Mile'], inplace=True)
        df_dist.sort_values(['Building_Number', 'Distance_Mile'], inplace=True)
        # print df_dist['Distance_Mile'].describe()
        # print df_dist.sort_values('Distance_Mile', ascending=False).head(n=10)
        # print df_dist.head(n=15)
        df = df_dist.groupby('Building_Number').last()
        print df.sort_values('Distance_Mile', ascending=False).head(n=10)
        print df.describe()
    # with conn:
    #     df = pd.read_sql('SELECT Building_Number, eui_gas, eui_elec FROM eui_by_fy', conn)
    #     print df[(df['eui_gas'] > 0) & (df['eui_gas'] != np.inf)]['eui_gas'].quantile(0.1)
    #     print df[(df['eui_gas'] > 0) & (df['eui_gas'] != np.inf)]['eui_gas'].describe()
    #     print df[(df['eui_elec'] > 0) & (df['eui_elec'] != np.inf)]['eui_elec'].quantile(0.1)
    #     print df[(df['eui_elec'] > 0) & (df['eui_elec'] != np.inf)]['eui_elec'].describe()

def get_latlng_from_datafile():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df1 = pd.read_sql('SELECT * FROM EUAS_latlng_', conn)
    print len(df1)
    df2 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly', conn)
    print len(df2)
    pd.read_sql('SELECT * FROM EUAS_latlng_2', conn).to_csv(homedir + \
                                                            'db_build_temp_csv/EUAS_latlng_2_old.csv')
    df = pd.merge(df2, df1, on='Building_Number', how='left')
    df['source'] = "geocoding"
    print len(df)
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    with conn:
        df_latlng = pd.read_sql('SELECT Building_Number, Latitude, Longitude FROM euas_database_of_buildings_cmu', conn)
    df_latlng['latlng'] = df_latlng.apply(lambda r: '[{}, {}]'.format(r['Latitude'], r['Longitude']), axis=1)
    df_latlng.drop(['Latitude', 'Longitude'], axis=1, inplace=True)
    conn.close()
    df_latlng['source'] = 'euas_database_of_buildings_cmu'
    df_final = pd.merge(df, df_latlng, how='left', on='Building_Number')
    print df_final.head()
    df_final['latlng_x'].update(df_final['latlng_y'])
    df_final['source_x'].update(df_final['source_y'])
    df_final.rename(index=str, columns={'latlng_x': 'latlng', 'source_x': 'source'}, inplace=True)
    df_final.drop('latlng_y', axis=1, inplace=True)
    df_final.drop('source_y', axis=1, inplace=True)
    print df_final.head()
    conn = uo.connect('all')
    with conn:
        df_final.to_sql('EUAS_latlng_2', conn, if_exists='replace')
    conn.close()
    print 'end'

def re_geocoding():
    conn = sqlite3.connect(homedir + 'db/all.db')
    df = pd.read_sql('SELECT * FROM EUAS_latlng', conn)
    print df.head()
    keys = df[df['latlng'] == 'None']['geocoding_input']
    def modify_zipcode(string):
        zipcode_idx = string.rfind(',') + 1
        zipcode = string[zipcode_idx:]
        if len(zipcode) == 9:
            return '{0}{1}-{2}'.format(string[:zipcode_idx],
                                       zipcode[:5], zipcode[-4:])
        else:
            return string
    newkeys = [modify_zipcode(k) for k in keys]
    def get_coarse(address):
        return ','.join(address.split(',')[-1:])
    newkeys = [get_coarse(x) for x in newkeys]
    set_newkeys = set(newkeys)
    d = geocoding_cache(set_newkeys)
    for k in d:
        print k, d[k]
    d1 = dict(zip(keys, newkeys))
    d2 = {k: d[d1[k]] for k in keys}
    for k in d2:
        print d2[k]
    df['latlng'] = df.apply(lambda r: d2[r['geocoding_input']] if r['latlng'] == 'None' else r['latlng'], axis=1)
    df.to_sql('EUAS_latlng_', conn, if_exists='replace')
    conn.close()
    return

def get_weather(test, step, db_prefix):
    # get_start_end()
    # download_weather(test)
    variable = 'temperature'
    download_weather_sep_db(test, step, db_prefix, variable)
    return

def get_monthly_temperature_CDD_HDD(test):
    if test:
        conn_w = sqlite3.connect(homedir + 'db/weather_monthly_test_17.db')
        c_w = conn_w.cursor()
    else:
        conn_w = sqlite3.connect(homedir + 'db/weather_monthly_17.db')
        c_w = conn_w.cursor()
    conn = sqlite3.connect(homedir + 'db/all.db')
    c = conn.cursor()
    with conn:
        df = pd.read_sql('SELECT Building_Number, Latlng FROM EUAS_latlng_2', conn)
    buildings = df['Building_Number'].tolist()
    latlngs = df['latlng'].tolist()
    print len(buildings), len(latlngs)
    length = 5
    stations = []
    dists = []
    bs = []
    ll = []
    downloaded = set()
    no_data = set()
    bs_list = zip(buildings, latlngs)
    if test:
        bs_list = bs_list[:5]
    for i, (b, loc) in enumerate(bs_list):
        try:
            latlng = ast.literal_eval(loc)
        except ValueError:
            print 'malformed latlng'
            continue
        print i, b, latlng
        sd_list = util.get_station_dist(b, latlng, length)
        print sd_list
        mindate = '2002-9-30T00:00:00Z'
        maxdate = '2017-10-1T00:00:00Z'
        station = 'Not Found'
        dist = -1
        for s, d in sd_list:
            if s in downloaded:
                print '{0} exist'.format(s)
                station = s
                dist = d
                break
            elif s in no_data:
                print '{0} has no data'.format(s)
                continue
            else:
                ori = time.time()
                df = ltm.get_weather_data(s, mindate, maxdate, 'M')
                if df is None:
                    print '{0} has no data'.format(s)
                    no_data.add(s)
                    continue
                df['ICAO'] = s
                station = s
                dist = d
                df.rename(columns={s: 'Temperature_F', 'index':
                                   'Timestamp'}, inplace=True)
                df['Timestamp'] = df.index.map(lambda x: x.strftime('%Y-%m-%d %H:%M:%S'))
                print df.head()
                with conn_w:
                    df.to_sql(s, conn_w, if_exists='replace', index=False)
                break
        bs.append(b)
        ll.append(loc)
        stations.append(station)
        dists.append(dist)
        downloaded.add(station)
    # print len(buildings), len(latlngs), len(stations), len(dists), '11111'
    df_station = pd.DataFrame({'Building_Number': bs, 'latlng': ll, 'ICAO': stations, 'Distance_Mile': dists})
    if test:
        df_station.to_sql('building_weather_station_test', conn, if_exists='replace')
    else:
        df_station.to_sql('building_weather_station', conn, if_exists='append')
    df_downloaded = pd.DataFrame({'ICAO': list(downloaded)})
    df_nodata = pd.DataFrame({'ICAO': list(no_data)})
    with conn_w:
        df_downloaded.to_sql('downloaded', conn_w, if_exists='replace')
        df_nodata.to_sql('nodata', conn_w, if_exists='replace')
    conn_w.close()
    conn.close()
    return

def input_energy_18format():
    df1 = pd.read_excel(os.getcwd() + '/input/FY/EUAS/EUAS_AllRegions_2016-2017.xlsx', sheet_name="2016")
    df2 = pd.read_excel(os.getcwd() + '/input/FY/EUAS/EUAS_AllRegions_2016-2017.xlsx', sheet_name="2017")
    df_all = pd.concat([df1, df2], ignore_index=True)
    print(list(df_all))
    df_all.rename(columns=lambda x: x.replace(" ", "_"), inplace=True)
    df_all = clean_energy(df_all)
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df_old = pd.read_sql('SELECT * FROM EUAS_monthly', conn)
    # df_old.to_sql('EUAS_monthly_backup', conn, if_exists='replace')
    df = pd.concat([df_old, df_all], ignore_index=True)
    df.drop_duplicates(inplace=True)
    df.to_sql('EUAS_monthly', conn, if_exists='replace')
    # print(list(df_old))

def input_energy():
    # excel2csv()
    # excel2csv_singlesheet()
    # concat()
    # get_eui()
    # get_eui_weather()
    # get_area()
    # get_cat()
    # dump_static()
    # join_static()
    # geocoding()
    # re_geocoding()
    get_latlng_from_datafile()
    return

# FIXME: why this is not used
# average_temp()

def join_energy_weather():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df1 = pd.read_sql('SELECT Building_Number, Fiscal_Year, Fiscal_Month, year, month, [Electric_(kBtu)], [Gas_(kBtu)], eui_elec, eui_gas, eui, eui_total FROM EUAS_monthly', conn)
        print len(df1)
        df2 = pd.read_sql('SELECT * FROM building_weather_station ORDER BY Building_Number ASC, Distance_Mile ASC', conn)
        print 'before delete duplicate station', len(df2)
        df2.drop_duplicates(cols=['Building_Number'], take_last=False, inplace=True)
        print 'after delete duplicate station', len(df2)
        df3 = pd.merge(df1, df2, on='Building_Number', how='left')
        print len(df3)
        dfs = [pd.read_sql('SELECT * FROM weather_{0}'.format(x), conn)
            for x in ['ave', 'hdd65', 'cdd65']]
        for df in dfs:
            df['year'] = df['year'].map(float)
            df['month'] = df['month'].map(float)
        df_all = reduce(lambda x, y: pd.merge(x, y, on=['ICAO', 'year', 'month'], how='left'), [df3] + dfs)
        df_all.sort_values(['Building_Number', 'year', 'month'], inplace=True)
        df_all.to_sql('EUAS_monthly_weather', conn,
                      if_exists='replace')
    print 'end'
    return
    
def combine_header(tuple3):
    list3 = list(tuple3)
    if list3[1] == 'Other':
        list3 = [list3[0], list3[2]]
    level3 = list3[-1]
    level3 = level3.replace('Repairs or Alterations', 'Repairs')
    list3 = list3[:-1] + [level3]
    if level3 == 'Yes':
        return '_'.join(list3[:2])
    elif level3 in ['New', 'Repairs']:
        return '_'.join([list3[0], list3[2], list3[1]])
    else:
        return '_'.join(list3)

def dump_ecm():
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    print 'reading and cleaning detail level ECM action files'
    # get rid of unicode
    df1 = pd.read_csv(os.getcwd() + '/input/FY/ScopePortfolioReport_20160105-7.csv', header=range(3))
    cols = list(df1)
    cols_combine = map(combine_header, cols)
    df2 = pd.read_csv(os.getcwd() +
                      '/input/FY/ScopePortfolioReport_20160105-5.csv',
                      header=None, skiprows=3, names=cols_combine)
    df2.drop('Project Type_Project Type_Project Type', axis=1, inplace=True)
    cols_combine.remove('Project Type_Project Type_Project Type')
    df2.to_sql('ScopePortfolioReport', conn, if_exists='replace')
    return
    
def melt_action(df, ecm_cols, source):
    df_melt = pd.melt(df, id_vars=['Building ID', 'Substantial Completion Date'], value_vars=ecm_cols)
    df_melt.dropna(subset=['Building ID', 'Substantial Completion Date'], inplace=True)
    df_melt.replace('Y', 1, inplace=True)
    df_melt.fillna({'value': 0}, inplace=True)
    # df_melt = df_melt[df_melt['value'] == 1]
    df_melt = df_melt[df_melt['value'] > 0]
    df_melt.drop('value', axis=1, inplace=True)
    df_melt['time_in_range'] = df_melt['Substantial Completion Date'].map(lambda x: x.year < 2016 and x.year > 2002)
    df_melt = df_melt[df_melt['time_in_range']]
    df_melt.drop('time_in_range', axis=1, inplace=True)
    df_melt.sort_values('Building ID', inplace=True)
    df_melt.rename(columns={'Building ID': 'Building Number', 'variable': 'ECM high level action'}, inplace=True)
    df_melt.drop_duplicates(inplace=True)
    df_melt['source'] = source
    df_melt['Substantial Completion Date'] = df_melt['Substantial'
        ' Completion Date'].map(str)
    return df_melt

def read_ecm_highlevel_long():
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    df = pd.read_csv(os.getcwd() + '/input/FY/Portfolio HPGB'
        ' Dashboard_highlevel.csv')
    with conn:
        df.to_sql('Portfolio_HPGB_Dashboard_highlevel', conn, if_exists='replace')
    df['Substantial Completion Date'] = pd.to_datetime(df['Substantial Completion Date'])
    ecm_cols = ['Advanced Metering', 'Building Envelope', 
                'Building Tuneup or Utility Improvements', 
                'HVAC', 'IEQ', 'Lighting', 'Renewable Energy', 'Water']
    df = df[['Building ID', 'Substantial Completion Date'] + ecm_cols]
    df_melt = melt_action(df, ecm_cols, 'Portfolio HPGB Dashboard')
    df_gsadate = pd.read_csv(os.getcwd() + \
                             '/input/FY/GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates.csv')
    with conn:
        df_gsadate.to_sql('GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates', conn, if_exists='replace')
    df_gsadate = df_gsadate[['Building ID', 'Rollout Date']]
    df_gsadate['ECM high level action'] = 'GSALink'
    df_gsadate.rename(columns={'Building ID': 'Building Number',
                               'Rollout Date': 'Substantial Completion'
                               ' Date'}, inplace=True)
    df_gsadate['source'] = 'GSAlink_Buildings_First_55'
    df_date = pd.read_csv(os.getcwd() + '/input/FY/ECM info/Light-Touch M&V_sheet0_new.csv', header=3, skipfooter=4)
    df_date = df_date[['Building ID', 'Substantial Completion Date1']]
    df_date.rename(columns={'Substantial Completion Date1': 'Substantial Completion Date'}, inplace=True)
    with conn:
        df_date.to_sql('Light_Touch_M_V_sheet0_new', conn, if_exists='replace')
    df_date['Substantial Completion Date'] = pd.to_datetime(df_date['Substantial Completion Date'])
    df_act = pd.read_csv(os.getcwd() + '/input/FY/ECM info/Light-Touch M&V_sheet1_new.csv', header=3, skipfooter=4)
    df_act.rename(columns={'Building Tune Up': 'Building Tuneup or '
        'Utility Improvements', 'Indoor Environmental Quality': 'IEQ'},
                  inplace=True)
    df_act = df_act[['Building ID', 'Total ARRA Obligation'] + ecm_cols]
    df_act.replace({'Total ARRA Obligation': {'\$': '', ',': ''}},
                   inplace=True)
    df_act['Total ARRA Obligation'] = df_act['Total ARRA Obligation'].map(lambda x: float(x))
    df_act.replace('Y', 1, inplace=True)
    with conn:
        df_act.to_sql('Light_Touch_M_V_sheet1_new', conn, if_exists='replace')
    if conn:
        conn.close()
    df_act_dedup = df_act.groupby('Building ID').sum()
    df_date.set_index('Building ID', inplace=True)
    df_dateact = pd.merge(df_act_dedup, df_date, left_index=True, right_index=True)
    df_date.reset_index(inplace=True)
    df_dateact.to_csv(homedir + \
                      'master_table/ECM/ecm_highlevel_new.csv')
    df_dateact.reset_index(inplace=True)
    df_melt2 = melt_action(df_dateact, ecm_cols, 'Light-Touch M_V')

    df_all = pd.concat([df_melt, df_melt2, df_gsadate], ignore_index=True)
    df_all.sort_values(columns=['Building Number', 'ECM high level action', 'source'], inplace=True)
    conn2 = sqlite3.connect(homedir + 'db/all.db')
    with conn2:
        df_all.to_sql('ecm_highlevel_long_wconflict', conn2, if_exists='replace')
    df_all.drop_duplicates(cols=['Building Number', 'ECM high level action'], take_last=False, inplace=True)
    with conn2:
        df_all.to_sql('ecm_highlevel_long', conn2, if_exists='replace')
    df_all.rename(columns={'Building Number': 'Building_Number'}, inplace=True)
    df_euas = gbs.intersect_EUAS(df_all)
    with conn2:
        df_euas.to_sql('EUAS_ecm_highlevel', conn2,
                        if_exists='replace')
    print 'end'
    return
    
def read_ecm_detail_long():
    print 'reading and cleaning detail level ECM action files'
    df1 = pd.read_csv(os.getcwd() + '/input/FY/ScopePortfolioReport_20160105-7.csv', header=range(3))
    cols = list(df1)
    cols_combine = map(combine_header, cols)
    df2 = pd.read_csv(os.getcwd() +
                      '/input/FY/ScopePortfolioReport_20160105-5.csv',
                      header=None, skiprows=3, names=cols_combine)
    df2.drop('Project Type_Project Type_Project Type', axis=1, inplace=True)
    cols_combine.remove('Project Type_Project Type_Project Type')
    df2['value_col'] = 1
    ecm_cols = cols_combine[1:]
    df2_melt = pd.melt(df2, id_vars=['Building ID_Building ID_Building ID'], value_vars=ecm_cols)
    df2_melt.dropna(subset=['value'], inplace=True)
    df3 = df2_melt.groupby('variable').filter(lambda x: x.count() > 30)
    df3['high_level_ECM'] = df3['variable'].map(lambda x: x[:x.find('_')])
    df3['detail_level_ECM'] = df3['variable'].map(lambda x: x[x.find('_') + 1:])
    df4 = df3.drop('value', axis=1)
    df4.rename(columns={'variable': 'ECM_combined_header', 'Building ID_Building ID_Building ID': 'Building Number'}, inplace=True)
    df4['source'] = 'ScopePortfolioReport_20160105-5'
    print len(df4)
    df4.drop_duplicates(cols=['Building Number', 'ECM_combined_header'], inplace=True)
    print len(df4)
    df4.replace({'high_level_ECM': {'Indoor Environmental Quality': 'IEQ', 'Advanced Metering Systems': 'Advanced Metering', 'Building Tune-up or Utility Improvements': 'Building Tuneup or Utility Improvements'}}, inplace=True)
    df4.to_csv(homedir + 'master_table/ECM/ecm_detail_long.csv', index=False)
    df_no_ieq = df4[df4['high_level_ECM'] != 'IEQ']
    df_no_ieq['high_level_ECM_standard'] = df_no_ieq['high_level_ECM']
    df_ieq = df4[df4['high_level_ECM'] == 'IEQ']
    df_ieq['high_level_ECM_standard'] = df_ieq.apply(lambda r: 'HVAC' if r['detail_level_ECM'] == 'Thermal Comfort and Ventilation Measures' else 'Lighting', axis=1)
    df5 = pd.concat([df_ieq, df_no_ieq], ignore_index=True)
    df5.sort_values(['high_level_ECM_standard', 'detail_level_ECM', 'Building Number'], inplace=True)
    df5.to_csv(homedir + 'master_table/ECM/ecm_detail_long_tidy.csv', index=False)
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df5.to_sql('ecm_detail_long', conn, if_exists='replace')
    df5.rename(columns=lambda x: x.replace(' ', '_'), inplace=True)
    df_euas = gbs.intersect_EUAS(df5)
    with conn:
        df_euas.to_sql('EUAS_ecm_detail', conn, if_exists='replace')
    print 'end'
    return

def join_detail_high_ecm():
    conn = uo.connect('all')
    with conn:
        df_dt = pd.read_sql('SELECT * FROM EUAS_ecm_detail', conn)
    df_dt.drop('high_level_ECM', axis=1, inplace=True)
    df_dt.rename(columns={'high_level_ECM_standard':
                          'high_level_ECM'}, inplace=True)
    df_dt = df_dt[df_dt['detail_level_ECM'] != 'Thermal Comfort and Ventilation Measures']
    with conn:
        df_h = pd.read_sql('SELECT * FROM EUAS_ecm_highlevel', conn)
    df_h.rename(columns={'ECM_high_level_action': 'high_level_ECM'},
                inplace=True)
    df_h.replace({'high_level_ECM': {'IEQ': 'Lighting'}}, inplace=True)
    df_all = pd.merge(df_h, df_dt, how='left', on=['Building_Number','high_level_ECM'], suffixes=['_highlevel', '_detail'])
    df_all = df_all[df_all['high_level_ECM'] != 'Water']
    df_all = df_all[df_all['high_level_ECM'] != 'Renewable Energy']
    df_all['detail_level_ECM'] = df_all.apply(lambda r: 'GSALink' if r['high_level_ECM'] == 'GSALink' else r['detail_level_ECM'], axis=1)
    conn2 = uo.connect('other_input')
    with conn2:
        df_cts = pd.read_sql('SELECT * FROM CTSReport_ecm', conn2)
    df_cts.info()
    df_cts['high_level_ECM'] = df_cts['ecm_action'].map({
        'Other HVAC': 'HVAC', 
        'Lighting Improvements': 'Lighting', 
        'Building Automation Systems EMCS': 'HVAC',
        'Commissioning Measures': 'Building Tuneup or Utility Improvements', 
        'Advanced Metering Systems': 'Advanced Metering', 
        'Chiller Plant Improvements': 'HVAC', 
        'Boiler Plant Improvements': 'HVAC', 
        'Building Envelope Modifications': 'Building Envelope', 
        'Refrigeration': 'HVAC'})
    df_cts['detail_level_ECM'] = df_cts['ecm_action'].map(lambda x: x if x in ['Building Automation Systems EMCS', 'Commissioning Measures', 'Chiller Plant Improvements', 'Boiler Plant Improvements', 'Refrigeration'] else np.nan)
    df_cts.drop(['ecm_action', 'value'], axis=1, inplace=True)
    df_cts['source_highlevel'] = df_cts['high_level_ECM'].map(lambda x: 'CTS' if type(x) != float else np.nan)
    df_cts['source_detail'] = df_cts['detail_level_ECM'].map(lambda x: 'CTS' if type(x) != float else np.nan)
    df_cts.dropna(subset=['high_level_ECM', 'detail_level_ECM'], how='all', axis=0, inplace=True)
    with conn2:
        df_ami_from_covered = pd.read_sql('SELECT * FROM AMI_from_Covered_Facilities_All_Energy_mmBTUs_FY14_EISA07Sec432', conn2)
    df_all2 = pd.concat([df_ami_from_covered, df_cts, df_all], ignore_index=True)
    print
    print df_all2.head(n=15)
    print df_all2.tail(n=15)
    with conn:
        print '1111111'
        df_all2.to_sql('EUAS_ecm', conn, if_exists='replace')
    print 'end'
    conn.close()
    conn2.close()
    return
    
def read_cts_ecm():
    df = pd.read_csv(os.getcwd() + '/input/FY/ECM info/CTS/CTSReport_ecm.csv', skiprows=5)
    print len(list(df))
    df.dropna(axis=1, how='all', inplace=True)
    df.rename(columns={'Agency Designated Covered Facility ID': 'Building_Number'}, inplace=True)
    print len(list(df))
    cols = list(df)
    idx = cols.index('Total ECMs') + 1
    ecm_cols = cols[idx:]
    # NOTE: 'Date of Last Project Awarded' start? or stop?
    df = df[['Building_Number'] + ecm_cols]
    df2 = pd.melt(df, id_vars='Building_Number', value_vars=ecm_cols)
    df2.dropna(subset=['value'], inplace=True)
    df2.rename(columns={'variable': 'ecm_action'}, inplace=True)
    print df2['ecm_action'].value_counts()
    conn = uo.connect('other_input')
    with conn:
        df2.to_sql('CTSReport_ecm', conn, if_exists='replace')
    print 'end'

def read_ecm_program():
    conn = sqlite3.connect(homedir + 'db/other_input.db')
    df_leedeb = pd.read_csv(os.getcwd() + '/input/FY/ECM Program/LEED EB Building totals and years.csv')
    df_leedeb.rename(columns={'Sep-12': 'start_date', 'End date of LEED process': 'end_date', 'Building FRPID': 'Building_Number'}, inplace=True)
    df_gp = pd.read_csv(os.getcwd() + '/input/FY/ECM Program/LEED and GP buildings v1-sheetGP.csv')
    df_gp.rename(columns={'Building FRPID': 'Building_Number'}, inplace=True)
    df_gp = df_gp[['Building_Number', 'FY in compliance']]
    df_gp['Building_Number'] = df_gp['Building_Number'].map(lambda x: x.upper())
    with conn:
        df_leedeb.to_sql(util.filerename_sql('LEED EB Building totals and years'), conn, if_exists='replace')
        df_gp.to_sql(util.filerename_sql('LEED and GP buildings v1-sheetGP'), conn, if_exists='replace')
    df_leedeb.info()
    df_leedeb = df_leedeb[['Building_Number', 'start_date', 'end_date']]
    df_leedeb['ECM program'] = 'LEED_EB'
    df_leedeb['source'] = 'LEED EB Building totals and years'
    df_gp['ECM program'] = 'GP'
    df_gp['source'] = 'LEED and GP buildings v1-sheetGP'
    df_gp.drop('FY in compliance', axis=1, inplace=True)
    df_pro = pd.read_csv(os.getcwd() + \
                         '/input/FY/ECM Program/GSA_F15_EUAS_v2.2.csv')
    with conn:
        df_pro.to_sql('GSA_F15_EUAS_v2_2', conn, if_exists='replace')
    conn.close()
    programs = ['GP', 'LEED', 'first fuel', 'Shave Energy', 'E4',
                'ESPC']
    df_pro = df_pro[['Building ID'] + programs]
    df_pro.replace('1_Yes', 1, inplace=True)
    df_pro.replace('2_No', np.nan, inplace=True)
    df_pro.rename(columns={'Building ID': 'Building_Number'},
                  inplace=True)
    df_pro_long = pd.melt(df_pro, id_vars=['Building_Number'],
                          value_vars=programs)
    df_pro_long.dropna(subset=['Building_Number', 'value'],
                       inplace=True)
    df_pro_long.drop('value', axis=1, inplace=True)
    df_pro_long.rename(columns={'variable': 'ECM program'}, inplace=True)
    df_pro_long['source'] = 'GSA_F15_EUAS_v2.2'
    df_gsa = pd.read_csv(os.getcwd() + \
                         '/input/FY/GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates.csv')
    df_gsa = df_gsa[['Building ID']]
    df_gsa['GSALink'] = 1
    df_gsa.rename(columns={'Building ID': 'Building_Number'},
                  inplace=True)
    df_gsa_long = pd.melt(df_gsa, id_vars=['Building_Number'],
                          value_vars=['GSALink'])
    df_gsa_long.drop('value', axis=1, inplace=True)
    df_gsa_long.rename(columns={'variable': 'ECM program'}, inplace=True)
    df_gsa_long['source'] = 'GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates'
    df_pro_all = pd.concat([df_pro_long, df_gsa_long, df_leedeb, df_gp], ignore_index=True)
    df_pro_all.sort_values(['Building_Number', 'ECM program'], inplace=True)
    df1 = df_pro_all[df_pro_all['ECM program'].map(lambda x: 'LEED' in x)]
    df1.info()
    df11 = df1.groupby('Building_Number').agg({'ECM program': lambda x: 'LEED_EB' if 'LEED_EB' in x.tolist() else 'LEED_NC'})
    print df11.head()
    df11.reset_index(inplace=True)
    df111 = pd.merge(df11, df_leedeb[['Building_Number', 'start_date', 'end_date', 'source']], on='Building_Number', how='left')
    df111['source'] = df111.apply(lambda r: 'GSA_F15_EUAS_v2.2' if r['ECM program'] == 'LEED_NC' else r['source'], axis=1)
    df2 = df_pro_all[df_pro_all['ECM program'].map(lambda x: not 'LEED' in x)]
    df_all = pd.concat([df111, df2])
    df_all = df_all[['Building_Number', 'ECM program', 'start_date', 'end_date', 'source']]
    conn2 = sqlite3.connect(homedir + 'db/all.db')
    df_euas = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly', conn2)
    df_all.rename(columns={'Building Number': 'Building_Number'},
                      inplace=True)
    df = pd.merge(df_euas, df_all, on='Building_Number', how='left')
    df.to_csv(homedir + 'master_table/EUAS_ecm_program.csv',
              index=False)
    with conn2:
        df.to_sql('EUAS_ecm_program', conn2, if_exists='replace')
    conn2.close()
    print 'end'
    return
    
def get_area():
    conn = sqlite3.connect(homedir + 'db/all.db')
    with conn:
        df = pd.read_sql('SELECT Building_Number, Fiscal_Year, [Gross_Sq.Ft], Cat FROM EUAS_monthly', conn)
    df2 = df.groupby(['Building_Number', 'Fiscal_Year']).agg({'Gross_Sq.Ft': 'mean', 'Cat': 'first'})
    df2.reset_index(inplace=True)
    print df2.head()
    with conn:
        df2.to_sql('EUAS_area_cat', conn, if_exists='replace')
    print 'end'
    return
    
def dump_covered():
    conn = uo.connect('other_input')
    df1 = pd.read_csv(os.getcwd() + '/input/FY/static info/Covered_Facilities_All Energy mmBTUs-sheetF13.csv', skiprows=4, skipfooter=14)
    df1.info()
    df1.rename(columns={'Building': 'Building_Number'}, inplace=True)
    print
    print df1.head()
    print df1.tail()
    with conn:
        df1.to_sql(util.filerename_sql('Covered_Facilities_All Energy mmBTUs-sheetF13'), conn, if_exists='replace')

    df2 = pd.read_csv(os.getcwd() + '/input/FY/static info/Covered_Facilities_All Energy mmBTUs-sheetF14.csv', skiprows=6, skipfooter=0)
    print
    print df2.head()
    print df2.tail()
    df2.rename(columns={'Facility ID': 'Building_Number'}, inplace=True)
    with conn:
        df2.to_sql(util.filerename_sql('Covered_Facilities_All Energy mmBTUs-sheetF14'), conn, if_exists='replace')
    files = glob.glob(os.getcwd() + '/input/FY/covered/*.csv')
    for f in files:
        filename = f[f.rfind('/') + 1:]
        label = filename[:filename.rfind('.')]
        df = pd.read_csv(f, skiprows=5).dropna(axis=1, how='all')
        df.rename(columns={'Agency Designated Covered Facility ID': 'Building_Number'}, inplace=True)
        with conn:
            df.to_sql(label, conn, if_exists='replace')
    conn.close()
    print 'end'
    
def read_ami_from_covered():
    df = pd.read_csv(os.getcwd() + '/input/FY/covered/Covered_Facilities_All Energy mmBTUs_FY14_EISA07Sec432_input.csv')
    df['Facility_Number'] = df['Facility_Number'].map(lambda x: np.nan if type(x) == float else x[:8])
    df_lookup = df.copy()
    df_lookup = df_lookup[['Building_Number', 'Facility_Number']]
    print df_lookup.head(n=10)
    df_lookup.dropna(axis=0, how='all', inplace=True)
    print df_lookup.head(n=10)
    df_lookup['Facility_Number'] = df_lookup['Facility_Number'].ffill()
    print df_lookup.head(n=10)
    df_lookup.dropna(subset=['Building_Number'], axis=0, inplace=True)
    df.replace({'Electric': {'--': np.nan}, 'Gas': {'--': np.nan}},
               inplace=True)
    df.dropna(subset=['Electric', 'Gas'], axis=0, inplace=True)
    df = df[(df['Electric'] > 0) & (df['Gas'] > 0)]
    print
    print df.head(n=20)
    facilities = set(df['Facility_Number'].tolist())
    buildings = set(df['Building_Number'].tolist())
    lookup = dict(zip(df_lookup['Building_Number'],
                      df_lookup['Facility_Number']))
    f_from_building = set([lookup[x] for x in buildings if x in
                           lookup])
    all_ids = buildings.union(facilities.union(f_from_building))
    df2 = pd.DataFrame({'Building_Number': list(all_ids)})
    df2.dropna(subset=['Building_Number'], axis=0, inplace=True)
    df2['high_level_ECM'] = 'Advanced Metering'
    df2['detail_level_ECM'] = np.nan
    df2['source_highlevel'] = 'Covered_Facilities_All Energy mmBTUs_FY14_EISA07Sec432_input'
    df2['source_detail'] = np.nan
    conn = uo.connect('other_input')
    with conn:
        df2.to_sql('AMI_from_Covered_Facilities_All_Energy_mmBTUs_FY14_EISA07Sec432', conn, if_exists='replace')
    conn.close()
        
def concat_covered():
    conn = uo.connect('other_input')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number FROM Covered_Facilities_All_Energy_mmBTUs_sheetF13', conn)
        df2 = pd.read_sql('SELECT DISTINCT Building_Number FROM Covered_Facilities_All_Energy_mmBTUs_sheetF14', conn)
        df3 = pd.read_sql('SELECT DISTINCT Building_Number, [Covered_Facility?] FROM Entire_GSA_Building_Portfolio_input', conn)
    dfs = []
    for year in range(2008, 2016):
        with conn:
            label = 'CTSReport_covered{0}'.format(year)
            df = pd.read_sql('SELECT DISTINCT Building_Number FROM {0}'.format(label), conn)
            df['source'] = label
        dfs.append(df)
    conn.close()
    df1['source'] = 'Covered_Facilities_FY13'
    df2['source'] = 'Covered_Facilities_FY14'
    df3 = df3[df3['Covered_Facility?'] == 'Yes']
    df3 = df3[['Building_Number']]
    df3['source'] = 'Entire_GSA_Building_Portfolio'
    df_all = pd.concat([df1, df2, df3] + dfs, ignore_index=True)
    df_all.sort_values('Building_Number', inplace=True)
    df = gbs.intersect_EUAS(df_all)
    conn = uo.connect('all')
    with conn:
        df.to_sql('covered_facility', conn, if_exists='replace')
    print 'end'
    return
    
def get_stat_small_area():
    df = check_small_area(1000)
    buildings = df['Building_Number'].unique()
    dfs = []
    for b in buildings:
        df_temp = view_building(b)
        dfs.append(df_temp)
    df_all = pd.concat(dfs, ignore_index=True)
    print df_all.head()
    df_all.to_csv(homedir + 'temp/small_area.csv', index=False)
    df2 = df_all[df_all['eui_total'] == np.inf]
    print (df2['Building_Number'].unique())
    print 'end'
    
def transit_weather():
    conn_w = uo.connect('weather_hourly_utc')
    with conn_w:
        df_w = pd.read_sql('SELECT * FROM weather_ave', conn_w)
        df_c = pd.read_sql('SELECT * FROM weather_cdd65', conn_w)
        df_h = pd.read_sql('SELECT * FROM weather_hdd65', conn_w)
    conn_w.close()
    conn = uo.connect('all')
    with conn:
        df_w.to_sql('weather_ave', conn, if_exists='replace')
        df_c.to_sql('weather_cdd65', conn, if_exists='replace')
        df_h.to_sql('weather_hdd65', conn, if_exists='replace')
    print 'end'
    conn.close()
    return
    
def get_facility_id():
    df = pd.read_csv(os.getcwd() + '/input/FY/static info/buildings_in_facility_fy15.csv', skiprows=6, names=['Region_Number', 'Facility_Number', 'Building_Number', 'Facility_total_gsf', 'Building_gsf'])
    conn = uo.connect('all')
    with conn:
        df.to_sql('building_facility', conn, if_exists='replace',
                  index=False)
    conn.close()
    print 'end'
    return
    
def good_energy():
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT * FROM EUAS_monthly', conn)
        df2 = pd.read_sql('SELECT * FROM eui_by_fy', conn)
    eng_set = gbs.get_energy_set('eui')
    print len(df1), len(df2)
    df1 = df1[df1['Building_Number'].isin(eng_set)]
    df2 = df2[df2['Building_Number'].isin(eng_set)]
    print len(df1), len(df2)
    with conn:
        df1.to_sql('EUAS_monthly_high_eui', conn, if_exists='replace')
        df2.to_sql('eui_by_fy_high_eui', conn, if_exists='replace')
    print 'end'
    conn.close()
    
def comb_pro_act():
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number, ECM_program AS investment FROM EUAS_ecm_program WHERE ECM_program != \'GSALink\'', conn)
        df2 = pd.read_sql('SELECT DISTINCT Building_Number, high_level_ECM AS investment FROM EUAS_ecm', conn)
    df = pd.concat([df1, df2], ignore_index=True)
    print len(df)
    df = df[df['investment'].notnull()]
    print len(df)
    with conn:
        df.to_sql('EUAS_invest_nona', conn, if_exists='replace')
    print 'end'
    conn.close()
    
def update_source_other_input():
    conn = uo.connect('other_input')
    df = pd.read_csv('/media/yujiex/work/GSA/merge/csv_FY/other_input_source.csv')
    with conn:
        df.to_sql('source', conn, if_exists='replace', index=False)
    conn.close()
    print 'updated source table in other_input.db'
    
def dump_station_id_translation():
    conn = uo.connect('other_input')
    df = pd.read_csv(weatherdir + 'noaa/isd-history.csv')
    with conn:
        df.to_sql('weather_station_id_translation', conn, if_exists='replace', index=True)
    conn.close()
    print 'dumped weather_station_id_translation'

def gather_stations():
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT ICAO FROM EUAS_monthly_weather')
    conn.close()
    # conn2 = uo.connect('other_input')
    df1.head()

def temp_check():
    conn = uo.connect('all')
    with conn:
        df1 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly', conn)
        df2 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_monthly_weather', conn)
    print(len(set(df1['Building_Number'])))
    print(len(set(df2['Building_Number'])))
    print(len(set(df1['Building_Number']).difference(set(df2['Building_Number']))))

def main():
    # temp_check()
    # interval_availability()
    # update_source_other_input()
    # dump_station_id_translation()
    # gather_stations()

    # timezone2db()
    # get_bd_timezone_output()
    # comb_pro_act()
    # good_energy()
    # get_facility_id()
    # download_weather_sep_db_gsa(False)
    # gsalink_address_geocoding()
    # show_covered_exception()
    # print len(df[df['Gross_Sq.Ft'] == 0])
    # print (df['Gross_Sq.Ft'].min())
    # print df[df['Gross_Sq.Ft'] == 2730.0]
    # get_stat_small_area()
    # check_small_area(1000)
    # df = view_building('PA0600ZZ', 'Gas_(kBtu)')
    # df = view_building('MA0011ZZ', 'Electricity_(KWH)')
    # df = df[df['year'] == 2015]
    # print
    # print df[['Region_No.', 'month', 'Electricity_(KWH)']]
    # print df['Gross_Sq.Ft'].value_counts()
    # view_building('AX0002RE')
    # view_building('MT0000WC')
    # view_building('ND0000FT')
    # view_building('ND0000NG')
    # check_change_area()
    # check_change_cat()
    # read_ami_from_covered()
    # copy_tables()
    # compare()
    input_energy()
    # use this to upload new data
    # input_energy_18format()
    # fixme: get new weather data
    # db_prefix = 'weather_hourly_utc'
    # step = 'H'
    # db_prefix = 'weather_monthly'
    # step = 'M'
    # fixme: pi system weather data froze till May 2017
    # get_weather(False, step, db_prefix)
    # aggregate('Temperature_Hour_UTC', 'cdd65')
    # aggregate_sep_station('cdd65')
    # aggregate_sep_station('hdd65')
    # aggregate_sep_station('ave')
    # transit_weather()
    # join_energy_weather()
    # db_view('all_back', False)
    # dump_ecm()
    # read_ecm_highlevel_long()
    # read_ecm_detail_long()
    # read_cts_ecm()
    # join_detail_high_ecm()
    # db_view_alltable('all')
    # read_ecm_program()
    # db_view('all', False, 'EUAS_monthly')
    # check()
    # get_use()
    # dump_covered()
    # concat_covered()
    # db_view('other_input', True)
    return

main()

# print df.groupby(['high_level_ECM', 'detail_level_ECM']).count()[['Building_Number']]

# conn = uo.connect('all')
# with conn:
#     df = pd.read_sql('SELECT Building_Number, high_level_ECM FROM EUAS_ecm', conn)
#     df = df[df['high_level_ECM'].map(lambda x: x != None)]
#     print len(df)
#     df2 = df.groupby('Building_Number').filter(lambda x: len(x) == 1)
#     print len(df2)
#     print
#     print df2[df2['high_level_ECM'] == 'Building Tuneup or Utility Improvements']
#     # print df2.head()
    
# conn.close()

# conn = sqlite3.connect(homedir + 'db/all.db')
# buildings = gbs.get_all_building_set()
# singles = [x for x in buildings if not '0000' in x]
# print len(buildings), len(singles)

# df = pd.read_sql('SELECT * FROM EUAS_ecm_program', conn)
# buildings = gbs.get_all_building_set()
# df = df[df['Building_Number'].isin(buildings)]
# print df.groupby('ECM_program').count()[['Building_Number']]

# conn = sqlite3.connect(homedir + 'db/all.db')
# df = pd.read_sql('SELECT * FROM EUAS_monthly WHERE Building_Number = \'AZ0000WW\'', conn)
# df2 = df[['Fiscal_Year', 'Fiscal_Month', 'Gas_(kBtu)', 'Electric_(kBtu)']].groupby('Fiscal_Year').sum()
# print df2
# df2['Total'] = df2['Gas_(kBtu)'] + ['Electric_(kBtu)']
# print df2['Total']

# df = pd.read_sql('SELECT Building_Number, Fiscal_Year, eui_gas FROM eui_by_fy WHERE Building_Number = \'AZ0000WW\'', conn)
# print df
# df.to_csv(homedir + 'temp/zero_gas.csv', index=False)

# df = pd.read_sql('SELECT Building_Number, Fiscal_Year, [Gross_Sq.Ft] FROM EUAS_area', conn)
# df.to_csv(homedir + 'temp/area.csv', index=False)

# conn = sqlite3.connect(homedir + 'db/all.db')
# df = pd.read_sql('SELECT * FROM EUAS_monthly', conn)
# df = df[['Building_Number', 'Fiscal_Year', 'Fiscal_Month', 'Gas_(kBtu)']]
# print df[(df['Building_Number'] == 'CA0194ZZ') & (df['Fiscal_Year'] == 2010)]

# df2 = df.groupby(['Building_Number', 'Fiscal_Year']).sum()
# df2.drop('Fiscal_Month', axis=1, inplace=True)
# df2 = df2[df2['Gas_(kBtu)'] == 0]
# df2.info()
# df2.reset_index(inplace=True)
# print
# print df2[df2['Building_Number'] == 'CA0194ZZ']
# # df2.to_csv(homedir + 'temp/zero_gas.csv', index=False)

# conn = sqlite3.connect(homedir + 'db/other_input.db')
# df2 = pd.read_sql('SELECT DISTINCT Building_Number, [Covered_Facility?] FROM Entire_GSA_Building_Portfolio_input', conn)
# df2.info()
# df2 = df2[df2['Covered_Facility?'] == 'Yes']
# print len(df2)
# buildings = gbs.get_all_building_set()
# covered = set(df2['Building_Number'].tolist())
# common = covered.intersection(buildings)
# print len(common)
# print list(common)[:5]
# b = 'NJ0000CT'
# print '{0}'.format(b) in covered
# print '{0}'.format(b) in buildings

# conn = sqlite3.connect(homedir + 'db/all.db')
# df2 = pd.read_sql('SELECT DISTINCT Building_Number FROM EUAS_category', conn)
# df2.to_csv(homedir + 'temp/all_buildings.csv', index=False)
