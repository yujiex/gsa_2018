from datetime import datetime
import numpy as np
import pandas as pd
import geocoder
import os
import re
import calendar
import requests
from vincenty import vincenty
homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/' 
interval_dir = os.getcwd() + '/input/FY/interval/'

def month_of_season(season):
    switcher = {
        'winter': [12, 1, 2],
        'spring': [3, 4, 5],
        'summer': [6, 7, 8],
        'fall': [9, 10, 11]
    }
    return switcher.get(season, "nothing")

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

# s can be building or weather station, just an identifier of the
# latlng
def get_timezone(lat, lng, s, i=0):
    # timestamp is not important
    url = 'https://maps.googleapis.com/maps/api/timezone/json?location={0},{1}&timestamp={2}'.format(lat, lng, 1254355200)
    r = requests.get(url)
    if r.json()['status'] == 'ZERO_RESULTS':
        print ','.join([str(i), s, lat, lng, 'nan', 'nan'])
        # return np.nan
    else:
        zoneId = r.json()['timeZoneId']
        offset = r.json()['rawOffset']
        print ','.join([str(i), s, str(lat), str(lng), zoneId, str(offset)])
        # return zoneId, offset

def float2rgb(x):
    return "rgb" + str(tuple([int(x[i] * 255) for i in range(3)]))
    
def float2hex(x):
    return '#' + ''.join(['{0:02X}'.format(int(x[i] * 255)) for i in range(3)])

def pal2rgb(pal):
    pal_str = str([float2rgb(x) for x in pal] + ["gray"])
    pal_str = pal_str.replace("'", "\"")
    return pal_str

def filerename_sql(string):
    string = string.replace(' ', '_')
    string = string.replace('-', '_')
    return string

def describe_table(conn, table, head):
    df = pd.read_sql('SELECT * FROM {0}'.format(table), conn)
    df.info()
    if head:
        print df.head()
    return df

def get_list_tables(cursor):
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = [x[0] for x in cursor.fetchall()]
    return tables

def timing(ori, current, funname):
    print '{0} takes {1}s...'.format(funname, current - ori)
    return current

# get station id for "b"
def get_station_dist(b, latlng, length):
    if latlng == None:
        print 'No input to get_station for {0}'.format(b)
        return None
    df_lookup = pd.read_csv(os.getcwd() + \
                            '/input/Weather Data Mapping to Use.csv')
    lat = latlng[0]
    lng = latlng[1]
    print b
    df_lookup['distance'] = df_lookup.apply(lambda r: vincenty((lat, lng), (r['Latitude'], r['Longitude']), miles=True), axis=1)
    df_lookup_sort = df_lookup.sort_values(by='distance')
    df_lookup_sort = df_lookup_sort.head(n=length)
    icao = df_lookup_sort['StationID'].tolist()
    distance = df_lookup_sort['distance'].tolist()
    return zip(icao, distance)

def get_month_lastday(year, month):
    return calendar.monthrange(year, month)[-1]

def get_lat_long(address):
    g = geocoder.google(address)
    if not (g.json['ok']):
        print 'Address not found'
        return None
    else:
        latlng = g.latlng
        # print '{0},{1},{2}'.format(address, latlng[0], latlng[1])
        return latlng

def get_state_abbr_dict():
    df = pd.read_csv(os.getcwd() + '/input/FY/state2abbr.csv')
    return dict(zip(df['Postal'], df['State']))

# fiscal year to calendar year
def fiscal2calyear(y, m):
    if m < 4:
        return y - 1
    else:
        return y

# fiscal month to calendar month
def fiscal2calmonth(m):
    m = m + 9
    if m < 13:
        return m
    else:
        return m % 12

def read_building_eui(b, timestep):
    if timestep == 'Y':
        df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    elif timestep == 'M':
        df = pd.read_csv(master_dir + 'energy_eui_monthly.csv')
    elif timestep == 'D':
        df = pd.read_csv(interval_dir +
                         'single/{0}.csv'.format(b))
    elif timestep == 'H':
        df = pd.read_csv(interval_dir +
                         'single_hourly/{0}.csv'.format(b))
    df = df[df['Building Number'] == b]
    return df

def CVRMSE(y, y_hat, n_par):
    n = len(y)
    return np.sqrt((np.subtract(y_hat, y) ** 2).sum() / (n - n_par))/np.array(y).mean()

def get_filename(path):
    return path[path.rfind('/') + 1:]

def str_represent_int(string):
    try:
        int(string)
        return True
    except ValueError:
        return False

def get_cf_indicator(string):
    if string[0] == 'F':
        return 'Fiscal Year'
    elif string[0] == 'C': 
        return 'year'
    elif str_represent_int(string[0]):
        return 'timestamp'
    else:
        print 'illegal time specification'
        return None

def get_time_filter(timerange):
    tokens = timerange.split(' ')
    if len(tokens) < 1:
        print 'illegal time range expression'
        return None
    elif len(tokens) == 1:
        yearcol = get_cf_indicator(tokens[0])
        ab = int(tokens[0][2:])
        print ab
        return (yearcol, lambda x: x == ab)
    elif len(tokens) == 2:
        if tokens[0] == 'before':
            yearcol = get_cf_indicator(tokens[1])
            if yearcol == 'Fiscal Year' or yearcol == 'year':
                b = int(tokens[1][2:])
            elif yearcol == 'timestamp':
                b = datetime.strptime(tokens[1], '%Y-%m-%d')
            return (yearcol, lambda x: x < b)
        elif tokens[0] == 'after':
            yearcol = get_cf_indicator(tokens[1])
            if yearcol == 'Fiscal Year' or yearcol == 'year':
                a = int(tokens[1][2:])
            elif yearcol == 'timestamp':
                a = datetime.strptime(tokens[1], '%Y-%m-%d')
            return (yearcol, lambda x: x > a)
        else:
            print 'illegal time range expression'
            return None
    elif len(tokens) == 3:
        a = datetime.strptime(tokens[0], '%Y-%m-%d')
        b = datetime.strptime(tokens[-1], '%Y-%m-%d')
        return ('timestamp', lambda x: x < b and x > a)
    elif len(tokens) == 5:
        yearcol = get_cf_indicator(tokens[1])
        b = int(tokens[1][2:])
        a = int(tokens[4][2:])
        return (yearcol, lambda x: x < b and x > a)
    else:
        print 'illegal time range expression'
        return None

def get_time_filter_dep(timerange):
    tokens = timerange.split(' ')
    if len(tokens) < 1:
        print 'illegal time range expression'
        return None
    elif len(tokens) == 1:
        yearcol = get_cf_indicator(tokens[0])
        ab = int(tokens[0][2:])
        print ab
        return (yearcol, lambda x: x == ab)
    elif len(tokens) == 2:
        if token[0] == 'before':
            yearcol = get_cf_indicator(tokens[1])
            b = int(tokens[1][2:])
            return (yearcol, lambda x: x < b)
        elif token[0] == 'after':
            yearcol = get_cf_indicator(tokens[1])
            a = int(tokens[1][2:])
            return (yearcol, lambda x: x > a)
        else:
            print 'illegal time range expression'
            return None
    elif len(tokens) == 5:
        yearcol = get_cf_indicator(tokens[1])
        b = int(tokens[1][2:])
        a = int(tokens[4][2:])
        return (yearcol, lambda x: x < b and x > a)
    else:
        print 'illegal time range expression'
        return None
