import glob
import os
import pandas as pd
import numpy as np
import datetime
import geocoder
from vincenty import vincenty
import time
import lean_temperature_vari_step as lt

fldir = os.getcwd() + '/input/FL/'

def read_energy_fl_clean():
    print 'reading energy fl ...'
    filelist = glob.glob(fldir + 'input/excel_energy/cleaned/*.xlsx')
    fs = []
    st = []
    bd = []
    total = []
    unique = []
    for f in filelist[8:]:
        filename = f[f.rfind('/') + 1:]
        print '[% -- read', filename
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
        print 'write to file {0}.csv'.format(outfile)
        df_r.to_csv(fldir + 'csv_energy_hour/{0}.csv'.format(outfile))
    return

def get_lat_long(joinkey, address, fd):
    g = geocoder.google(address)
    if not (g.json['ok']):
        fd.write('"{0}",{1},{2}\n'.format(joinkey, 
                                        'Not Found', 'Not Found'))
    else:
        latlng = g.latlng
        fd.write('"{0}",{1},{2}\n'.format(joinkey, latlng[0], latlng[1]))

def geocode():
    print 'geocoding ...'
    filelist = glob.glob(fldir + 'csv_energy_hour/*.csv')
    addresses = [x[x.rfind('/') + 1:x.find('-') - 1] for x in filelist]
    df = pd.DataFrame({'address': addresses})
    df['street_num'] = df['address'].map(lambda x: x[:x.find(' ')])
    df_citystate = pd.read_csv(fldir + 'input/area_square footage.csv')
    df_citystate = df_citystate[['Address', 'City, State, Zip', 
                                 'Gross Floor Area(sf)']]
    df_citystate['street_num'] = df_citystate['Address'].map(lambda x: x[:x.find(' ')])
    df_all = pd.merge(df, df_citystate, how='left', on='street_num')
    df_all['geocoding_input'] = df_all.apply(lambda r: '{0}, {1}'.format(r['address'], r['City, State, Zip']), axis=1)
    print 'write to file address_citystate.csv'
    df_all.to_csv(fldir + 'address_citystate.csv', index=False)
    geocoding_inputs = df_all['geocoding_input'].tolist()
    with open (fldir + 'geocoding.txt', 'w+') as wt:
        for x in geocoding_inputs:
            get_lat_long(x, x, wt)
            time.sleep(0.1)
    df_geo = pd.read_csv(fldir + 'geocoding.txt', header=None,
                         names=['geocoding_input', 'Latitude',
                                'Longitude'])
    print 'write to file geocoding_latlng.csv'
    df_geo.to_csv(fldir + 'geocoding_latlng.csv', index=False)
    return

def get_station_id():
    df_geo = pd.read_csv(fldir + 'geocoding_latlng.csv')
    df_geo['station_id'] = df_geo.apply(lambda r: get_station_latlng((r['Latitude'], r['Longitude']))[0], axis=1)
    df_geo['distance'] = df_geo.apply(lambda r: get_station_latlng((r['Latitude'], r['Longitude']))[1], axis=1)
    print 'write to file latlng_station.csv'
    df_geo.to_csv(fldir + 'latlng_station.csv', index=False)
    return

def get_station_latlng(latlng):
    if latlng == None:
        print 'No input to get_station'
        return None
    df_lookup = pd.read_csv(fldir + 'Weather Data Mapping to Use.csv')
    lat = latlng[0]
    lng = latlng[1]
    df_lookup['distance'] = df_lookup.apply(lambda r: vincenty((lat, lng), (r['Latitude'], r['Longitude']), miles=True), axis=1)
    min_distance = df_lookup['distance'].min()
    df_temp = df_lookup[df_lookup['distance'] == min_distance]
    icao = df_temp['StationID'].tolist()[0]
    distance = df_temp['distance'].tolist()[0]
    return (icao, distance)
    
# BOOKMARK: read_weather_data_with start and end time
def join_station_weather():
    df_bd = pd.read_csv(fldir + 'address_citystate.csv')
    df_station = pd.read_csv(fldir + 'latlng_station.csv')
    df_static = pd.merge(df_bd, df_station, on='geocoding_input', how='left')
    print 'write to file bd_station.csv'
    df_static.to_csv(fldir + 'bd_station.csv', index=False)
    bs_pair = zip(df_static['address'], df_static['station_id'])
    df_static.set_index('address', inplace=True)
    df_static.drop_duplicates(inplace=True)
    counter = 0
    for (b, s) in bs_pair[7:]:
        print counter
        print b, s
        area = df_static.ix[b, 'Gross Floor Area(sf)']
        print area
        try:
            df_energy = pd.read_csv(fldir + \
                                    'csv_energy_hour/{0} - clean.csv'.format(b))
        except IOError:
            print 'file not exist'
            continue
        if len(df_energy) == 0:
            print 'empty input energy data'
            continue
        mintime = df_energy['Time Stamp'].min()
        maxtime = df_energy['Time Stamp'].max()
        df_weather = lt.get_weather_data(s, mintime, maxtime, 'hourly')
        if len(df_weather) == None:
            df_weather = lt.get_weather_data('KPTW', mintime, maxtime, 'hourly')
        df_energy[s] = df_weather[s]
        print type(area), area
        if type(area) != np.float64:
            print 'wrong type for floor area'
            continue
        if area <= 0:
            print 'non-positive floor area'
            continue
        df_energy['eui_elec'] = df_energy['Meter Total'] * 3.412/area
        # BOOKMARK error
        df_energy['Time Stamp'] = pd.to_datetime(df_energy['Time Stamp'])
        df_energy.set_index('Time Stamp', inplace=True)
        df_energy.index.name = 'Time Stamp'
        df_energy = df_energy.resample('M', how={s: np.mean, 'eui_elec': np.sum})
        # lt.piecewise_reg_one(b, s, 3, 'eui_elec', 'Time Stamp',
        #                      df_energy)
        d_elec = lt.piecewise_reg_one(b, s, 3, 'eui_elec', 'Time Stamp', df_energy)
        print d_elec['breakpoint']
        lt.plot_lean_one(b, s, 'elec', elec=d_elec)
        counter += 1
    return

def main():
    # read_energy_fl_clean()
    # geocode()
    # get_station_id()
    join_station_weather()
    return
    
main()
