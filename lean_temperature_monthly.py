# uncomment lines containing "marker_style" to show real data scatter plot
import os
import glob
import pandas as pd
import numpy as np
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
from scipy import optimize
import requests
import textwrap as tw
import time
import geocoder
import json
import util_io as uo
# from geopy.distance import vincenty
from vincenty import vincenty
import calendar

import util

weatherdir = os.getcwd() + '/csv_FY/weather/'
# image_output_dir = os.getcwd() + '/plot_FY_weather/lean/'
image_output_dir = os.getcwd() + '/plot_FY_weather/html/single_building/lean_piecewise/'
# image_output_dir = os.getcwd() + '/input/FY/interval/ion_0627/lean/'
# image_output_dir = os.getcwd() + '/plot_FY_weather/html/single_building/lean_interval/'
# json_output_dir = os.getcwd() + '/plot_FY_weather/lean_piecewise/json/'
json_output_dir = os.getcwd() + '/plot_FY_weather/lean/json/'

# label for plotting
ylabel_dict = {'combined':'Electricity + Gas [kBtu/sq.ft]',
               'elec':'Electricity Conditioning [kBtu/sq.ft]',
               'base_elec':'Base Electric Load [kBtu/sq.ft]',
               'base_gas':'Base Gas Load [kBtu/sq.ft]',
               'gas':'Gas Conditioning [kBtu/sq.ft]'}

title_dict = {'combined':'Electricity + Gas',
              'elec':'Electricity Conditioning',
              'base_elec':'Base Elec Load',
              'base_gas':'Base Gas Load',
              'gas':'Gas Conditioning'}

def plot_lean_one_fromdb(b, s, side, timerange, plotPoint=False, **kwargs):
    print 'creating {0} LEAN for building {1} {2} ...'.format(side, b, timerange)
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("paper", font_scale=0.8)
    gas_line_color = '#DE4A50'
    gas_mk_color = '#DE4A50'
    elec_line_color = '#429CD5'
    elec_mk_color = '#429CD5'
    base_gas_color = 'orange'
    base_elec_color = 'yellow'
    alpha = 0.5
    plt.figure(figsize=(3, 3), dpi=150, facecolor='w', edgecolor='k')
    bx = plt.axes()
    if side == 'elec' or side == 'gas':
        if kwargs[side] == None:
            return None
        t = kwargs[side]['x_range']
        t_min = t[0]
        t_max = t[1]
        x_min = t_min
        x_max = t_max
        par = kwargs[side]['regression_par']
        k = par[0]
        intercept = par[1]
        if type(kwargs[side]['breakpoint']) == int:
            breakpoint = kwargs[side]['breakpoint']
            xd = np.array([x_min, breakpoint, breakpoint, x_max])
        else:
            break_left = kwargs[side]['breakpoint'][0]
            break_right = kwargs[side]['breakpoint'][1]
            breakpoint = break_left
            xd = np.array([x_min, break_left, break_right, x_max])
        base = k * breakpoint + intercept
        yd = kwargs[side]['fun'](xd, *par) - base
        if side == 'gas':
            plt.plot(xd, yd, gas_line_color)
            bx.fill_between(xd, 0, yd, facecolor=gas_line_color,
                            alpha=alpha)
            rug_x = kwargs[side]['x']
            rug_x = [x for x in rug_x if x < breakpoint]
            # sns.rugplot(rug_x, ax=bx, color=gas_line_color)
            output = (xd, yd)
        elif side == 'elec':
            plt.plot(xd, yd, elec_line_color)
            bx.fill_between(xd, 0, yd, facecolor=elec_line_color,
                            alpha=alpha)
            rug_x = kwargs[side]['x']
            try: 
                break_left
                rug_x = [x for x in rug_x if x > break_right or x <
                         break_left]
            except NameError:
                rug_x = [x for x in rug_x if x > breakpoint]
            # sns.rugplot(rug_x, ax=bx, color=elec_line_color)
            output = (xd, yd)
        if 'y_upper' in kwargs:
            plt.ylim((0, kwargs['y_upper']))
        else:
            plt.ylim((0, max(yd) * 1.1))
    else:
        if kwargs['gas'] != None:
            t = kwargs['gas']['x_range']
        elif kwargs['elec'] != None:
            t = kwargs['elec']['x_range']
        else:
            return None
        t_min = t[0]
        t_max = t[1]
        if kwargs['gas'] != None:
            x_gas = kwargs['gas']['x']
            y_gas = kwargs['gas']['y']
            par_gas = kwargs['gas']['regression_par']
            intercept_gas = par_gas[1]
            k_gas = par_gas[0]
            breakpoint_gas = kwargs['gas']['breakpoint']
            xd_gas = np.array([t_min, breakpoint_gas, t_max])
            base_gas = k_gas * breakpoint_gas + intercept_gas
        if kwargs['elec'] != None:
            x_elec = kwargs['elec']['x']
            y_elec = kwargs['elec']['y']
            par_elec = kwargs['elec']['regression_par']
            k_elec = par_elec[0]
            intercept_elec = par_elec[1]
            if type(kwargs['elec']['breakpoint']) == int:
                breakpoint_elec = kwargs['elec']['breakpoint']
                xd_elec = np.array([t_min, breakpoint_elec, t_max])
            else:
                break_elec_left = kwargs['elec']['breakpoint'][0]
                break_elec_right = kwargs['elec']['breakpoint'][1]
                breakpoint_elec = break_elec_left
                xd_elec = np.array([t_min, break_elec_left,
                                    break_elec_right, t_max])
            base_elec = k_elec * breakpoint_elec + intercept_elec
        if kwargs['gas'] != None and kwargs['elec'] != None:
            yd_elec = (kwargs['elec']['fun'](xd_elec, *par_elec)) + base_gas
            yd_gas = (kwargs['gas']['fun'](xd_gas, *par_gas)) + base_elec
        elif kwargs['gas'] != None:
            yd_gas = (kwargs['gas']['fun'](xd_gas, *par_gas))
        elif kwargs['elec'] != None:
            yd_elec = (kwargs['elec']['fun'](xd_elec, *par_elec))
        else:
            return None
        t_min = t[0]
        t_max = t[1]
        # xd = np.linspace(t_min, t_max, 150)
        if side == 'combined':
            marker_style = "o"
            marker_size = 2
            if kwargs['gas'] != None and kwargs['elec'] != None:
                plt.plot(xd_gas, yd_gas, gas_line_color)
                plt.plot(xd_elec, yd_elec, elec_line_color)
                if (plotPoint):
                    plt.plot(x_gas, y_gas + base_elec, marker_style, markerfacecolor=gas_mk_color, ms=marker_size)
                    plt.plot(x_elec, y_elec + base_gas, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
                bx.fill_between(xd_elec, base_elec + base_gas, yd_elec,
                                facecolor=elec_line_color, alpha=alpha)
                bx.fill_between(xd_gas, base_elec + base_gas, yd_gas,
                                facecolor=gas_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_elec,
                                facecolor=base_elec_color, alpha=alpha)
                bx.fill_between(xd_elec, base_elec, base_elec + base_gas,
                                facecolor=base_gas_color, alpha=alpha)
                output = (xd_elec, yd_elec, xd_gas, yd_gas, base_elec,
                          base_gas)
            elif kwargs['gas'] != None:
                plt.plot(xd_gas, yd_gas, gas_line_color)
                if (plotPoint):
                    plt.plot(x_gas, y_gas, marker_style, markerfacecolor=gas_mk_color, ms=marker_size)
                bx.fill_between(xd_gas, base_gas, yd_gas,
                                facecolor=gas_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_gas,
                                facecolor=base_gas_color, alpha=alpha)
                output = (xd_gas, yd_gas, base_gas)
            elif kwargs['elec'] != None:
                plt.plot(xd_elec, yd_elec, elec_line_color)
                if (plotPoint):
                    plt.plot(x_elec, y_elec, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
                bx.fill_between(xd_elec, base_elec, yd_elec,
                                facecolor=elec_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_elec,
                                facecolor=base_elec_color, alpha=alpha)
                output = (xd_elec, yd_elec, base_elec)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, max(max(yd_elec), max(yd_gas)) * 1.1))
        elif side == 'base_elec':
            plt.plot(xd_elec, [base_elec] * len(xd_elec), base_elec_color)
            bx.fill_between(xd_elec, 0, base_elec,
                            facecolor=base_elec_color, alpha=alpha)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, base_elec * 3))
            output = (xd_elec, base_elec)
        elif side == 'base_gas':
            plt.plot(xd_elec, [base_gas] * len(xd_elec),
                     base_gas_color)
            bx.fill_between(xd_elec, 0, base_gas,
                            facecolor=base_gas_color, alpha=alpha)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, base_gas * 3))
            output = (xd_elec, base_gas)
    plt.title('Lean {0} plot, Building {1}'.format(title_dict[side], b, timerange))
    # if kwargs['action'] != '':
    #     plt.suptitle(kwargs['action'], fontsize=7)
    plt.xlabel('Monthly Mean Temperature, Deg F')
    plt.ylabel(ylabel_dict[side])
    plt.tight_layout()
    P.savefig('{0}{1}_{2}_{3}_{4}.png'.format(image_output_dir, b, s,
                                              side, timerange), dpi = 150)
    # plt.show()
    plt.close()
    return output

def plot_lean_one(b, s, side, timerange, **kwargs):
    print 'creating {0} LEAN for building {1} {2} ...'.format(side, b, timerange)
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("paper", font_scale=0.8)
    gas_line_color = '#DE4A50'
    gas_mk_color = '#DE4A50'
    elec_line_color = '#429CD5'
    elec_mk_color = '#429CD5'
    base_gas_color = 'orange'
    base_elec_color = 'yellow'
    alpha = 0.5
    plt.figure(figsize=(3, 3), dpi=150, facecolor='w', edgecolor='k')
    bx = plt.axes()
    if side == 'elec' or side == 'gas':
        print '11111'
        if kwargs[side] == None:
            return None
        t = kwargs[side]['x_range']
        t_min = t[0]
        t_max = t[1]
        par = kwargs[side]['regression_par']
        k = par[0]
        intercept = par[1]
        if type(kwargs[side]['breakpoint']) == int:
            breakpoint = kwargs[side]['breakpoint']
            xd = np.array([t_min, breakpoint, breakpoint, t_max])
        else:
            break_left = kwargs[side]['breakpoint'][0]
            break_right = kwargs[side]['breakpoint'][1]
            breakpoint = break_left
            xd = np.array([t_min, break_left, break_right, t_max])
        base = k * breakpoint + intercept
        # xd = np.linspace(t_min, t_max, 150)
        yd = kwargs[side]['fun'](xd, *par) - base
        if side == 'gas':
            plt.plot(xd, yd, gas_line_color)
            bx.fill_between(xd, 0, yd, facecolor=gas_line_color,
                            alpha=alpha)
            rug_x = kwargs[side]['x']
            rug_x = [x for x in rug_x if x < breakpoint]
            # sns.rugplot(rug_x, ax=bx, color=gas_line_color)
            output = (xd, yd)
        elif side == 'elec':
            plt.plot(xd, yd, elec_line_color)
            bx.fill_between(xd, 0, yd, facecolor=elec_line_color,
                            alpha=alpha)
            rug_x = kwargs[side]['x']
            try: 
                break_left
                rug_x = [x for x in rug_x if x > break_right or x <
                         break_left]
            except NameError:
                rug_x = [x for x in rug_x if x > breakpoint]
            # sns.rugplot(rug_x, ax=bx, color=elec_line_color)
            output = (xd, yd)
        if 'y_upper' in kwargs:
            plt.ylim((0, kwargs['y_upper']))
        else:
            plt.ylim((0, max(yd) * 1.1))
    else:
        print '22222'
        if kwargs['gas'] != None:
            t = kwargs['gas']['x_range']
        elif kwargs['elec'] != None:
            t = kwargs['elec']['x_range']
        else:
            return None
        t_min = t[0]
        t_max = t[1]
        if kwargs['gas'] != None:
            x_gas = kwargs['gas']['x']
            y_gas = kwargs['gas']['y']
            par_gas = kwargs['gas']['regression_par']
            intercept_gas = par_gas[1]
            k_gas = par_gas[0]
            breakpoint_gas = kwargs['gas']['breakpoint']
            xd_gas = np.array([t_min, breakpoint_gas, t_max])
            base_gas = k_gas * breakpoint_gas + intercept_gas
        if kwargs['elec'] != None:
            x_elec = kwargs['elec']['x']
            y_elec = kwargs['elec']['y']
            par_elec = kwargs['elec']['regression_par']
            k_elec = par_elec[0]
            intercept_elec = par_elec[1]
            if type(kwargs['elec']['breakpoint']) == int:
                breakpoint_elec = kwargs['elec']['breakpoint']
                xd_elec = np.array([t_min, breakpoint_elec, t_max])
            else:
                break_elec_left = kwargs['elec']['breakpoint'][0]
                break_elec_right = kwargs['elec']['breakpoint'][1]
                breakpoint_elec = break_elec_left
                xd_elec = np.array([t_min, break_elec_left,
                                    break_elec_right, t_max])
            base_elec = k_elec * breakpoint_elec + intercept_elec
        if kwargs['gas'] != None and kwargs['elec'] != None:
            yd_elec = (kwargs['elec']['fun'](xd_elec, *par_elec)) + base_gas
            yd_gas = (kwargs['gas']['fun'](xd_gas, *par_gas)) + base_elec
        elif kwargs['gas'] != None:
            yd_gas = (kwargs['gas']['fun'](xd_gas, *par_gas))
        elif kwargs['elec'] != None:
            yd_elec = (kwargs['elec']['fun'](xd_elec, *par_elec))
        else:
            return None
        t_min = t[0]
        t_max = t[1]
        # xd = np.linspace(t_min, t_max, 150)
        if side == 'combined':
            marker_style = "o"
            marker_size = 2
            if kwargs['gas'] != None and kwargs['elec'] != None:
                plt.plot(xd_gas, yd_gas, gas_line_color)
                plt.plot(xd_elec, yd_elec, elec_line_color)
                plt.plot(x_gas, y_gas + base_elec, marker_style, markerfacecolor=gas_mk_color, ms=marker_size)
                plt.plot(x_elec, y_elec + base_gas, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
                bx.fill_between(xd_elec, base_elec + base_gas, yd_elec,
                                facecolor=elec_line_color, alpha=alpha)
                bx.fill_between(xd_gas, base_elec + base_gas, yd_gas,
                                facecolor=gas_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_elec,
                                facecolor=base_elec_color, alpha=alpha)
                bx.fill_between(xd_elec, base_elec, base_elec + base_gas,
                                facecolor=base_gas_color, alpha=alpha)
                output = (xd_elec, yd_elec, xd_gas, yd_gas, base_elec,
                          base_gas)
            elif kwargs['gas'] != None:
                plt.plot(xd_gas, yd_gas, gas_line_color)
                plt.plot(x_gas, y_gas, marker_style, markerfacecolor=gas_mk_color, ms=marker_size)
                bx.fill_between(xd_gas, base_gas, yd_gas,
                                facecolor=gas_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_gas,
                                facecolor=base_gas_color, alpha=alpha)
                output = (xd_gas, yd_gas, base_gas)
            elif kwargs['elec'] != None:
                plt.plot(xd_elec, yd_elec, elec_line_color)
                plt.plot(x_elec, y_elec, marker_style, markerfacecolor=elec_mk_color, ms=marker_size)
                bx.fill_between(xd_elec, base_elec, yd_elec,
                                facecolor=elec_line_color, alpha=alpha)
                bx.fill_between(xd_elec, 0, base_elec,
                                facecolor=base_elec_color, alpha=alpha)
                output = (xd_elec, yd_elec, base_elec)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, max(max(yd_elec), max(yd_gas)) * 1.1))
        elif side == 'base_elec':
            plt.plot(xd_elec, [base_elec] * len(xd_elec), base_elec_color)
            bx.fill_between(xd_elec, 0, base_elec,
                            facecolor=base_elec_color, alpha=alpha)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, base_elec * 3))
            output = (xd_elec, base_elec)
        elif side == 'base_gas':
            plt.plot(xd_elec, [base_gas] * len(xd_elec),
                     base_gas_color)
            bx.fill_between(xd_elec, 0, base_gas,
                            facecolor=base_gas_color, alpha=alpha)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, base_gas * 3))
            output = (xd_elec, base_gas)
    print '33333'
    plt.title('Lean {0} plot, Building {1}'.format(title_dict[side], b, timerange))
    # if kwargs['action'] != '':
    #     plt.suptitle(kwargs['action'], fontsize=7)
    plt.xlabel('Monthly Mean Temperature, Deg F')
    plt.ylabel(ylabel_dict[side])
    plt.tight_layout()
    P.savefig('{0}{1}_{2}_{3}_{4}.png'.format(image_output_dir, b, s,
                                              side, timerange), dpi = 150)
    # plt.show()
    plt.close()
    return output

# adapted from Shilpi's code
def get_weather_data(s, minDate, maxDate, step, variable='temperature'):
    # FIXME: cache result
    print 'start reading {0}'.format(s)
    starttime = time.time()
    if step == 'M':
        if variable == 'temperature':
            url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=weatherunderground*{0}*{1}*Monthly".format(s, variable)
        elif variable == 'CDD' or variable == 'HDD':
            url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=weatherunderground*{0}*Monthly*{1}".format(s, variable)
    else:
        url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=weatherunderground*{0}*Hourly".format(s)
    r = requests.get(url, auth=('Weather', 'Weather1!@'), verify=False)
    if len(r.json()['Items']) == 0:
        print 'No Data for station {0}'.format(s)
        return
    webId = r.json()['Items'][0]['WebId']
    recordUrl = "https://128.2.109.159/piwebapi/streams/"+webId+"/recorded?starttime='"+minDate+"'&endtime='"+maxDate+"'&maxcount=1490000"
    # recordUrl = "https://128.2.109.159/piwebapi/streams/"+webId+"/recorded?starttime='"+minDate+"'&endtime='"+maxDate+"'&maxcount=149000"
    rec = requests.get(recordUrl, auth=('Weather', 'Weather1!@'),
                       verify=False)
    json_list = (rec.json()['Items'])
    # print json_list
    timestamps = [x['Timestamp'] for x in json_list]
    temp = [x['Value'] for x in json_list]
    df = pd.DataFrame({'Timestamp': timestamps, s: temp})
    col_value = df[s].tolist()
    errs = []
    for x in col_value:
        try:
            y = float(x)
        except TypeError:
            errs.append(x)
    if len(errs) > 0:
        print '# Errors: {0}'.format(len(errs))
        return None
    t1 = time.time()
    # df.to_csv(os.getcwd() + '/{0}.csv'.format(s),
    #           index=False)
    df.set_index(pd.DatetimeIndex(pd.to_datetime(df['Timestamp'])), inplace=True)
    df_re = df.resample(step, how='mean')
    print 'finish reading {0} in {1}s'.format(s, time.time() - starttime)
    return df_re

def weather2calyear(y, m):
    if m > 1:
        return y
    else:
        return y - 1

# weather month to calendar month
def weather2calmonth(m):
    m = m - 1
    if m > 0:
        return m
    else:
        return 12

def test_weather2cal():
    for month in range(1, 13):
        print '2014-{0} to {1}-{2}'.format(month,
                                           weather2calyear(2014,
                                                           month),
                                           weather2calmonth(month))

def get_mean_temp(s, minDate, maxDate): 
    df = get_weather_data(s, minDate, maxDate, 'monthly')
    # FIXME: conversion to local time make a big difference, why?
    df['w_year'] = df['Timestamp'].map(lambda x: int(x[:4]) if not type(x) == float else x)
    df['w_month'] = df['Timestamp'].map(lambda x: int(x[5:7]) if not type(x) == float else x)
    df['year'] = df.apply(lambda r: weather2calyear(r['w_year'],
                                                    r['w_month']),
                          axis=1)
    df['month'] = df['w_month'].map(weather2calmonth)
    return df

def get_lat_long(address):
    g = geocoder.google(address)
    if not (g.json['ok']):
        print 'Address not found'
        return None
    else:
        latlng = g.latlng
        # print '{0},{1},{2}'.format(address, latlng[0], latlng[1])
        return latlng

def get_station(latlng):
    if latlng == None:
        print 'No input to get_station'
        return None
    df_lookup = pd.read_csv(os.getcwd() + \
                            '/input/Weather Data Mapping to Use.csv')
    lat = latlng[0]
    lng = latlng[1]
    df_lookup['distance'] = df_lookup.apply(lambda r: vincenty((lat, lng), (r['Latitude'], r['Longitude']), miles=True), axis=1)
    min_distance = df_lookup['distance'].min()
    df_temp = df_lookup[df_lookup['distance'] == min_distance]
    icao = df_temp['StationID'].tolist()[0]
    distance = df_temp['distance'].tolist()[0]
    return (icao, distance)

# b: building ID, kwargs include the following keys: state_abbr(state
# abbreviation, address(street address), zipcode(zip code or postal
# code), city(name of city)
# return weather station ICAO
def geocode(b, **kwargs):
    print 'geocoding ...'
    d = {k:kwargs[k] for k in kwargs if k in ['address', 'city',
                                              'state', 'zipcode']}
    tokens = []
    for k in ['address', 'city', 'state', 'zipcode']:
        if k in kwargs:
            if kwargs[k] != None:
                tokens.append(kwargs[k])
    geocode_input = ','.join(tokens)
    # caching needed
    latlng = get_lat_long(geocode_input)
    icao, distance = get_station(latlng)
    # print '{0}, latlng: {1}, station: {2}, distance: {3} mile'.format(geocode_input, latlng, icao, distance)
    return icao

def test_geocode():
    geocode('testbuilding', address='1620 V STREET NW', state='DC',
            city='Washington', zipcode='20009')
    
def test_get_weather_data():
    s = geocode('testbuilding', address='1620 V STREET NW',
                state='DC', city='Washington', zipcode='20009')
    minDate = '2007-10-01 00:00:00'
    maxDate = '2016-01-01 00:00:00'
    get_weather_data(s, minDate, maxDate)
    return
    
def get_area(d_gas, d_elec, n_par_elec):
    print 'computing area under curve ...'
    t = d_gas['x_range']
    t_min = t[0]
    t_max = t[1]
    par_gas = d_gas['regression_par']
    par_elec = d_elec['regression_par']
    k_gas = par_gas[0]
    intercept_gas = par_gas[1]
    breakpoint_gas = d_gas['breakpoint']
    f_gas = lambda x: k_gas * x + intercept_gas
    base_gas = f_gas(breakpoint_gas)
    k_elec = par_elec[0]
    intercept_elec = par_elec[1]
    if n_par_elec == 2:
        breakpoint_elec = d_elec['breakpoint']
        f_elec = lambda x: k_elec * x + intercept_elec
        base_elec = k_elec * breakpoint_elec + intercept_elec
        area_elec = (t_max - breakpoint_elec) * max(0, (f_elec(t_max) -
                                                 base_elec)) / 2
    else:
        break_elec_left = d_elec['breakpoint'][0]
        break_elec_right = d_elec['breakpoint'][1]
        k_elec_right = par_elec[2]
        f_elec_left = lambda x: k_elec * x + intercept_elec
        base_elec = k_elec * break_elec_left + intercept_elec
        f_elec_right = lambda x: k_elec_right * (x - break_elec_right)\
            + f_elec_left(break_elec_left)
        area_elec = (break_elec_left - t_min) * max(0, (f_elec_left(t_min) - base_elec)) / 2 + (t_max - break_elec_right) * max(0, (f_elec_right(t_max) - base_elec)) / 2

    area_base_elec = (t_max - t_min) * base_elec
    area_base_gas = (t_max - t_min) * base_gas
    area_gas = (breakpoint_gas - t_min) * max(0, (f_gas(t_min) - base_gas)) / 2
    area_total = area_gas + area_elec + area_base_gas + area_base_elec
    # print 'area of base electricity: {0}'.format(area_base_elec)
    # print 'area of base gas: {0}'.format(area_base_gas)
    # print 'area of conditioning electricity: {0}'.format(area_elec)
    # print 'area of conditioning gas: {0}'.format(area_gas)
    return (area_base_elec, area_base_gas, area_elec, area_gas,
            area_total, base_gas, base_elec)

# FIXME: change args to kwargs
def lean_temperature_fromdb(b, s, n_par_elec, timerange, plotPoint=False, *args, **kwargs):
    print 'creating lean plot ...'
    if len(args) > 0:
        d_gas = piecewise_reg_one_fromdb(b, s, 2, 'eui_gas', True, timerange, args[0])
        d_elec = piecewise_reg_one_fromdb(b, s, n_par_elec, True,
                                          'eui_elec', timerange,
                                          args[0])
    else:
        print b, s, timerange
        d_gas = piecewise_reg_one_fromdb(b, s, 2, 'eui_gas', True,
                                         timerange)
        d_elec = piecewise_reg_one_fromdb(b, s, n_par_elec, 'eui_elec',
                                          True, timerange)
    if not 'action' in kwargs:
        action = ''
    else:
        action = kwargs['action']
    # FIXME: add back later
    # if not (d_gas['good_regression'] and d_elec['good_regression']):
    #     print 'bad regression: {0}'.format(b)
    #     return None
    # if d_elec['regression_par'][0] > 0:
    #     print d_elec['regression_par']
    #     print 'bad electric fit line'
    #     return None
    # FIXME: modify area calculation later
    # (area_base_elec, area_base_gas, area_elec, area_gas, area_total, base_gas, base_elec) = get_area(d_gas, d_elec, n_par_elec)

    # plot_lean_one(b, s, "gas", gas=d_gas)
    # plot_lean_one(b, s, "elec", elec=d_elec)
    # plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec)
    # plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec)
    # (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = plot_lean_one(b, s, "combined", gas=d_gas, elec=d_elec)
    result = None
    if (d_gas != None) and (d_elec != None):
        result = plot_lean_one_fromdb(b, s, "combined", timerange, plotPoint=plotPoint, gas=d_gas, elec=d_elec, y_upper=15, action=action)
    elif d_gas != None:
        result = plot_lean_one_fromdb(b, s, "combined", timerange, plotPoint=plotPoint, gas=d_gas, elec=None, y_upper=60)
    elif d_elec != None:
        result = plot_lean_one_fromdb(b, s, "combined", timerange, plotPoint=plotPoint, gas=None, elec=d_elec, y_upper=30)
    # plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec,
    #               y_upper=25)
    # plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec, y_upper=5)
    if result == None:
        return None
    if len(result) > 3:
        (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = result
    else:
        if d_gas != None:
            (xd_gas, yd_gas, base_gas) = result
        else:
            (xd_elec, yd_elec, base_elec) = result

    # d_gas.pop('fun', None)
    # d_gas.pop('x', None)
    # d_gas.pop('y', None)
    d_plot = {}
    if d_gas != None:
        d_gas['regression_par'] = list(d_gas['regression_par'])
        d_gas['x'] = list(d_gas['x'])
        d_gas['y'] = list(d_gas['y'])
        # d_gas['area_base_gas'] = area_base_gas
        # d_gas['area_gas'] = area_gas
        # d_gas['area_total'] = area_total
        d_gas['base_gas'] = base_gas
        d_plot['xd_gas'] = list(xd_gas)
        d_plot['yd_gas'] = list(yd_gas)
        d_plot['base_gas'] = base_gas
    # d_elec.pop('fun', None)
    # d_elec.pop('x', None)
    # d_elec.pop('y', None)
    if d_elec != None:
        d_elec['regression_par'] = list(d_elec['regression_par'])
        d_elec['x'] = list(d_elec['x'])
        d_elec['y'] = list(d_elec['y'])
        # d_elec['area_base_elec'] = area_base_elec
        # d_elec['area_elec'] = area_elec
        # d_elec['area_total'] = area_total
        d_elec['base_elec'] = base_elec
        d_plot['xd_elec'] = list(xd_elec)
        d_plot['yd_elec'] = list(yd_elec)
        d_plot['base_elec'] = base_elec
    # for key in ['x_range', 'CV(RMSE)', 'regression_par', 'breakpoint']:
    #     print key
    #     print d_gas[key]
    # with open ('{0}{1}_elec.json'.format(json_output_dir, b), 'w+') as wt:
    #     json.dump(d_elec, wt)
    # with open ('{0}{1}_gas.json'.format(json_output_dir, b), 'w+') as wt:
    #     json.dump(d_gas, wt)
    return d_gas, d_elec, d_plot

# FIXME: change args to kwargs
def lean_temperature(b, s, n_par_elec, timerange, *args, **kwargs):
    print 'creating lean plot ...'
    if len(args) > 0:
        d_gas = piecewise_reg_one(b, s, 2, 'eui_gas', True, timerange, args[0])
        d_elec = piecewise_reg_one(b, s, n_par_elec, True, 'eui_elec',
                                   timerange, args[0])
    else:
        d_gas = piecewise_reg_one(b, s, 2, 'eui_gas', True, timerange)
        d_elec = piecewise_reg_one(b, s, n_par_elec, 'eui_elec',
                                   True, timerange)
    if not 'action' in kwargs:
        action = ''
    else:
        action = kwargs['action']
    # FIXME: add back later
    # if not (d_gas['good_regression'] and d_elec['good_regression']):
    #     print 'bad regression: {0}'.format(b)
    #     return None
    # if d_elec['regression_par'][0] > 0:
    #     print d_elec['regression_par']
    #     print 'bad electric fit line'
    #     return None
    # FIXME: modify area calculation later
    # (area_base_elec, area_base_gas, area_elec, area_gas, area_total, base_gas, base_elec) = get_area(d_gas, d_elec, n_par_elec)

    # plot_lean_one(b, s, "gas", gas=d_gas)
    # plot_lean_one(b, s, "elec", elec=d_elec)
    # plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec)
    # plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec)
    # (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = plot_lean_one(b, s, "combined", gas=d_gas, elec=d_elec)
    # plot_lean_one(b, s, "gas", gas=d_gas, y_upper=60)
    # plot_lean_one(b, s, "elec", elec=d_elec, y_upper=30)
    # plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec,
    #               y_upper=25)
    # plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec, y_upper=5)
    result = plot_lean_one(b, s, "combined", timerange, gas=d_gas, elec=d_elec, y_upper=15, action=action)
    if result == None:
        return None
    if len(result) > 3:
        (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = result
    else:
        if d_gas != None:
            (xd_gas, yd_gas, base_gas) = result
        else:
            (xd_elec, yd_elec, base_elec) = result

    # d_gas.pop('fun', None)
    # d_gas.pop('x', None)
    # d_gas.pop('y', None)
    d_plot = {}
    if d_gas != None:
        d_gas['regression_par'] = list(d_gas['regression_par'])
        d_gas['x'] = list(d_gas['x'])
        d_gas['y'] = list(d_gas['y'])
        # d_gas['area_base_gas'] = area_base_gas
        # d_gas['area_gas'] = area_gas
        # d_gas['area_total'] = area_total
        d_gas['base_gas'] = base_gas
        d_plot['xd_gas'] = list(xd_gas)
        d_plot['yd_gas'] = list(yd_gas)
        d_plot['base_gas'] = base_gas
    # d_elec.pop('fun', None)
    # d_elec.pop('x', None)
    # d_elec.pop('y', None)
    if d_elec != None:
        d_elec['regression_par'] = list(d_elec['regression_par'])
        d_elec['x'] = list(d_elec['x'])
        d_elec['y'] = list(d_elec['y'])
        # d_elec['area_base_elec'] = area_base_elec
        # d_elec['area_elec'] = area_elec
        # d_elec['area_total'] = area_total
        d_elec['base_elec'] = base_elec
        d_plot['xd_elec'] = list(xd_elec)
        d_plot['yd_elec'] = list(yd_elec)
        d_plot['base_elec'] = base_elec
    # for key in ['x_range', 'CV(RMSE)', 'regression_par', 'breakpoint']:
    #     print key
    #     print d_gas[key]
    # with open ('{0}{1}_elec.json'.format(json_output_dir, b), 'w+') as wt:
    #     json.dump(d_elec, wt)
    # with open ('{0}{1}_gas.json'.format(json_output_dir, b), 'w+') as wt:
    #     json.dump(d_gas, wt)
    return d_gas, d_elec, d_plot

# CV(RMSE) cutoff: the baseline model shall have a maximum CV(RMSE) of
# 20% for energy use, These requirements are 25% and 35%,
# respectively, when 12 to 60 months of data will be used in computing
# savings.
def CVRMSE(x, y, p, f, n_par):
    y_hat = f(x, *p)
    n = len(x)
    return np.sqrt((np.subtract(y_hat, y) ** 2).sum() / (n - n_par))/np.array(y).mean()

def piecewise_reg_one_fromdb(b, s, n_par, theme, cuttail, timerange=None, *args):
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("paper", font_scale=1)
    if len(args) == 0:
        conn = uo.connect('all')
        with conn:
            df = pd.read_sql('SELECT * FROM EUAS_monthly_weather WHERE Building_Number = \'{0}\''.format(b), conn)
        conn.close()
    else:
        df = args[0]
    # df = df[df[s].notnull()]
    # df['year'] = df['timestamp'].map(lambda x: int(x[:4]))
    # df['month'] = df['timestamp'].map(lambda x: int(x[5:7]))
    df['timestamp'] = df.apply(lambda r: '{0}-{1}-1'.format(int(r['year']), int(r['month'])), axis=1)
    df['timestamp'] = pd.to_datetime(df['timestamp'])
    df.sort_values(['year', 'month'], inplace=True)
    if timerange == None:
        df = df.tail(n=36)
    else:
        yearcol, timefilter = util.get_time_filter(timerange)
        df['in_range'] = df[yearcol].map(timefilter)
        df = df[df['in_range']]
    if (len(df) > 36 and cuttail and ("before" in timerange)):
        df = df.tail(n=36)
    if (len(df) > 36 and cuttail and ("after" in timerange)):
        df = df.head(n=36)
    if len(df) < 6:
        print 'not enough data points in {0}'.format(timerange)
        return None
    if 'day' in df:
        df = df[['timestamp', 'year', 'month', 'ave', theme, 'day']]
    else:
        df = df[['timestamp', 'year', 'month', 'ave', theme]]
    df = df[df['ave'].notnull()]
    x = np.array(df['ave'])
    y = np.array(df[theme])
    # t = np.array(df['begin_time'])
    t = np.array(df['timestamp'])
    x_min = x.min()
    x_max = x.max()
    break_low = 40
    break_high = 81
    xd = np.linspace(x_min, x_max, 150)
    cvrmses = []
    ps = []
    slope_side = []
    breakpoint_cal = []
    if n_par == 2:
        breakpoints = range(break_low, break_high)
        for breakpoint in breakpoints:
            def piecewise_linear_leftslope(x, k, intercept):
                return np.piecewise(x, [x < breakpoint, x >= breakpoint], [lambda x:k * x + intercept, lambda x:k * breakpoint + intercept])
            try:
                p1 , e1 = optimize.curve_fit(piecewise_linear_leftslope,
                                            x, y)
            except (RuntimeError, TypeError) as e:
                continue
            cvrmse = CVRMSE(x, y, p1, piecewise_linear_leftslope,
                            n_par)
            cvrmses.append(cvrmse)
            ps.append(p1)
            slope_side.append('left')
            breakpoint_cal.append(breakpoint)

            if theme == 'eui_elec':
                def piecewise_linear_rightslope(x, k, intercept):
                    return np.piecewise(x, [x >= breakpoint, x < breakpoint], [lambda x:k * x + intercept, lambda x:k * breakpoint + intercept])
                try:
                    p2 , e2 = optimize.curve_fit(piecewise_linear_rightslope,
                                                x, y)
                except (RuntimeError, TypeError) as e:
                    continue
                cvrmse = CVRMSE(x, y, p2, piecewise_linear_rightslope,
                                n_par)
                cvrmses.append(cvrmse)
                ps.append(p2)
                slope_side.append('right')
                breakpoint_cal.append(breakpoint)
        result = sorted(zip(breakpoint_cal, cvrmses, ps, slope_side), key=lambda x: x[1])
    elif n_par == 3:
        breakpoints = [(i, j) for i in range(break_low, break_high) for j in range(i + 1, break_high)]
        for (break_1, break_2) in breakpoints:
            def piecewise_linear(x, k1, b1, k2):
                x0 = break_1
                x1 = break_2
                y0 = k1 * x0 + b1
                y1 = y0
                return np.piecewise(x, [x < x0, x >= x1], [lambda x:k1 * x + b1, lambda x:k2 * (x - x1) + y1, lambda x:y0])
            try:
                p , e = optimize.curve_fit(piecewise_linear, x, y)
            except (RuntimeError, TypeError) as e:
                continue
            cvrmse = CVRMSE(x, y, p, piecewise_linear, n_par)
            cvrmses.append(cvrmse)
            ps.append(p)
            slope_side.append('NA')
        result = sorted(zip(breakpoints, cvrmses, ps, slope_side), key=lambda x: x[1])
    if len(result) == 0:
        print 'optimization failed'
        # good_regression = False
        return None
    best = result[0]
    b_point_opt = best[0]
    p_opt = best[2]
    slope_side_opt = best[3]
    cvrmse_opt = best[1]
    # print theme, slope_side_opt, p_opt
    # print 'breakpoint: {1}, error: {0}'.format(cvrmse_opt, b_point_opt)
    if n_par == 2:
        if slope_side_opt == 'left':
            def piecewise_linear(x, k, intercept):
                return np.piecewise(x, [x < b_point_opt, x >= b_point_opt], [lambda x:k * x + intercept, lambda x:k * b_point_opt + intercept])
        else:
            assert(slope_side_opt == 'right')
            def piecewise_linear(x, k, intercept):
                return np.piecewise(x, [x >= b_point_opt, x < b_point_opt], [lambda x:k * x + intercept, lambda x:k * b_point_opt + intercept])
    elif n_par == 3:
        def piecewise_linear(x, k1, b1, k2):
            x0 = b_point_opt[0]
            x1 = b_point_opt[1]
            y0 = k1 * x0 + b1
            y1 = y0
            return np.piecewise(x, [x < x0, x >= x1], [lambda x:k1 * x + b1, lambda x:k2 * (x - x1) + y1, lambda x:y0])
    if abs(p_opt[0] - 0) < 1e-20:
        print 'all zero {0} consumption: {1}'.format(theme, p_opt)
        good_regression = False
        return None
    # bx = plt.axes()
    # bx.plot(x, y, "o")
    # g = sns.lmplot(x=s, y=theme, hue='day', data=df, fit_reg=False)
    # # print piecewise_linear(xd, *p_opt)
    # plt.ylabel('kBtu')
    # # plt.gca().set_ylim(bottom=0)
    # ax = g.axes
    # ax[0, 0].set_ylim((0, 1e5))
    # P.savefig('{3}scatter_{0}_{1}_{2}.png'.format(b, s, theme, image_output_dir), dpi = 150)
    # # plt.show()
    # plt.close()
    # g = sns.lmplot(x=s, y=theme, col='day', hue='day', col_wrap=3, size=3, data=df, fit_reg=False)
    # ax = g.axes
    # for i in ax:
    #     i.set_ylim((0, 1e5))
    # P.savefig('{3}scatter_{0}_{1}_{2}_3b3.png'.format(b, s, theme, image_output_dir), dpi = 150)
    # plt.close()

    plt.plot(x, y, "o")
    plt.plot(xd, piecewise_linear(xd, *p_opt), "-")
    plt.title('break point {0}F, CV(RMSE): {1}'.format(b_point_opt, cvrmse_opt))
    # plt.show()
    # P.savefig('{3}regression_{0}_{1}_{2}.png'.format(b, s, theme, image_output_dir), dpi = 150)
    plt.close()
    good_regression = True
    # remove electric heating
    if theme == 'eui_elec':
        if slope_side_opt == 'left':
            print 'bad electric regression left side -----------------'
            good_regression = False
        if slope_side_opt == 'right' and p_opt[0] < 0:
            print 'bad electric regression right side ----------------'
            good_regression = False
    if theme == 'eui_gas':
        if slope_side_opt == 'left' and p_opt[0] > 0:
            print 'bad gas regression left side ----------------------'
            good_regression = False
        if slope_side_opt == 'right' > 0:
            print 'bad gas regression right side ---------------------'
            good_regression = False
    return {'breakpoint': b_point_opt, 'CV(RMSE)': best[1],
            'regression_par': p_opt, 'x_range': (x_min, x_max), 'fun':
            piecewise_linear, 'x': x, 'y': y, 'good_regression':
            good_regression, 'df': df}

# issue: from the document, it seems more years will need larger
# threshold for error (CV(RMSE)), no threshold is specified for > 60
# months

# if n_par == 2: return {'breakpoint': int, 'regression_par':
# array([k, intercept]), 'CV(RMSE)': float}
# if n_par == 3: return {'breakpoint': (int, int), 'regression_par':
# array([k1, intercept1, k2]), 'CV(RMSE)': float}
def piecewise_reg_one(b, s, n_par, theme, cuttail, timerange=None, x=None, y=None, *args):
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("paper", font_scale=1)
    if len(args) == 0:
        df = pd.read_csv(weatherdir + 'energy_temp/{0}_{1}.csv'.format(b, s))
    else:
        df = args[0]
    df = df[df[s].notnull()]
    if not 'year' in df:
        df['year'] = df['timestamp'].map(lambda x: int(x[:4]))
    if not 'month' in df:
        df['month'] = df['timestamp'].map(lambda x: int(x[5:7]))
    df['timestamp'] = pd.to_datetime(df['timestamp'])
    df.sort_values('timestamp', inplace=True)
    if timerange == None and cuttail:
        df = df.tail(n=36)
    elif timerange == None:
        df = df
    else:
        yearcol, timefilter = util.get_time_filter(timerange)
        df['in_range'] = df[yearcol].map(timefilter)
        df = df[df['in_range']]
    if len(df) > 36 and cuttail:
        df = df.tail(n=36)
    if len(df) < 6:
        print 'not enough data points in {0}'.format(timerange)
        return None
    if 'day' in df:
        df = df[['timestamp', 'year', 'month', s, theme, 'day']]
    else:
        df = df[['timestamp', 'year', 'month', s, theme]]
    if x is None:
        x = np.array(df[s])
        y = np.array(df[theme])
    x_min = x.min()
    x_max = x.max()
    break_low = 40
    break_high = 81
    xd = np.linspace(x_min, x_max, 150)
    cvrmses = []
    ps = []
    slope_side = []
    breakpoint_cal = []
    if n_par == 2:
        breakpoints = range(break_low, break_high)
        for breakpoint in breakpoints:
            def piecewise_linear_leftslope(x, k, intercept):
                return np.piecewise(x, [x < breakpoint, x >= breakpoint], [lambda x:k * x + intercept, lambda x:k * breakpoint + intercept])
            p1 , e1 = optimize.curve_fit(piecewise_linear_leftslope,
                                         x, y)
            cvrmse = CVRMSE(x, y, p1, piecewise_linear_leftslope,
                            n_par)
            cvrmses.append(cvrmse)
            ps.append(p1)
            slope_side.append('left')
            breakpoint_cal.append(breakpoint)

            if theme == 'eui_elec':
                def piecewise_linear_rightslope(x, k, intercept):
                    return np.piecewise(x, [x >= breakpoint, x < breakpoint], [lambda x:k * x + intercept, lambda x:k * breakpoint + intercept])
                p2 , e2 = optimize.curve_fit(piecewise_linear_rightslope,
                                            x, y)
                cvrmse = CVRMSE(x, y, p2, piecewise_linear_rightslope,
                                n_par)
                cvrmses.append(cvrmse)
                ps.append(p2)
                slope_side.append('right')
                breakpoint_cal.append(breakpoint)
        result = sorted(zip(breakpoint_cal, cvrmses, ps, slope_side), key=lambda x: x[1])
    elif n_par == 3:
        breakpoints = [(i, j) for i in range(break_low, break_high) for j in range(i + 1, break_high)]
        for (break_1, break_2) in breakpoints:
            def piecewise_linear(x, k1, b1, k2):
                x0 = break_1
                x1 = break_2
                y0 = k1 * x0 + b1
                y1 = y0
                return np.piecewise(x, [x < x0, x >= x1], [lambda x:k1 * x + b1, lambda x:k2 * (x - x1) + y1, lambda x:y0])
            p , e = optimize.curve_fit(piecewise_linear, x, y)
            cvrmse = CVRMSE(x, y, p, piecewise_linear, n_par)
            cvrmses.append(cvrmse)
            ps.append(p)
            slope_side.append('NA')
        result = sorted(zip(breakpoints, cvrmses, ps, slope_side), key=lambda x: x[1])
    best = result[0]
    b_point_opt = best[0]
    p_opt = best[2]
    slope_side_opt = best[3]
    cvrmse_opt = best[1]
    # print theme, slope_side_opt, p_opt
    # print 'breakpoint: {1}, error: {0}'.format(cvrmse_opt, b_point_opt)
    if n_par == 2:
        if slope_side_opt == 'left':
            def piecewise_linear(x, k, intercept):
                return np.piecewise(x, [x < b_point_opt, x >= b_point_opt], [lambda x:k * x + intercept, lambda x:k * b_point_opt + intercept])
        else:
            assert(slope_side_opt == 'right')
            def piecewise_linear(x, k, intercept):
                return np.piecewise(x, [x >= b_point_opt, x < b_point_opt], [lambda x:k * x + intercept, lambda x:k * b_point_opt + intercept])
    elif n_par == 3:
        def piecewise_linear(x, k1, b1, k2):
            x0 = b_point_opt[0]
            x1 = b_point_opt[1]
            y0 = k1 * x0 + b1
            y1 = y0
            return np.piecewise(x, [x < x0, x >= x1], [lambda x:k1 * x + b1, lambda x:k2 * (x - x1) + y1, lambda x:y0])
    if abs(p_opt[0] - 0) < 1e-20:
        print 'all zero {0} consumption: {1}'.format(theme, p_opt)
        good_regression = False
        return None
    # bx = plt.axes()
    # bx.plot(x, y, "o")
    # g = sns.lmplot(x=s, y=theme, hue='day', data=df, fit_reg=False)
    # # print piecewise_linear(xd, *p_opt)
    # plt.ylabel('kBtu')
    # # plt.gca().set_ylim(bottom=0)
    # ax = g.axes
    # ax[0, 0].set_ylim((0, 1e5))
    # P.savefig('{3}scatter_{0}_{1}_{2}.png'.format(b, s, theme, image_output_dir), dpi = 150)
    # # plt.show()
    # plt.close()
    # g = sns.lmplot(x=s, y=theme, col='day', hue='day', col_wrap=3, size=3, data=df, fit_reg=False)
    # ax = g.axes
    # for i in ax:
    #     i.set_ylim((0, 1e5))
    # P.savefig('{3}scatter_{0}_{1}_{2}_3b3.png'.format(b, s, theme, image_output_dir), dpi = 150)
    # plt.close()

    # plt.plot(x, y, "o")
    # plt.plot(xd, piecewise_linear(xd, *p_opt))
    # plt.title('break point {0}F, CV(RMSE): {1}'.format(b_point_opt, cvrmse_opt))
    # plt.show()

    # P.savefig('{3}regression_{0}_{1}_{2}.png'.format(b, s, theme, image_output_dir), dpi = 150)
    # plt.close()
    good_regression = True
    # remove electric heating
    if theme == 'eui_elec':
        if slope_side_opt == 'left':
            print 'bad electric regression left side -----------------'
            good_regression = False
        if slope_side_opt == 'right' and p_opt[0] < 0:
            print 'bad electric regression right side ----------------'
            good_regression = False
    if theme == 'eui_gas':
        if slope_side_opt == 'left' and p_opt[0] > 0:
            print 'bad gas regression left side ----------------------'
            good_regression = False
        if slope_side_opt == 'right' > 0:
            print 'bad gas regression right side ---------------------'
            good_regression = False
    return {'breakpoint': b_point_opt, 'CV(RMSE)': best[1],
            'regression_par': p_opt, 'x_range': (x_min, x_max), 'fun':
            piecewise_linear, 'x': x, 'y': y, 'good_regression':
            good_regression, 'df': df}

def main():
    get_weather_data('KGAD', '2009-09-30T00:00:00Z', 
                     '2009-10-01T00:00:00Z', 'H')
                     # '2016-05-01T00:00:00Z', 'H')
    # get_weather_data('KPIT', '2007-12-01 00:00:00', 
    #                  '2015-09-23 00:00:00')
    # test_weather2cal()
    # get_mean_temp('KPIT', '2007-12-01 00:00:00', 
    #               '2015-09-23 00:00:00')
    # 2 corresponds to the 3-parameter change point model
    # lean_temperature('NM0050ZZ', 'KABQ', 2)
    # 3 corresponds to the 5-parameter change point model
    # lean_temperature('MA0113ZZ', 'KCQX', 3)

# main()
