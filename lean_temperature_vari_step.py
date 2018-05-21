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
from vincenty import vincenty

homedir = os.getcwd() + '/csv_FY/'
weatherdir = os.getcwd() + '/csv_FY/weather/'
fldir = os.getcwd() + '/input/FL/'

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

def plot_lean_one(b, s, side, **kwargs):
    print 'creating {0} LEAN for building {1} ...'.format(side, b)
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
    # plt.figure(figsize=(5, 5), dpi=150, facecolor='w', edgecolor='k')
    bx = plt.axes()
    if side == 'elec' or side == 'gas':
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
        t = kwargs['gas']['x_range']
        t_min = t[0]
        t_max = t[1]
        par_gas = kwargs['gas']['regression_par']
        par_elec = kwargs['elec']['regression_par']
        k_gas = par_gas[0]
        intercept_gas = par_gas[1]
        breakpoint_gas = kwargs['gas']['breakpoint']
        xd_gas = np.array([t_min, breakpoint_gas, t_max])
        base_gas = k_gas * breakpoint_gas + intercept_gas
        k_elec = par_elec[0]
        intercept_elec = par_elec[1]
        if type(kwargs['elec']['breakpoint']) == int:
            breakpoint_elec = kwargs['elec']['breakpoint']
            xd_elec = np.array([t_min, breakpoint_elec, t_max])
        else:
            break_elec_left = kwargs['elec']['breakpoint'][0]
            break_elec_right = kwargs['elec']['breakpoint'][1]
            breakpoint_elec = break_elec_left
            xd_elec = np.array([t_min, breakpoint_elec_left,
                                breakpoint_elec_right, t_max])
        base_elec = k_elec * breakpoint_elec + intercept_elec
        # xd = np.linspace(t_min, t_max, 150)
        yd_gas = (kwargs['gas']['fun'](xd_gas, *par_gas)) + base_elec
        yd_elec = (kwargs['elec']['fun'](xd_elec, *par_elec)) + base_gas
        if side == 'combined':
            plt.plot(xd_gas, yd_gas, gas_line_color)
            plt.plot(xd_elec, yd_elec, elec_line_color)
            bx.fill_between(xd_elec, base_elec + base_gas, yd_elec,
                            facecolor=elec_line_color, alpha=alpha)
            bx.fill_between(xd_gas, base_elec + base_gas, yd_gas,
                            facecolor=gas_line_color, alpha=alpha)
            if 'y_upper' in kwargs:
                plt.ylim((0, kwargs['y_upper']))
            else:
                plt.ylim((0, max(max(yd_elec), max(yd_gas)) * 1.1))
            output = (xd_elec, yd_elec, xd_gas, yd_gas, base_elec,
                      base_gas)
            bx.fill_between(xd_elec, 0, base_elec,
                            facecolor=base_elec_color, alpha=alpha)
            bx.fill_between(xd_elec, base_elec, base_elec + base_gas,
                            facecolor=base_gas_color, alpha=alpha)
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
        rug_x = kwargs['gas']['x']
        # sns.rugplot(rug_x, ax=bx, color='gray')
    plt.title('Lean {0} plot\nBuilding {1}, station {2}'.format(title_dict[side], b, s))
    plt.xlabel('Monthly Mean Temperature, Deg F')
    plt.ylabel(ylabel_dict[side])
    plt.tight_layout()
    # P.savefig(os.getcwd() + '/testoutput/lean_seed/{0}_{1}_{2}.png'.format(b, s, side), dpi = 150)
    P.savefig(fldir + 'output/{0}_{1}_{2}.png'.format(b, s, side), dpi = 150)
    # plt.show()
    plt.close()
    return output

# adapted from Shilpi's code
def get_weather_data(s, minDate, maxDate, step, **kwargs):
    # FIXME: cache result
    print 'start reading {0}'.format(s)
    starttime = time.time()
    if step == 'monthly':
        url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=weatherunderground*{0}*temperature*Monthly".format(s)
    else:
        url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=weatherunderground*{0}*temperature*Hourly".format(s)
    # url =  "https://128.2.109.159/piwebapi/dataservers/s0-MYhSMORGkyGTe9bdohw0AV0lOLTYyTlBVMkJWTDIw/points?namefilter=*underground/*"+s+"*tempe*"
    r = requests.get(url, auth=('Weather', 'Weather1!@'), verify=False)
    print r
    if len(r.json()['Items']) == 0:
        print 'No Data for station {0}'.format(s)
        return pd.DataFrame({'Timestamp': [], s: []})
    webId = r.json()['Items'][0]['WebId']
    recordUrl = "https://128.2.109.159/piwebapi/streams/"+webId+"/recorded?starttime='"+minDate+"'&endtime='"+maxDate+"'&maxcount=149000"
    rec = requests.get(recordUrl, auth=('Weather', 'Weather1!@'),
                       verify=False)
    json_list = (rec.json()['Items'])
    timestamps = [x['Timestamp'] for x in json_list]
    temp = [x['Value'] for x in json_list]
    df = pd.DataFrame({'Timestamp': timestamps, s: temp})
    # t1 = time.time()
    if 'outfile' in kwargs:
        df.to_csv(kwargs['outfile'], index=False)
    print 'finish reading {0} in {1}s'.format(s, time.time() - starttime)
    # print df.head(n=24)
    return df

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
    print '{0}, latlng: {1}, station: {2}, distance: {3} mile'.format(geocode_input, latlng, icao, distance)
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
        area_elec = (t_max - breakpoint_elec) * (f_elec(t_max) -
                                                 base_elec) / 2
    else:
        break_elec_left = d_elec['breakpoint'][0]
        break_elec_right = d_elec['breakpoint'][1]
        k_elec_right = par_elec[2]
        f_elec_left = lambda x: k_elec * x + intercept_elec
        base_elec = k_elec * break_elec_left + intercept_elec
        f_elec_right = lambda x: k_elec_right * (x - break_elec_right)\
            + f_elec_left(break_elec_left)
        area_elec = (break_elec_left - t_min) * (f_elec_left(t_min) - base_elec) / 2 + (t_max - break_elec_right) * (f_elec_right(t_max) - base_elec) / 2

    area_base_elec = (t_max - t_min) * base_elec
    area_base_gas = (t_max - t_min) * base_gas
    area_gas = (breakpoint_gas - t_min) * (f_gas(t_min) - base_gas) / 2
    print 'area of base electricity: {0}'.format(area_base_elec)
    print 'area of base gas: {0}'.format(area_base_gas)
    print 'area of conditioning electricity: {0}'.format(area_elec)
    print 'area of conditioning gas: {0}'.format(area_gas)
    return (area_base_elec, area_base_gas, area_elec, area_gas)

def lean_temperature(b, s, n_par_elec, *args):
    if len(args) > 0:
        d_gas = piecewise_reg_one(b, s, 2, 'eui_gas', args[0])
        d_elec = piecewise_reg_one(b, s, n_par_elec, 'eui_elec', args[0])
    else:
        d_gas = piecewise_reg_one(b, s, 2, 'eui_gas')
        d_elec = piecewise_reg_one(b, s, n_par_elec, 'eui_elec')
    (area_base_elec, area_base_gas, area_elec, area_gas) = \
        get_area(d_gas, d_elec, n_par_elec)
    plot_lean_one(b, s, "gas", gas=d_gas)
    plot_lean_one(b, s, "elec", elec=d_elec)
    plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec)
    plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec)
    (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = plot_lean_one(b, s, "combined", gas=d_gas, elec=d_elec)
    # Plot_lean_one(b, s, "gas", gas=d_gas, y_upper=50)
    # plot_lean_one(b, s, "elec", elec=d_elec, y_upper=30)
    # plot_lean_one(b, s, "base_elec", gas=d_gas, elec=d_elec,
    #               y_upper=12)
    # plot_lean_one(b, s, "base_gas", gas=d_gas, elec=d_elec, y_upper=5)
    # (xd_elec, yd_elec, xd_gas, yd_gas, base_elec, base_gas) = plot_lean_one(b, s, "combined", gas=d_gas, elec=d_elec, y_upper=60)
    d_gas.pop('fun', None)
    d_gas.pop('x', None)
    d_gas.pop('y', None)
    d_gas['regression_par'] = list(d_gas['regression_par'])
    # d_gas['x'] = list(d_gas['x'])
    # d_gas['y'] = list(d_gas['y'])
    d_gas['area_base_gas'] = area_base_gas
    d_gas['area_gas'] = area_gas
    d_elec.pop('fun', None)
    d_elec.pop('x', None)
    d_elec.pop('y', None)
    d_elec['regression_par'] = list(d_elec['regression_par'])
    # d_elec['x'] = list(d_elec['x'])
    # d_elec['y'] = list(d_elec['y'])
    d_elec['area_base_elec'] = area_base_elec
    d_elec['area_elec'] = area_elec
    d_elec['area_base_elec'] = area_base_elec
    d_elec['area_elec'] = area_elec
    d_plot = {'xd_gas': list(xd_gas), 'xd_elec': list(xd_elec), 'yd_gas': list(yd_gas), 'yd_elec': list(yd_elec), 'base_elec': base_elec, 'base_gas': base_gas}
    # with open (os.getcwd() + \
    #            '/testoutput/json/{0}_{1}.json'.format(b, s), 'w+') as wt:
    #     json.dump(output_dict, wt)
    # with open (os.getcwd() + \
    #            '/testoutput/json/{0}_elec.json'.format(b), 'w+') as wt:
    #     json.dump(d_elec, wt)
    for key in ['x_range', 'CV(RMSE)', 'regression_par', 'breakpoint']:
        print key
        print d_gas[key]
    return d_gas, d_elec, d_plot

# CV(RMSE) cutoff: the baseline model shall have a maximum CV(RMSE) of
# 20% for energy use, These requirements are 25% and 35%,
# respectively, when 12 to 60 months of data will be used in computing
# savings.
def CVRMSE(x, y, p, f, n_par):
    y_hat = f(x, *p)
    n = len(x)
    return np.sqrt((np.subtract(y_hat, y) ** 2).sum() / (n - n_par))/np.array(y).mean()

# issue: from the document, it seems more years will need larger
# threshold for error (CV(RMSE)), no threshold is specified for > 60
# months

# if n_par == 2: return {'breakpoint': int, 'regression_par':
# array([k, intercept]), 'CV(RMSE)': float}
# if n_par == 3: return {'breakpoint': (int, int), 'regression_par':
# array([k1, intercept1, k2]), 'CV(RMSE)': float}
def piecewise_reg_one(b, s, n_par, theme, timecol, *args):
    if len(args) == 0:
        df = pd.read_csv(os.getcwd() + '/input/energy_temp/{0}_{1}.csv'.format(b, s))
    else:
        df = args[0]
    df.info()
    x = np.array(df[s])
    y = np.array(df[theme])
    z = np.array(df[s])
    t = np.array(df.index)
    # t = np.array(df['timestamp'])
    f, (ax1, ax2) = plt.subplots(2, sharex=True)
    print df.head()
    ax1.plot(t, y)
    ax1.set_title('Monthly {0} plot'.format(theme))
    ax2.plot(t, z)
    ax2.set_title('Monthly mean temperature (F) plot'.format(theme))
    # P.savefig(os.getcwd() + '/testoutput/lean_seed/trend_{0}_{1}_{2}.png'.format(b, s, theme), dpi = 150, bbox_inches='tight')
    plt.show()
    plt.close()
    t_min = x.min()
    t_max = x.max()
    break_low = 40
    break_high = 81
    xd = np.linspace(t_min, t_max, 150)
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
    print 'breakpoint: {1}, error: {0}'.format(cvrmse_opt, b_point_opt)
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
    plt.plot(x, y, "o")
    plt.plot(xd, piecewise_linear(xd, *p_opt))
    # plt.title('break point {0}F'.format(b_point_opt))
    # P.savefig(os.getcwd() + '/testoutput/lean_seed/regression_{0}_{1}_{2}.png'.format(b, s, theme), dpi = 150)
    plt.show()
    plt.close()
    if best[1] > 0.25:
        print 'exceeding max CV(RMSE)!'
    return {'breakpoint': b_point_opt, 'CV(RMSE)': best[1],
            'regression_par': p_opt, 'x_range': (t_min, t_max), 'fun':
            piecewise_linear, 'x': x, 'y': y}

def main():
    # get_weather_data('KPIT', '2007-12-01 00:00:00', 
    #                  '2015-09-23 00:00:00')
    # test_weather2cal()
    get_mean_temp('KPIT', '2007-12-01 00:00:00', 
                  '2015-09-23 00:00:00')
    # 2 corresponds to the 3-parameter change point model
    # lean_temperature('NM0050ZZ', 'KABQ', 2)
    # 3 corresponds to the 5-parameter change point model
    # lean_temperature('MA0113ZZ', 'KCQX', 3)

# main()
