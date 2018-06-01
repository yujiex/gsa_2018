import util_io as uo
import os
import pandas as pd
import seaborn as sns
import util
import pylab as P
import matplotlib.pyplot as plt
import lean_temperature_monthly as ltm
import numpy as np
import label as lb
import json
import glob
import get_building_set as gbs

my_dpi = 300

def plot_json(jsondir, measure_type, region, season=None, subset=None):
    if season is None:
        files = glob.glob('{0}*_{1}.json'.format(jsondir, measure_type))
    else:
        files = glob.glob('{0}*_{1}_{2}.json'.format(jsondir, measure_type, season))

    if measure_type == 'electric':
        good_set = gbs.get_energy_set('eui_elec')
    elif measure_type == 'gas':
        good_set = gbs.get_energy_set('gas')
    files = [f for f in files if len([x for x in good_set if x in f]) > 0]
    print len(files)
    if subset is not None:
        files = [f for f in files if len([x for x in subset if x in f]) > 0]
    # if season is None:
    #     files = glob.glob(os.getcwd() + \
    #                     '/input/FY/interval/ion_0627/{0}/json_{1}/*_{2}.json'.format(dirname, occtime, measure_type))
    # else:
    #     files = glob.glob(os.getcwd() + \
    #                       '/input/FY/interval/ion_0627/{0}/json_{1}/*_{2}_{3}.json'.format(dirname, occtime, measure_type, season))
    def get_name(string):
        idx = string.rfind('/')
        return string[idx + 1: idx + 9]
    data = []
    for x in files:
        with open (x, 'r') as rd:
            j = json.load(rd)
        data.append(j)
    data_str = 'series: [{0}]'.format(','.join(map(str, data)))
    data_str = data_str.replace('u\'', '\'')
    with open (os.getcwd() + '/input/FY/interval/ion_0627/piecewise_all/template.html', 'r') as rd:
        lines = rd.readlines()
    if season is None:
        mytitle = 'Region {1} Monthly {0} (kBtu/sq.ft) vs Temperature (F)'.format(measure_type, region)
    else:
        mytitle = '{1} Monthly {0} (kBtu/sq.ft) vs Temperature (F)'.format(measure_type, season.title())
    for i in range(len(lines)):
        lines[i] = lines[i].replace('series: []', data_str)
        lines[i] = lines[i].replace('Mytitle', mytitle)
        lines[i] = lines[i].replace('Myylabel', 'kBtu/sq.ft')
    if season is None:
        f = os.getcwd() + '/plot_FY_weather/html/by_region/{}_piecewise_all_region_{}.html'.format(measure_type, region)
    if subset is None:
        f = os.getcwd() + '/plot_FY_weather/html/by_region/{}_piecewise_all_region_{}_no_filter.html'.format(measure_type, region)
    with open (f, 'w+') as wt:
        wt.write(''.join(lines))
    print 'end'
    return

def compute_piecewise(measure_type, df_all, b, s):
    npar = 3
    if measure_type == 'gas':
        npar = 2
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format('ave'), 'eui': 'eui_gas', 'Timestamp': 'timestamp'})
        d = ltm.piecewise_reg_one_fromdb(b, s, npar, 'eui_gas', False, None, df_reg)
    elif measure_type == 'electric':
        df_reg = df_all.rename(columns={'Temperature_F': '{0}'.format('ave'), 'eui': 'eui_elec', 'Timestamp': 'timestamp'})
        d = ltm.piecewise_reg_one_fromdb(b, s, npar, 'eui_elec', False, None, df_reg)
    return d

def plot_piece(df, ax, title, color, measure_type, b, s, scatter=True,
               annote=False, jsondir=None, csvdir=None, season=None):
    temp = df.reset_index()
    d = compute_piecewise(measure_type, temp, b, s)
    if d is None:
        return None
    x0 = d['x_range'][0]
    x1 = d['x_range'][1]
    if type(d['breakpoint']) == tuple:
        b0 = d['breakpoint'][0]
        b1 = d['breakpoint'][1]
        x = np.array([x0, b0, b1, x1])
    else:
        x = np.array([x0, d['breakpoint'], x1])
    if scatter:
        ax.plot(d['x'], d['y'], 'o', c=color)
    y = d['fun'](x, *d['regression_par'])
    ax.plot(x, y, c='salmon')
    if annote:
        if measure_type == 'electric':
            ax.annotate(b, xy=(x[-1], y[-1]))
        else:
            ax.annotate(b, xy=(x[0], y[0]))
    ax.set_ylabel(lb.ylabel_dict[measure_type])
    if not jsondir is None:
        d_plot = {}
        d_plot['name'] = b
        x = map(lambda m: round(m, 4), x)
        y = map(lambda m: round(m, 4), y)
        d_plot['data'] = map(list, zip(x, y)) 
        if season is None:
            path = '{0}{1}_{2}.json'.format(jsondir, b, measure_type)
        else:
            path = '{0}{1}_{2}_{3}.json'.format(jsondir, b, measure_type, season)
        with open (path, 'w+') as wt:
            json.dump(d_plot, wt)
    if not csvdir is None:
        x = map(lambda m: round(m, 2), x)
        y = map(lambda m: round(m, 2), y)
        df = pd.DataFrame({'x': x, 'y': y})
        df['id'] = b
        df.to_csv('{0}{1}_{2}.csv'.format(csvdir, b, measure_type), index=False)
    return d

def fit_time(measure_type, region, season=None):
    conn = uo.connect('all')
    with conn:
        df_bs = pd.read_sql('SELECT Building_Number, ICAO, eui_elec, eui_gas, year, month, ave FROM EUAS_monthly_weather', conn)
        df_region = pd.read_sql('SELECT DISTINCT Building_Number, [Region_No.] FROM EUAS_monthly', conn)
    if measure_type == 'electric':
        good_set = gbs.get_energy_set('eui_elec')
    elif measure_type == 'gas':
        good_set = gbs.get_energy_set('gas')
    df_bs = pd.merge(df_bs, df_region, on='Building_Number', how='left')
    df_bs = df_bs[df_bs['Building_Number'].map(lambda x: x in good_set)]
    df_bs = df_bs[df_bs['Region_No.'] == str(region)]
    df_bs = df_bs[df_bs['eui_elec'].notnull()]
    df_bs = df_bs[df_bs['eui_gas'].notnull()]
    df_bs.sort_values(by=['Building_Number', 'year', 'month'], ascending=False, inplace=True)
    bs_pair = list(set(zip(df_bs['Building_Number'], df_bs['ICAO'])))
    df_bs['Timestamp'] = df_bs.apply(lambda r: '{}-{}'.format(int(r['year']),
                                                              int(r['month'])), axis=1)
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    ylabel = {'electric': 'electric (kBtu/sq.ft)', 'gas': 'gas kBtu/sq.ft'}
    col_dict = {'electric': 'eui_elec', 'gas': 'eui_gas'}
    print len(bs_pair)
    sns.set_style("whitegrid")
    # palette = sns.cubehelix_palette(len(bs_pair))
    palette = sns.color_palette('husl', len(bs_pair))
    sns.set_palette(palette)
    colors_rgb = [util.float2hex(x) for x in palette]
    sns.set_context("talk", font_scale=1)
    jsondir = os.getcwd() + '/plot_FY_weather/html/by_region/Region{}/piecewise_all/json/'.format(region)
    title = "Region {}".format(region)
    col = col_dict[measure_type]
    for i, (b, s) in enumerate(bs_pair):
        print b, s
        df = df_bs[df_bs['Building_Number'] == b]
        df = df.head(n=36)
        print df.head()
        points = df[col]
        min_time = df['Timestamp'].min()
        max_time = df['Timestamp'].max()
        bx = plt.axes()
        d0 = plot_piece(df, bx, title, colors_rgb[i], measure_type, b, s, scatter=False, annote=True, jsondir=jsondir, season=season)
    plt.xlabel('Temperature_F')
    # plt.show()
    if season is None:
        path = os.getcwd() + '/plot_FY_weather/html/by_region/Region{}/piecewise_all/{}.png'.format(region, measure_type)
    else:
        path = os.getcwd() + '/plot_FY_weather/html/by_region/Region{}/piecewise_all/{}_{}.png'.format(region, measure_type, season)
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()
    return

def plot_saving_oneplot(region, season=None):
    jsondir = os.getcwd() + '/plot_FY_weather/html/by_region/Region{}/piecewise_all/json/'.format(region)
    for measure_type in ['electric', 'gas']:
    # for measure_type in ['gas']:
        buildings = pd.read_csv(os.getcwd() + '/plot_FY_weather/html/by_region/{}.csv'.format(measure_type))
        subset = buildings[buildings['region'] == region]['Building_Number'].tolist()
        # fit_time(measure_type, region=region, season=season)
        plot_json(jsondir, measure_type, region=region, season=season, subset=None)

# fixme: remove filters for electric and gas eui, only remove ones with no sqft
def main():
    for region in range(1, 12):
    # for region in range(10, 11):
        plot_saving_oneplot(region)
    return

main()
