import sqlite3
import pandas as pd
import numpy as np
import os
import glob
import seaborn as sns
import pylab as P
import matplotlib.pyplot as plt
import util_io as uo

homedir = os.getcwd() + '/csv_FY/'
my_dpi = 300

def get_pc_array(array, period=720):
    y = array.tolist()[:len(array) / period * period]
    mx = np.reshape(y, (period, -1))
    mu = np.mean(y)
    mx_center = mx - mu
    sigma = np.dot(mx_center, mx_center.transpose())
    w, v = np.linalg.eig(sigma)
    return w, v

def get_pc_matrix(X):
    mu = np.mean(X)
    mx_center = X - mu
    sigma = np.dot(mx_center, mx_center.transpose())
    w, v = np.linalg.eig(sigma)
    return w, v

# period: the length of period, usually monthly, defaults to 720
def get_num_pc(array, period=720, cuts=[0.95]):
    w, v = get_pc_array(array, period)
    percent_var = np.cumsum(w / sum(w))
    result = []
    for c in cuts:
        result.append(sum([1 if x < c else 0 for x in percent_var]))
    return result

def pca_var():
    conn = uo.connect('weather_hourly_utc')
    with conn:
        df = pd.read_sql('SELECT * FROM downloaded', conn)
    nums95 = []
    nums99 = []
    ss = []
    stations = df['ICAO'].tolist()
    for s in stations:
        with conn:
            df_temp = pd.read_sql('SELECT Temperature_F FROM {0}'.format(s), conn)
        n95, n99 = get_num_pc(df_temp['Temperature_F'], cuts=[0.95,
                                                              0.99])
        nums95.append(n95)
        nums99.append(n99)
        ss.append(s)
        print s, n95, n99
    df_out = pd.DataFrame({'ICAO': ss, 'num_pc_95_percent':
                           nums95, 'num_pc_99_percent': nums99})
    df_out.to_csv(homedir + 'num_pc.csv')

def test():
    df = pd.read_csv(homedir + 'temp/test_DC0083ZZ.csv')
    n = len(df)
    y = df['Temperature_F'].tolist()[:n / 720 * 720]
    print len(y)
    mx = np.reshape(y, (720, -1))
    # print mx.shape
    mu = np.mean(mx)
    mx_center = mx - mu
    # plt.plot(mx_center)
    # plt.show()
    sigma = np.dot(mx_center, mx_center.transpose())
    print sigma.shape
    # print type(mx)
    w, v = np.linalg.eig(sigma)
    percent_var = np.cumsum(w / sum(w))
    print sum([1 if x < 0.95 else 0 for x in percent_var])
    # plt.plot(percent_var)
    # plt.show()
    
def plot_num_pc():
    df = pd.read_csv(homedir + 'num_pc.csv')
    print df['num_pc_99_percent'].value_counts()
    # f, axes = plt.subplots(2, 1, sharex=True)
    # plt.suptitle('number of principal component distribution, 95% (top) 99% (bottom) reconstruction error')
    # sns.set_style("whitegrid")
    # sns.set_palette("Set2", 2)
    # sns.distplot(df['num_pc_95_percent'], ax=axes[0])
    # sns.distplot(df['num_pc_99_percent'], ax=axes[1])
    # plt.gca().set_xlim(left=0)
    # # plt.show()
    # path = '/media/yujiex/work/project/images/num_pcs.png'
    # P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    # print 'end'

def main():
    # test()
    # pca_var()
    plot_num_pc()
    return 0

# main()
