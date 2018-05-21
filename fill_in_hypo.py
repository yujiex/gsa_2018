import pandas as pd
import os
import glob
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import pylab as P

import label as lb

homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/'

def average_eui(df, suf):
    ave = df.groupby(['Fiscal Year']).mean()
    ave.to_csv(master_dir + 'ave_eui_{0}.csv'.format(suf))
    cnt = df.groupby(['Fiscal Year']).count()
    cnt.to_csv(master_dir + 'cnt_eui_{0}.csv'.format(suf))

def average_eui_program():
    df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    df = df[df['eui_elec'] >= 12]
    df = df[df['eui_gas'] >= 3]
    df = df[df['Cat'].isin(['A', 'I'])]
    df_pro = pd.read_csv(master_dir + 'ecm_program_tidy.csv')
    programs = df_pro['ECM program'].unique()
    for p in programs:
        buildings = df_pro[df_pro['ECM program'] == p]['Building Number'].unique()
        df_temp = df.copy()
        df_temp = df_temp[df_temp['Building Number'].isin(buildings)]
        average_eui(df_temp, p)
    files = [master_dir + 'ave_eui_{0}.csv'.format(p) for p in
             programs]
    dfs = []
    for p, f in zip(programs, files):
        df = pd.read_csv(f)
        df = df[['Fiscal Year', 'eui']]
        df.rename(columns={'eui': 'eui_' + p}, inplace=True)
        dfs.append(df)
    df_all = reduce(lambda x, y: pd.merge(x, y, on='Fiscal Year', how='left'), dfs)
    df_all.to_csv(master_dir + 'program_eui.csv', index=False)

def df_range(df, col):
    lst = df[col].tolist()
    return (min(lst), max(lst))
    
def get_total(df_all):
    kbtu = df_all.groupby('Fiscal Year').sum()
    # kbtu.info()
    kbtu = kbtu[['Total Electric + Gas']]
    area_year = df_all.groupby(['Fiscal Year', 'Building Number']).mean()
    area_year.reset_index(inplace=True)
    area = area_year.groupby('Fiscal Year').sum()
    area = area[['Gross Sq.Ft']]
    total = pd.merge(kbtu, area, left_index=True, right_index=True, how='inner')
    total['eui'] = total['Total Electric + Gas'] / total['Gross Sq.Ft']
    total.reset_index(inplace=True)
    return total

def get_total_cnt(df_all, title):
    def range2str(range_pair):
        return '\nn in range {0} to {1}'.format(range_pair[0], range_pair[1])
    total = get_total(df_all)
    temp = total.copy()
    df_eui = temp[['Fiscal Year', 'Gross Sq.Ft', 'eui', 'Total'
        ' Electric + Gas']]
    df_one = df_all.drop_duplicates(cols=['Building Number', 
        'Fiscal Year'])
    df_cnt = df_one.groupby(['Fiscal Year']).count()
    cnt_eui_range = df_range(df_cnt, 'Building Number')
    df_eui.rename(columns={x: '{0}_{1}{2}'.format(x, title,
                                                  range2str(cnt_eui_range))
                           for x in ['eui',
                                     'Gross Sq.Ft',
                                     'Total Electric + Gas']},
                  inplace=True)
    return df_eui

def read_total_cnt(df_all, suf, title):
    def range2str(range_pair):
        return '\nn in range {0} to {1}'.format(range_pair[0], range_pair[1])
    total = get_total(df_all)
    temp = total.copy()
    df_eui = temp[['Fiscal Year', 'Gross Sq.Ft', 'eui', 'Total'
        ' Electric + Gas']]
    df_cnt = pd.read_csv(master_dir + 'cnt_eui{0}.csv'.format(suf))
    cnt_eui_range = df_range(df_cnt, 'Building Number')
    df_eui.rename(columns={x: '{0}_{1}{2}'.format(x, title,
                                                  range2str(cnt_eui_range))
                           for x in ['eui',
                                     'Gross Sq.Ft',
                                     'Total Electric + Gas']},
                  inplace=True)
    return df_eui

def plot_trend(kw, title, df_merge, suf, multi, unit, plot_set):
    key_cols = [x for x in list(df_merge) if kw in x]
    # print key_cols
    lines = []
    maxs = []
    labels = []
    for c in key_cols:
        df_merge[c] = df_merge[c] * multi 
        line, = plt.plot(df_merge['Fiscal Year'], df_merge[c], ls='-',
                         lw=2, marker='o')
        maxs.append(max(df_merge[c].tolist()))
        lines.append(line)
        labels.append(c)
    label_cols = [x[x.find('_') + 1:] for x in key_cols]
    plt.legend(lines, labels, loc='center left', 
               bbox_to_anchor=(1, 0.5), prop={'size':13})
    plt.xlabel('Fiscal Year')
    plt.title('GSA Portfolio (A + I) {0} Trend'.format(kw))
    if plot_set == 'good_energy':
        plt.suptitle('With Electric EUI >= 12 kBtu/sq.ft/year and Gas EUI >= 3 kBtu/sq.ft/year')
    ylimit = max(maxs) * 1.1
    plt.ylim((0, ylimit))
    plt.fill_between([2004.5, 2006.5], 0, ylimit, facecolor='gray',
                     alpha=0.2)
    if kw == 'eui':
        plt.title('{0} Trend'.format(kw.upper()))
        plt.ylabel(lb.ylabel_dict['eui'])
    elif kw == 'Gross Sq.Ft':
        plt.ylabel('{0} Sq. Ft'.format(unit))
    elif kw == 'Total Electric + Gas':
        plt.ylabel('{0} kBtu'.format(unit))
    P.savefig(os.getcwd() + \
              '/plot_FY_annual/{0}_trend{1}_{2}.png'.format(title,
                                                            suf, plot_set), dpi=300, bbox_inches='tight')
    plt.close()

def read_energy(plot_set):
    df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    if plot_set == 'good_energy':
        df = df[df['eui_elec'] >= 12]
        df = df[df['eui_gas'] >= 3]
    df = df[df['Cat'].isin(['A', 'I'])]
    df = df[['Fiscal Year', 'Building Number']]
    df['good'] = True
    df_energy = pd.read_csv(master_dir + 'energy_info_monthly.csv')
    df_energy = df_energy[['Building Number', 'Fiscal Year', 'Gross'
        ' Sq.Ft', 'Electricity (kBtu)', 'Gas (kBtu)']]
    df_energy['Total Electric + Gas'] = df_energy['Electricity (kBtu)'] + \
        df_energy['Gas (kBtu)']
    df_all = pd.merge(df_energy, df, 
                      on=['Fiscal Year', 'Building Number'])
    df_all = df_all[df_all['good']]
    return df_all

def total_eui_pnnl(plot_set, eui_method):
    df_all = read_energy(plot_set)
    df_total = df_all.groupby('Fiscal Year').sum()
    df_sf = df_all.groupby(['Fiscal Year', 'Building Number']).mean()
    df_sf.reset_index(inplace=True)
    df_totalsf = df_sf.groupby('Fiscal Year').sum()
    df_eui = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    df_eui = df_eui[df_eui['Cat'].isin(['A', 'I'])]
    if plot_set == 'good_energy':
        df_eui = df_eui[df_eui['eui_elec'] >= 12]
        df_eui = df_eui[df_eui['eui_gas'] >= 3]
    df_eui = df_eui[['Fiscal Year', 'eui']]
    df_mean_eui = df_eui.groupby('Fiscal Year').mean()
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1)
    f, (ax1, ax2, ax3) = plt.subplots(3, sharex=True)
    x1 = df_mean_eui.index
    if eui_method == 'single':
        y1 = df_mean_eui['eui']*1e3
    elif eui_method == 'total over total':
        y1 = df_total['Total Electric + Gas'] / df_totalsf['Gross Sq.Ft']*1e3
    ax1.plot(x1, y1, color='blue', marker='o')
    for x,y in zip(x1, y1):
        ax1.annotate(int(round(y, 0)), xy = (x - 0.5, y-0.7e4),
                     fontsize=12, weight='semibold', color='black')
    ax1.set_ylabel('EUI Btu/SF')
    x2 = df_total.index
    y2 = df_total['Total Electric + Gas']*1e-6
    for x,y in zip(x2, y2):
        ax2.annotate(int(round(y, 0)), xy = (x - 0.5, y-0.7e3),
                     fontsize=12, weight='semibold', color='black')
    ax2.plot(x2, y2, color='red', marker='o')
    ax2.set_ylabel('Energy Use (BBtu)')
    x3 = df_totalsf.index
    y3 = df_totalsf['Gross Sq.Ft']*1e-3
    ax3.plot(x3, y3, color='green', marker='o')
    for x,y in zip(x3, y3):
        ax3.annotate(int(round(y, 0)), xy = (x - 0.5, y-0.9e4),
                     fontsize=12, weight='semibold', color='black')
    ax3.set_ylabel('Floor Space/kSF')
    # if plot_set == 'all':
    plt.sca(ax1)
    plt.yticks(range(40000, 90000, 20000), ['40k', '60k', '80k'])
    ax1.set_ylim((40000, 90000))
    plt.sca(ax2)
    plt.yticks(range(5000, 19000, 5000), ['5k', '10k', '15k'])
    # ax2.set_ylim((5000, 19000))
    plt.sca(ax3)
    plt.yticks(range(100000, 210000, 50000), ['100k', '150k', '200k'])
    # ax3.set_ylim((100000, 200000))
    # elif plot_set == 'good_energy':
    #     label1 = ax1.get_yticklabels()
    #     for i in range(len(label1)):
    #         label1[i] = '{0}k'.format(20 + 10 * i)
    #     ax1.set_yticklabels(label1)
    #     label2 = ax2.get_yticklabels()
    #     for i in range(len(label2)):
    #         label2[i] = '{0}k'.format(2 + i)
    #     # ax2.set_yticklabels(label2)
    #     label3 = ax3.get_yticklabels()
    #     for i in range(len(label3)):
    #         label3[i] = '{0}k'.format(30 + 10 * i)
    #     ax3.set_yticklabels(label3)

    plt.xlim((2002, 2013))
    plt.xlabel('Fiscal Year')
    my_dpi=100
    # plt.show()
    path = os.getcwd() + \
        '/plot_FY_annual/trend_pnnl_{0}_{1}.png'.format(plot_set,
                                                        eui_method)
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    plt.close()
    return

def total_eui(plot_set):
    df_all = read_energy(plot_set)
    df_ecm = pd.read_csv(master_dir + 'ECM/ecm_highlevel_long.csv')
    ecm_bds = df_ecm['Building Number'].unique()
    df1 = df_all[df_all['Building Number'].isin(ecm_bds)]
    df2 = df_all[~df_all['Building Number'].isin(ecm_bds)]
    # total = read_total_cnt(df_all, '', 'All building')
    # wecm = read_total_cnt(df1, '_wecm', 'Building with ECM')
    # woutecm = read_total_cnt(df2, '_woutecm', 'Building without ECM')
    total = get_total_cnt(df_all, 'All building')
    wecm = get_total_cnt(df1, 'Building with ECM')
    woutecm = get_total_cnt(df2, 'Building without ECM')
    df_merge = reduce(lambda x, y: pd.merge(x, y, on='Fiscal Year',
                                            how='inner'), [total,
                                                           wecm, woutecm])
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1.5)
    df_merge = df_merge[df_merge['Fiscal Year'] < 2016]
    plot_trend('Gross Sq.Ft', 'area', df_merge, '', 1e-6, 'Million', plot_set)
    # plot_trend('eui', 'eui', df_merge, '')
    plot_trend('Total Electric + Gas', 'totalkbtu', df_merge, '', 1e-9, "Billion", plot_set)
    
def plot_eui_trend(df, ax):
    df2 = get_total(df)
    # df2.info()
    df2 = df2[df2['Fiscal Year'] < 2016]
    line, = plt.plot(df2['Fiscal Year'], df2['eui'], ls='-', lw=2,
                     marker='o')
    return line

def program_eui():
    df_eng = read_energy('good_energy')
    df_pro = pd.read_csv(master_dir + 'ecm_program_tidy.csv')
    programs = list(set(df_pro['ECM program'].tolist()))
    dfs = []
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 8)
    sns.set_context("talk", font_scale=1.5)
    bx = plt.axes()
    lines = []
    labels = []
    programs.remove('Energy Star')
    for p in programs:
        buildings = df_pro[df_pro['ECM program'] == p]['Building Number'].unique()
        df_temp = df_eng.copy()
        df_temp = df_temp[df_temp['Building Number'].isin(buildings)]
        df_temp = df_temp[['Building Number', 'Fiscal Year', 'Gross Sq.Ft', 'Total Electric + Gas']]
        line = plot_eui_trend(df_temp, bx)
        lines.append(line)
        labels.append('{0} (n={1})'.format(p, len(df_temp['Building'
            ' Number'].unique())))
    plt.title('Energy Program EUI Trend')
    plt.ylabel(lb.ylabel_dict['eui'])
    plt.xlabel('Fiscal Year')
    plt.gca().set_ylim(bottom=0)
    ylimit = bx.get_ylim()
    plt.fill_between([2004.5, 2006.5], 0, ylimit, facecolor='gray',
                     alpha=0.2)
    plt.legend(lines, labels, loc='center left', 
               bbox_to_anchor=(1, 0.5), prop={'size':13})
    P.savefig(os.getcwd() + '/plot_FY_annual/program_trend.png', dpi =
              300, bbox_inches='tight')
    plt.close()

def average_eui_catecm():
    df = pd.read_csv(master_dir + 'eui_by_fy_wcat.csv')
    df = df[df['eui_elec'] >= 12]
    df = df[df['eui_gas'] >= 3]
    df = df[df['Cat'].isin(['A', 'I'])]
    ave = df.groupby(['Fiscal Year']).mean()
    ave.to_csv(master_dir + 'ave_eui.csv')
    cnt = df.groupby(['Fiscal Year']).count()
    cnt.to_csv(master_dir + 'cnt_eui.csv')

    df_ecm = pd.read_csv(master_dir + 'ECM/ecm_highlevel_long.csv')
    ecm_bds = df_ecm['Building Number'].unique()
    df_withecm = df[df['Building Number'].isin(ecm_bds)]
    ave = df_withecm.groupby(['Fiscal Year']).mean()
    ave.to_csv(master_dir + 'ave_eui_wecm.csv')
    cnt = df_withecm.groupby(['Fiscal Year']).count()
    cnt.to_csv(master_dir + 'cnt_eui_wecm.csv')

    df_woutecm = df[~df['Building Number'].isin(ecm_bds)]
    ave = df_woutecm.groupby(['Fiscal Year']).mean()
    ave.to_csv(master_dir + 'ave_eui_woutecm.csv')
    cnt = df_woutecm.groupby(['Fiscal Year']).count()
    cnt.to_csv(master_dir + 'cnt_eui_woutecm.csv')
    
def read_eui_cnt(suf, theme, title):
    def range2str(range_pair):
        return '\nn in range {0} to {1}'.format(range_pair[0], range_pair[1])
    df_eui = pd.read_csv(master_dir + 'ave_eui{0}.csv'.format(suf))
    df_eui = df_eui[['Fiscal Year', theme]]
    df_cnt = pd.read_csv(master_dir + 'cnt_eui{0}.csv'.format(suf))
    cnt_eui_range = df_range(df_cnt, 'Building Number')
    df_eui.rename(columns={theme: title + range2str(cnt_eui_range)},
                  inplace=True)
    return df_eui

def plot_program_eui(theme):
    sns.set_style("whitegrid")
    sns.set_palette("Set2", 8)
    sns.set_context("talk", font_scale=1.5)
    df_pro = pd.read_csv(master_dir + 'ecm_program_tidy.csv')
    programs = df_pro['ECM program'].unique()
    dfs = [read_eui_cnt('_' + p, theme, p) for p in programs]
    df_all = reduce(lambda x, y: pd.merge(x, y, on='Fiscal Year',
                                          how='inner'), dfs)
    df_all = df_all[df_all['Fiscal Year'] < 2016]
    lines = []
    cols = list(df_all)
    cols.remove('Fiscal Year')
    bx = plt.axes()
    maxs = []
    for x in cols:
        line, = plt.plot(df_all['Fiscal Year'], df_all[x], ls='-',
                         lw=2, marker='o')
        maxs.append(max(df_all[x].tolist()))
        lines.append(line)
    plt.legend(lines, cols, loc='center left', 
               bbox_to_anchor=(1, 0.5), prop={'size':13})
    ylimit = max(maxs) * 1.1
    plt.plot([2013.75] * 2, [0, ylimit], '--', color='yellow')
    plt.plot([2014.75] * 2, [0, ylimit], '--', color='yellow')
    bx.annotate('GSALink', xy = (2013.5, ylimit * 0.2), fontsize=15,
                weight='semibold', color='gray')
    plt.ylim((0, ylimit))
    plt.fill_between([2004.5, 2006.5], 0, ylimit, facecolor='gray',
                     alpha=0.2)
    plt.title('GSA Portfolio (A + I) Average EUI Trend by Programs')
    plt.xlabel('Fiscal Year')
    plt.ylabel(lb.ylabel_dict[theme])
    P.savefig(os.getcwd() + '/plot_FY_annual/program_eui.png', dpi =
              300, bbox_inches='tight')
    plt.close()

def plot_total_eui(theme):
    sns.set_style("whitegrid")
    sns.set_palette("Set2")
    sns.set_context("talk", font_scale=1.5)
    df_eui = read_eui_cnt('', theme, 'All Building')
    df_eui_wecm = read_eui_cnt('_wecm', theme, 'Building with ECM')
    df_eui_woutecm = read_eui_cnt('_woutecm', theme, 
                                  'Building without ECM')
    df_all = reduce(lambda x, y: pd.merge(x, y, on='Fiscal Year',
                                          how='inner'), [df_eui,
                                                         df_eui_wecm,
                                                         df_eui_woutecm])
    df_all = df_all[df_all['Fiscal Year'] < 2016]
    lines = []
    cols = list(df_all)
    cols.remove('Fiscal Year')
    bx = plt.axes()
    for x in cols:
        line, = plt.plot(df_all['Fiscal Year'], df_all[x], ls='-',
                         lw=2, marker='o')
        lines.append(line)
    plt.legend(lines, cols, loc='center left', 
               bbox_to_anchor=(1, 0.5), prop={'size':13})
    ylimit = 90
    plt.ylim((0, ylimit))
    plt.fill_between([2004.5, 2006.5], 0, ylimit, facecolor='gray',
                     alpha=0.2)
    plt.title('GSA Portfolio (A + I) Average EUI Trend')
    plt.xlabel('Fiscal Year')
    plt.ylabel(lb.ylabel_dict[theme])
    P.savefig(os.getcwd() + '/plot_FY_annual/ave_eui.png', dpi = 300,
              bbox_inches='tight')
    plt.close()
    
def main():
    # average_eui_program()
    # average_eui_catecm()
    # plot_total_eui('eui')
    total_eui('all')
    # total_eui_pnnl('all', 'single')
    # total_eui_pnnl('all', 'total over total')
    # total_eui_pnnl('good_energy', 'single')
    # total_eui_pnnl('good_energy', 'total over total')

    # program_eui()
    # plot_program_eui('eui')
    return
    
main()
