import pandas as pd
import os
import glob
import numpy as np

# get a set of buildings of a dataframe
def get_building(df):
    return set(df['Building Number'].tolist())

def check_num_bd(dfs):
    buildings = [get_building(df) for df in dfs]
    return [len(b) for b in buildings]

# check number of common buildings of two list of data frames
def check_common_bd_pair(dfs_1, dfs_2):
    buildings_1 = [get_building(df) for df in dfs_1]
    buildings_2 = [get_building(df) for df in dfs_2]
    assert(len(buildings_1) == len(buildings_2))
    return [len(buildings_1[i].intersection(buildings_2[i]))
            for i in range(len(buildings_1))]

def check_sheetname(excel, flag):
    if flag:
        excelfile = pd.ExcelFile(excel)
        print excelfile.sheet_names

# read 11 sheets of
def tocsv(excel, sheet_ids):
    filename = excel[excel.find('FY1'):]
    for i in sheet_ids:
        df = pd.read_excel(excel, sheetname=i)
        # filter out records with empty name
        df = df[pd.notnull(df['Building Number'])]
        outfile = '{0}/csv_FY/{1}_{2}.csv'.format(os.getcwd(), filename[:4], i + 1)
        print 'write to file' + outfile
        df.to_csv(outfile, index=False)

def excel2csv():
    filelist = glob.glob(os.getcwd() + '/input/FY/' + '*.xlsx')
    frames = []
    for excel in filelist:
        filename = excel[excel.find('FY1'):]
        print 'processing {0}'.format(filename)
        check_sheetname(excel, False)
        tocsv(excel, range(11))

def df_year(year):
    return [pd.read_csv(os.getcwd() + '/csv_FY/FY{0}_{1}.csv'.format(year, i)) for i in range(1, 12)]

def all_building_set(df_list):
    bd_set_listlist = [[get_building(df) for df in sheet] for sheet in df_list]
    bd_set_list = [reduce(set.union, z) for z in bd_set_listlist]
    return list(reduce(set.union, bd_set_list))

# return a dataframe marking which year of data is available for which building
def mark_bd(df_list, title_list):
    assert(len(df_list) == len(title_list))
    bd_set_listlist = [[get_building(df) for df in x] for x in df_list]
    bd_set_list = [reduce(lambda x, y: x.union(y), z) for z in bd_set_listlist]
    all_bd_set = reduce(lambda x, y: x.union(y), bd_set_list)
    mark_lists = [[1 if x in b else 0 for x in all_bd_set] for b in bd_set_list]
    return pd.DataFrame(dict(zip(title_list, mark_lists)))

def common_building_set(df_list):
    bd_set_listlist = [[get_building(df) for df in sheet] for sheet in df_list]
    bd_set_list = [reduce(set.union, z) for z in bd_set_listlist]
    return list(reduce(set.intersection, bd_set_list))

def region2building():
    filelist = glob.glob(os.getcwd() + '/csv_FY/*.csv')
    for csv in filelist:
        df = pd.read_csv(csv)
        year = int(df.ix[0, 'Fiscal Year'])
        bds = set(df['Building Number'].tolist())
        for b in bds:
            df_b = df[df['Building Number'] == b]
            outfile = (os.getcwd() + '/csv_FY/single/{0}_{1}.csv'.format(b, year))
            df_b.to_csv(outfile)

def building_info():
    filelist = glob.glob(os.getcwd() + '/csv_FY/' + '*.csv')
    dfs13 = df_year(13)
    dfs14 = df_year(14)
    dfs15 = df_year(15)
    df_listlist = [dfs13, dfs14, dfs15]
    print 'number of buildings'
    df = pd.DataFrame({'FY13':check_num_bd(dfs13),
                       'FY14':check_num_bd(dfs14),
                       'FY15':check_num_bd(dfs15)}, index=range(1, 12))
    print df
    df.to_csv(os.getcwd() + '/csv_FY/info/num_building.csv')

    print 'common buildings'
    df2 = pd.DataFrame({'FY13-14': check_common_bd_pair(dfs13, dfs14),
                        'FY14-15': check_common_bd_pair(dfs14, dfs15),
                        'FY13-15': check_common_bd_pair(dfs13, dfs15)},
                       index=range(1, 12))
    df2.to_csv(os.getcwd() + '/csv_FY/info/num_common_building.csv')
    print df2

    common = common_building_set(df_listlist)
    all_bd = all_building_set(df_listlist)
    print 'number of common buildings: {0}'.format(len(common))
    print 'number of all buildings: {0}'.format(len(all_bd))
    df3 = mark_bd(df_listlist, ['2013', '2014', '2015'])
    df3['Building Number'] = all_bd
    df3.to_csv(os.getcwd() + '/csv_FY/info/record_year.csv', index=False)
    print df3

def calculate():
    filelist = glob.glob(os.getcwd() + '/csv_FY/single/*.csv')
    for csv in filelist:
        df = pd.read_csv(csv)
        filename = csv[csv.find('single') + 7:]
        print filename
        df['elec'] = df['Electricity (KWH)'] * 3.412
        df['gas'] = df['Gas (Cubic Ft)'] * 1.026
        df['eui_elec'] = df['elec']/df['Gross Sq.Ft']
        df['eui_gas'] = df['gas']/df['Gross Sq.Ft']
        df['eui_oil'] = df['Oil (Gallon)']/df['Gross Sq.Ft']
        df['eui_water'] = df['Water (Gallon)']/df['Gross Sq.Ft']
        df['eui'] = (df['elec'] + df['gas'])/df['Gross Sq.Ft']
        bd = df.ix[0, 'Building Number']
        yr = int(df.ix[0, 'Fiscal Year'])
        # note: cols is for pandas v0.13.0, for v.017.0, use columns
        df.to_csv(os.getcwd() + '/csv_FY/single_eui/{0}_{1}.csv'.format(bd,yr),
                  cols = ['Region No.', 'Fiscal Month', 'Fiscal Year',
                          'Building Number', 'eui_elec', 'eui_gas', 'eui_oil',
                          'eui_water', 'eui'],
                  index=False)

def aggregate(year):
    filelist = glob.glob(os.getcwd() +
                         '/csv_FY/single_eui/*{0}.csv'.format(year))
    dfs = []
    for csv in filelist:
        df = pd.read_csv(csv)
        filename = csv[csv.find('single_eui') + 11:]
        # check monthly records availability
        '''
        if (len(df) != 12 or len(df['Fiscal Month'].unique()) != 12):
            print filename
        '''
        df['Region No.'] = df['Region No.'].map(lambda x: str(x))
        df['Fiscal Year'] = df['Fiscal Year'].map(lambda x: str(int(x)))
        df['Fiscal Month'] = df['Fiscal Month'].map(lambda x: str(int(x)))
        region = df.ix[0, 'Region No.']
        yr = df.ix[0, 'Fiscal Year']
        bd = df.ix[0, 'Building Number']
        df_agg = df.groupby('Fiscal Year').sum()
        df_agg['Region No.'] = region
        df_agg['Fiscal Year'] = yr
        df_agg['Building Number'] = bd
        dfs.append(df_agg)
    df_yr = pd.concat(dfs)
    df_yr.to_csv(os.getcwd() + '/csv_FY/agg/eui_{0}.csv'.format(year),
                 index=False)

def aggregate_allyear(yearlist):
    for year in yearlist:
        aggregate(year)

def euas2csv():
    df = pd.read_excel(os.getcwd() + '/input/FY/GSA_F15_EUAS_v2.2.xls',
                       sheetname=0)
    program_hd = ['GP', 'LEED', 'first fuel', 'Shave Energy',
                  'GSALink Option(26)', 'GSAlink I(55)', 'E4', 'ESPC',
                  'Energy Star']
    '''
    for hd in program_hd:
        print df[hd].value_counts()
    '''
    df.to_csv(os.getcwd() + '/csv_FY/program/GSA_F15_EUAS.csv', index=False,
              cols=['Building ID', 'GP', 'LEED', 'first fuel', 'Shave Energy',
                    'GSALink Option(26)', 'GSAlink I(55)', 'E4', 'ESPC',
                    'Energy Star', 'Cat'])
    df_bool2int = pd.read_csv(os.getcwd() + '/csv_FY/program/GSA_F15_EUAS.csv')
    for col in program_hd:
        df_bool2int[col] = df_bool2int[col].map(lambda x: 1 if x == '1_Yes'
                                              else 0)
    df_bool2int['Total Programs_v2'] = df_bool2int[program_hd].sum(axis=1)
    df_bool2int['Total Programs (Y/N)_v2'] = df_bool2int['Total Programs_v2'].map(lambda x: 1 if x > 0 else 0)
    df_bool2int.to_csv(os.getcwd() + '/csv_FY/program/GSA_F15_EUAS_int.csv',
                       index=False)

# join EUAS program info and eui info for year 2015
def join_program():
    df_eui = pd.read_csv(os.getcwd() + '/csv_FY/agg/eui_2015.csv')
    df_pro = pd.read_csv(os.getcwd() + '/csv_FY/program/GSA_F15_EUAS_int.csv')
    bd_eui = set(df_eui['Building Number'].tolist())
    bd_pro = set(df_pro['Building ID'].tolist())
    print 'number of buildings in eui_2015: {0}'.format(len(bd_eui))
    print 'number of buildings in program : {0}'.format(len(bd_pro))
    print 'number of common buildings: {0}'.format(len(bd_eui.intersection(bd_pro)))
    df_merge = pd.merge(df_eui, df_pro, how='inner', left_on='Building Number',
                        right_on = 'Building ID')
    df_merge.info()
    df_merge.drop('Building Number', inplace=True, axis=1)
    df_merge.to_csv(os.getcwd() + '/csv_FY/join/join_2015.csv', index=False)

def main():
    #excel2csv()
    #building_info()
    #region2building()
    #calculate()
    #aggregate_allyear([2013, 2014, 2015])
    #euas2csv()
    join_program()
main()
