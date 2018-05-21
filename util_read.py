import pandas as pd
import os

homedir = os.getcwd() + '/csv_FY/'
master_dir = homedir + 'master_table/'

def read_building_energy(b, **kwargs):
    df = pd.read_csv(master_dir + 'energy_info_monthly.csv')
    df = df[df['Building Number'] == b]
    if 'outpath' in kwargs:
        df.to_csv(kwargs['outpath'] + '{0}.csv'.format(b), index=False)

