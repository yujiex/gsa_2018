import pandas as pd
import numpy as np
from scipy import stats

def lean(b, s, df, theme, base_temp):
    df = df[['year', 'month', s, base_temp, theme]]
    energys = sorted(df[theme].tolist())
    base = np.array(energys[:3]).mean()
    df['y_offset'] = df[theme] - base
    df['y_per_dd'] = df.apply(lambda r: 0 if r[base_temp] == 0 else r['y_offset']/r[base_temp], axis=1)
    df2 = df.groupby('month').mean()
    df2 = df2[['y_per_dd']]
    return df2, base
