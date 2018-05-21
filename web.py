import pandas as pd
import numpy as np
import os
import glob
import seaborn as sns
import sqlite3
import util_io as uo

def main():
    dirname = '/media/yujiex/work/GSA/merge/plot_FY_annual/presentation_img/'
    files = ['{0}Slide{1}.PNG'.format(dirname, x) for x in range(1,
                                                                 35)]
    uo.dir2html(dirname,
                '*.PNG', 'Final presentation', 'presentation.html',
                files=files,
                templatepath='/css_template/01-bootstrap-kickoff-template/index.html',
                assetdir='../', withname=False)
    return 0

main()
