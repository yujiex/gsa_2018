import sqlite3
import pandas as pd
import os
import glob
import shutil

homedir = os.getcwd() + '/csv_FY/'

def connect(db):
    conn = sqlite3.connect('{0}/db/{1}.db'.format(homedir, db))
    return conn

# print dictionary d with 'limit' number of records
def print_dict(d, limit):
    count = 0
    iterator = iter(d)
    while count < limit:
        key = next(iterator)
        print '{0} -> {1}'.format(key, d[key])
        count += 1

def view_building(b, col):
    conn = connect('all')
    df = pd.read_sql('SELECT DISTINCT Building_Number, Fiscal_Year, Fiscal_Month, year, month, [Gross_Sq.Ft], [Region_No.], Cat, [{1}] FROM EUAS_monthly WHERE Building_Number = \'{0}\''.format(b, col), conn)
    return df

def create_header(title, templatepath=None, assetdir=None):
    lines = []
    if templatepath is None:
        lines.append('<!DOCTYPE html>')
        lines.append('<html>')
        lines.append('<head>')
        lines.append('<title>{0}</title>'.format(title))
        lines.append('<h1>{0}</h1>'.format(title))
    else:
        with open (os.getcwd() + templatepath, 'r') as rd:
            temp = rd.readlines()
            head_end_idx = (map(lambda x: '</head>' in x, temp)).index(True)
            lines = temp[:head_end_idx]
        for i in range(len(lines)):
            lines[i] = lines[i].replace("<title>Page title - Sitename</title>", "<title>{0}</title>".format(title))
            lines[i] = lines[i].replace("assets/", "{0}assets/".format(assetdir))
    return lines

def dir2html(dirname, suffix, title, outfile, files=None, templatepath = None, assetdir=None, style='width:700px;height:auto;', withname=True):
    if files is None:
        files = glob.glob(dirname + suffix)
    lines = create_header(title, templatepath, assetdir)
    if '.png' in suffix or '.PNG' in suffix:
        if withname:
            template = '<h2>name</h2>\n<img src="file" alt="No Data" style="{0}">'.format(style)
        else:
            template = '<img src="file" alt="No Data" style="{0}">'.format(style)
    lines.append('<h1>{0}</h1>'.format(title))
    for f in files:
        relative = f[- len(f) + len(dirname):]
        filename = f[f.rfind('/') + 1: f.find(suffix[1:])]
        line = template.replace('file', relative)
        line = line.replace('name', filename)
        lines.append(line)
    lines.append('</body>')
    lines.append('</html>')
    with open(dirname + outfile, 'w+') as wt:
        wt.write('\n'.join(lines))
    print 'end'
    return
    
def csv2html(path, rename_dict=None, format_dict=None):
    df = pd.read_csv(path)
    if not rename_dict is None:
        df.rename(columns=rename_dict, inplace=True)
    if not format_dict is None:
        for k in format_dict:
            df[k] = df[k].map(format_dict[k])
    df.to_html(path.replace('.csv', '.html'))
    return

# def dir2html(dirname, suffix, title, outfile):
#     files = glob.glob(dirname + suffix)
#     lines = []
#     lines.append('<!DOCTYPE html>')
#     lines.append('<html>')
#     lines.append('<head>')
#     lines.append('<title>{0}</title>'.format(title))
#     lines.append('<h1>{0}</h1>'.format(title))
#     if '.png' in suffix:
#         template = '<h2>name</h2>\n<img src="file" alt="No Data" style="width:700px;height:auto;">'
#     for f in files:
#         relative = f[- len(f) + len(dirname):]
#         filename = f[f.rfind('/') + 1: f.find(suffix[1:])]
#         line = template.replace('file', relative)
#         line = line.replace('name', filename)
#         lines.append(line)
#     lines.append('</body>')
#     lines.append('</html>')
#     with open(dirname + outfile, 'w+') as wt:
#         wt.write('\n'.join(lines))
#     print 'end'
#     return

