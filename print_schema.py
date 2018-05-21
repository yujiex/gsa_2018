import sqlite3
import util_io as uo

# def print_db_schema(dbname):
#     conn = uo.connect('all')
#     for

with open ('/media/yujiex/work/GSA/merge/csv_FY/db/column_all.txt', 'r') as rd:
    lines = rd.readlines()
unique_lines = list(set(lines))
unique_lines.sort()
for x in unique_lines:
    print x[:-1]

