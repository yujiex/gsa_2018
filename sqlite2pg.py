import pandas as pd
import os
import glob
import psycopg2
import sqlite3
import sql

homedir = os.getcwd() + '/csv_FY/'
weatherdir = os.getcwd() + '/csv_FY/weather/'

def convert_db():
    dbs = glob.glob(homedir + 'db/*.db')
    conn_pg = psycopg2.connect(dbname='postgres', user='postgres',
                               host='localhost', password='test')
    cur_pg = conn_pg.cursor()
    cur_pg.execute("select relname from pg_class where relkind='r' and relname !~ '^(pg_|sql_)';")
    pg_tables = cur_pg.fetchall()
    pg_tables = [x[0] for x in pg_tables]
    for db in dbs[3:]:
        print db
        conn = sqlite3.connect(db)
        c = conn.cursor()
        c.execute("SELECT name FROM sqlite_master WHERE type='table';")
        tables = c.fetchall()
        for table in tables[1:2]:
            name = table[0]
            print name
            data = pd.read_sql("SELECT * from {0}".format(name), conn)
            # data = data[['Property_Name', 'Street_Address', 'Street_Address_2', 'City/Municipality', 'State/Province', 'Postal_Code', 'Country', 'Year_Built', 'Self-Selected_Primary_Function', 'Occupancy_(%)']]
            data = data[['Property_Name', 'Street_Address', 'Street_Address_2', 'City/Municipality', 'State/Province', 'Postal_Code', 'Year_Built', 'Self-Selected_Primary_Function']]
            if not name.lower() in pg_tables:
                sql.write_frame(data, name, conn_pg,
                                flavor="postgresql",
                                if_exists="replace")

def main():
    convert_db()
    return
    
main()
