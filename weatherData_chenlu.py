import ConfigParser
import requests
import json
import argparse
import sys
import datetime
from datetime import date, timedelta


def readINI():
    Config = ConfigParser.ConfigParser()
    Config.read("weather.ini")
    sections = Config.sections()
    return (Config, sections)

def ConfigSectionMap(section, Config):
    dict1 = {}
    options = Config.options(section)
    for option in options:
        try:
            dict1[option] = Config.get(section, option)
            if dict1[option] == -1:
                DebugPrint("skip: %s" % option)
        except:
            print("exception on %s!" % option)
            dict1[option] = None
    return dict1

def writeData():
    (Config, sections) = readINI()
    weatherStation = ConfigSectionMap(sections[0],Config)
    past_days = ConfigSectionMap(sections[1],Config)["past_days"] 
  
    for key in weatherStation:
        fileName = str(weatherStation[key]) + ".csv"
        with open(fileName,'w') as write_file:
            for k in xrange (0, int(past_days)):
                past = date.today() - timedelta(days= k)
                ulr = 'http://www.wunderground.com/history/airport/' \
                + weatherStation[key] \
                + '/' + str(past.year) + '/' + str(past.month) + '/' + str(past.day)\
                + '/DailyHistory.html?req_city=Pittsburgh&req_state=PA&req_statename=Pennsylvania&reqdb.zip=50001&reqdb.magic=1&reqdb.wmo=99999&format=1'
                response = requests.get(ulr).content
                response = response.replace("<br />", "") 
                write_file.write(response)
       
writeData()