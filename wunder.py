import urllib2
import json
api_key = '8561d3582d3c2773'
f = urllib2.urlopen('http://api.wunderground.com/api/{0}/history_20060405/q/KJFK.json'.format(api_key))
json_string = f.read()
print json_string
# parsed_json = json.loads(json_string)
# location = parsed_json['location']['city']
# print location
# temp_f = parsed_json['current_observation']['temp_f']
# print "Current temperature in %s is: %s" % (location, temp_f)
f.close()
