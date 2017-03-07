#!/usr/bin/python

import sys
import json
import uuid
import subprocess

urlBase = 'http://headache.hungry.com/~seth/hifi/plants'
fsBase = '/home/seth/public_html/hifi/plants'

# https://docs.python.org/2/library/json.html
# https://docs.python.org/2/library/json.html#json-to-py-table
inputPlants = json.load(sys.stdin)

newPlantGroupID = uuid.uuid4();
groupJSONFile = 'plants-' + str(newPlantGroupID) + '.json'
groupSEXPFile = 'plants-' + str(newPlantGroupID) + '.sexp'

# jsonHandle = open(fsBase + '/groups/' + groupJSONFile, 'w')
# jsonHandle.write(json.dumps(inputPlants, indent = 4));
# print >> jsonHandle
# jsonHandle.close()

sexpHandle = open(fsBase + '/groups/' + groupSEXPFile, 'w')
sexpHandle.write('(\n')
for inputPlant in inputPlants:
    position = inputPlant['position']
    rotation = inputPlant['rotation']
    dimensions = inputPlant['dimensions']
    sexpHandle.write('((' + '%.5f' % position['x'] + ' ' +
                     '%.5f' % position['y'] + ' ' +
                     '%.5f' % position['z'] + ')' +
                     ' (' + '%.5f' % rotation['w'] + ' ' +
                     '%.5f' % rotation['x'] + ' ' +
                     '%.5f' % rotation['y'] + ' ' +
                     '%.5f' % rotation['z'] + ') ' +
                     '%.5f' % dimensions['y'] + ')\n')
sexpHandle.write(')\n')
sexpHandle.close()


print 'Content-Type: application/json\n\n'
print json.dumps({
    'success':'true',
    'modelURL': 'okokok'
})
