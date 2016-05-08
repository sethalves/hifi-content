#!/usr/bin/python

import sys
import json
import uuid


urlBase = 'http://headache.hungry.com/~seth/hifi/hifi-openscad'

# https://docs.python.org/2/library/json.html
# https://docs.python.org/2/library/json.html#json-to-py-table
inputEntities = json.load(sys.stdin)

newModelID = uuid.uuid4();
visualFile = str(newModelID) + '-visual.obj';
collisionFile = str(newModelID) + '-hull.obj';
inputsFile = str(newModelID) + '-inputs.obj';

print >> sys.stderr, '-----------------'
for inputEntity in inputEntities:
    entityType = inputEntity['type']
    entityTranslation = inputEntity['translation']
    entityRotation = inputEntity['rotation']
    entityScale = inputEntity['scale']
    entityColor = inputEntity['color']

    print >> sys.stderr, entityType, entityTranslation, entityRotation, entityScale, entityColor


print 'Content-Type: application/json\n\n'
print json.dumps({
    'success':'true',
    'modelURL': urlBase + '/models/' + visualFile,
    'collisionURL': urlBase + '/models/' + collisionFile
})
