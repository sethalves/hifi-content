#!/usr/bin/python

import sys
import json
import uuid
import subprocess

urlBase = 'http://headache.hungry.com/~seth/hifi/hifi-openscad'
fsBase = '/home/seth/public_html/hifi/hifi-openscad'

# https://docs.python.org/2/library/json.html
# https://docs.python.org/2/library/json.html#json-to-py-table
inputEntities = json.load(sys.stdin)

# pick filenames
newModelID = uuid.uuid4();
visualFile = str(newModelID) + '-visual.obj';
collisionFile = str(newModelID) + '-hull.obj';
inputsFile = str(newModelID) + '-inputs.txt';
openscadFile = str(newModelID) + '.scad';
stlFile = str(newModelID) + '.stl';

# record the inputs we got
inputsHandle = open(fsBase + '/models/' + inputsFile, 'w')
inputsHandle.write(json.dumps(inputEntities, indent = 4));
print >> inputsHandle
inputsHandle.close()

# generate openscad input
# print >> sys.stderr, '-----------------'
openscadHandle = open(fsBase + '/models/' + openscadFile, 'w')
for inputEntity in inputEntities:
    entityType = inputEntity['type']
    entityTranslation = inputEntity['translation']
    entityRotation = inputEntity['rotation']
    entityScale = inputEntity['scale']
    entityColor = inputEntity['color']

    # print >> sys.stderr, entityType, entityTranslation, entityRotation, entityScale, entityColor

    if (entityType == 'cube'):
        item = 'cube(1, center=true)';
    elif (entityType == 'sphere'):
        item = 'sphere(0.5, center=true, $fn=24)';
    else:
        item = 'cube(1, center=true)';

    scad = """
    translate([{0}, {1}, {2}])
      rotate([{6}, {7}, {8}])
        scale([{3}, {4}, {5}])
          color([{9}, {10}, {11}, 1])
            {12};

    """

    openscadHandle.write(scad.format(entityTranslation['x'],
                                     entityTranslation['y'],
                                     entityTranslation['z'],
                                     entityScale['x'],
                                     entityScale['y'],
                                     entityScale['z'],
                                     entityRotation['x'],
                                     entityRotation['y'],
                                     entityRotation['z'],
                                     entityColor['red'] / 255.0,
                                     entityColor['green'] / 255.0,
                                     entityColor['blue'] / 255.0,
                                     item));



print >> openscadHandle
openscadHandle.close()

# process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
# process.wait()
# print process.returncode


# cmd = ['/run/myscript', '--arg', 'value']
# p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
# for line in p.stdout:
#     print line
# p.wait()
# print p.returncode


# run openscad


# openscad -o 71d3df7f-79f8-4efc-9757-7214cf23d06a.stl 71d3df7f-79f8-4efc-9757-7214cf23d06a.scad
cmd = ['/usr/bin/openscad', '-o', fsBase + '/models/' + stlFile, fsBase + '/models/' + openscadFile]
print >> sys.stderr, cmd
p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
for line in p.stdout:
    print >> sys.stderr, line
p.wait()
print >> sys.stderr, p.returncode


cmd = ['/usr/local/bin/wavefront-obj-tool', '-n', fsBase + '/models/' + stlFile, '-o', fsBase + '/models/' + visualFile]
# cmd = ['/usr/bin/meshlabserver', '-i', fsBase + '/models/' + stlFile, '-o', fsBase + '/models/' + visualFile]

print >> sys.stderr, cmd
p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
for line in p.stdout:
    print >> sys.stderr, line
p.wait()
print >> sys.stderr, p.returncode


print 'Content-Type: application/json\n\n'
print json.dumps({
    'success':'true',
    'modelURL': urlBase + '/models/' + visualFile,
    'collisionURL': urlBase + '/models/' + collisionFile
})
