#
#
#

all:

upload: makePlanets.js gravity.js gravity.svg earth.fbx moon.fbx
	scp $^ headache:public_html/hifi/NBody/

upload-s3: gravity.js earth.fbx moon.fbx planet-rezzer-ES.js
	s3cmd put $^ s3://hifi-content/seth/production/NBody/
	s3cmd setacl s3://hifi-content/seth/production/NBody/ --acl-public --recursive
