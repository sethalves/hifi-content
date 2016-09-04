#
#
#

all:

upload: earth.fbx gravity.js gravity.svg makePlanets.js moon.fbx planet-rezzer-ES.js
	scp $^ headache:public_html/hifi/NBody/

upload-s3: earth.fbx gravity.js gravity.svg makePlanets.js moon.fbx planet-rezzer-ES.js
	s3cmd put $^ s3://hifi-content/seth/production/NBody/
	s3cmd setacl s3://hifi-content/seth/production/NBody/ --acl-public --recursive

clean:
