#
#
#

# http://headache.hungry.com/~seth/hifi/hifi-openscad/bake.js

all:


clean:
	rm -f *~

upload: bake.js invoke-scad.cgi template-ES.js
	scp $^ headache.hungry.com:public_html/hifi/hifi-openscad/
