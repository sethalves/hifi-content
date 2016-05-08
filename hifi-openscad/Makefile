#
#
#

# http://headache.hungry.com/~seth/hifi/hifi-openscad/bake.js

all:


clean:
	rm -f *~

upload: bake.js invoke-scad.cgi
	scp $^ headache.hungry.com:public_html/hifi/hifi-openscad/
