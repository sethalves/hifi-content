#
#
#

all:

upload: brick-walls.js
	mkdir -p brick-walls
	cp $^ brick-walls/
	rsync -avP brick-walls/ headache.hungry.com:public_html/hifi/brick-walls/

clean:
	rm -f *~
	rm -rf brick-walls
