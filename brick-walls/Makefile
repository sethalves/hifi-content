#
#
#

all:

upload: brick-walls.js brick-walls.html brick-walls-interface.js
	mkdir -p brick-walls
	cp $^ brick-walls/
	rsync --delete -avP brick-walls/ headache.hungry.com:public_html/hifi/brick-walls/

clean:
	rm -f *~
	rm -rf brick-walls
