#
#
#

all:

upload: rez-sword.js sword.js hit-detector.js
	rm -rf sword
	mkdir -p sword
	cp  $^ sword/
	rsync --delete -avP sword/ headache.hungry.com:public_html/hifi/sword/

clean:
	rm -f *~
	rm -rf sword
