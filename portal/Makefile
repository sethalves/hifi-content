#
#
#


all:


upload: rez-portal.js portal.fs override.js
	mkdir -p portal
	cp $^ portal/
	rsync --delete -avP portal/ headache.hungry.com:public_html/hifi/portal/


clean:
	rm -rf portal
	rm -f *~
