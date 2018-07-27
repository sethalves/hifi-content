#
#
#

all:


upload: bag.js bagES.js
	rm -rf bag
	mkdir -p bag
	cp $^ bag/
	rsync --delete -avP bag/ headache.hungry.com:public_html/hifi/bag/


clean:
	rm -f *~
	rm -rf bag
