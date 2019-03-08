#
#
#

all:

upload: initial-rez.js
	rm -rf tower-grounds-1
	mkdir -p tower-grounds-1
	cp  $^ tower-grounds-1/
	rsync --delete -avP tower-grounds-1/ headache.hungry.com:public_html/hifi/tower-grounds-1/

clean:
	rm -f *~
	rm -rf tower-grounds-1
