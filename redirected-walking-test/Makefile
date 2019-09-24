#
#
#

all:

upload: redirected-walking-test.js
	rm -rf redirected-walking-test
	mkdir -p redirected-walking-test
	cp $^ redirected-walking-test/
	rsync --delete -avP redirected-walking-test/ seth@headache.hungry.com:public_html/hifi/redirected-walking-test/

clean:
	rm -f *~
	rm -rf redirected-walking-test
