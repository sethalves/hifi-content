#
#
#

all:

upload: dynamic-load-js-test.js dynamic-load-js-test.html dynamic-load-js-test-interface.js a.js
	mkdir -p dynamic-load-js-test
	cp $^ dynamic-load-js-test/
	rsync --delete -avP dynamic-load-js-test/ headache.hungry.com:public_html/hifi/dynamic-load-js-test/

clean:
	rm -f *~
	rm -rf dynamic-load-js-test
