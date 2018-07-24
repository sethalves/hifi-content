#
#
#

all:

upload: lib.js prog.js es0.js es1.js es2.js
	rm -rf require-test
	mkdir require-test
	cp $^ require-test/
	rsync --delete -avP require-test/ headache.hungry.com:public_html/hifi/require-test/

clean:
	rm -rf *~ require-test
