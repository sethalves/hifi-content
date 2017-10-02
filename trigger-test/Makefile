#
#
#

TARGETS=rez-trigger-test.js trigger-test.js

all: ${TARGETS}

upload: ${TARGETS}
	mkdir -p trigger-test
	cp $^ trigger-test/
	rsync --delete -avP trigger-test/ headache.hungry.com:public_html/hifi/trigger-test/

clean:
	rm -rf *~ trigger-test
