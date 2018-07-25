#
#
#

all:


upload:
	rm -rf entity-utils
	mkdir -p entity-utils
	cp entity-utils.js entity-utils/
	rsync --delete -avP entity-utils/ headache.hungry.com:public_html/hifi/entity-utils/


clean:
	rm -f *~
	rm -rf entity-utils
