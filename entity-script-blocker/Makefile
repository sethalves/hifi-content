#
#
#

all:


upload: entity-script-blocker.js entity-script-blocker.qml
	rm -rf entity-script-blocker
	mkdir -p entity-script-blocker
	cp $^ entity-script-blocker/
	rsync --delete -avP entity-script-blocker/ headache.hungry.com:public_html/hifi/entity-script-blocker/


clean:
	rm -f *~
	rm -rf entity-script-blocker
