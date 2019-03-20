#
#
#

CPPFLAGS=-std=c++11 -Wall -pedantic -g
LDLIBS=-lsqlite3 -lstdc++ -lm

all: reputation-server.cgi

reputation-server.cgi: reputation-server.o

reputation-server.o: reputation-server.cpp

reputation-server.cgi: reputation-server
	cp $< $@



upload: reputation-server.cgi
	rm -rf reputation
	mkdir -p reputation/resources/modules
	mkdir -p reputation/resources/images/icons
	mkdir -p reputation/resources/css
	mkdir -p reputation/resources/js
	cp reputation-server.cgi reputation/
	cp nameTag.js reputation/
	cp resources/nameTag_ui.html reputation/resources/
	cp resources/modules/pickRayController.js reputation/resources/modules/
	cp resources/modules/nameTagListManager.js reputation/resources/modules/
	cp resources/modules/entityMaker.js reputation/resources/modules/
	cp resources/modules/objectAssign.js reputation/resources/modules/
	cp resources/modules/defaultLocalEntityProps.js reputation/resources/modules/
	cp resources/modules/textHelper.js reputation/resources/modules/
	cp resources/images/loadingSpinner.svg reputation/resources/images/
	cp resources/images/icons/nametag-i.svg reputation/resources/images/icons/
	cp resources/images/icons/nametag-a.svg reputation/resources/images/icons/
	cp resources/css/style.css reputation/resources/css/
	cp resources/js/nameTag_ui.js reputation/resources/js/
	rsync --delete -avP reputation/ headache.hungry.com:public_html/hifi/reputation/

clean:
	rm -f *~
	rm -f reputation-server.o reputation-server reputation-server.cgi
	rm -rf reputation
