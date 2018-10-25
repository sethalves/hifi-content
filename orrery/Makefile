#
#
#

CPPFLAGS=-std=c++11
LDLIBS=cspice/lib/cspice.a -lstdc++ -lm
DATAFILES=data/pck00011.tpc data/naif0012.tls data/de438.bsp

all: orrery-web-api.cgi

orrery-web-api.cgi: orrery-web-api.o

orrery-web-api.o: orrery-web-api.cpp

orrery-web-api.cgi: orrery-web-api
	cp $< $@

install: orrery-web-api.cgi
	rm -rf orrery
	mkdir orrery
	cp orrery-web-api.cgi orrery/
	mkdir orrery/data
	cp -p $(DATAFILES) orrery/data/
	rsync -avP orrery/ headache.hungry.com:public_html/hifi/orrery/

clean:
	rm -f *~ orrery-web-api.o orrery-web-api orrery-web-api.cgi
	rm -rf orrery
