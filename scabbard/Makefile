#
#
#

all:


upload:
	rm -rf scabbard
	mkdir -p scabbard
	cp scabbard.js scabbard/
	rsync --delete -avP scabbard/ headache.hungry.com:public_html/hifi/scabbard/


clean:
	rm -f *~
	rm -rf scabbard
