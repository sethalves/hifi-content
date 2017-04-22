#
#
#

all:

upload: buggy.js buggy.html buggy-interface.js
	mkdir -p buggy
	cp $^ buggy/
	rsync --delete -avP buggy/ headache.hungry.com:public_html/hifi/buggy/

clean:
	rm -f *~
	rm -rf buggy
