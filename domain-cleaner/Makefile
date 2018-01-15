#
#
#

all:

upload: domain-cleaner.js domain-cleaner.html brick-walls-interface.js domain-cleaner.svg
	mkdir -p domain-cleaner
	cp $^ domain-cleaner/
	rsync --delete -avP domain-cleaner/ headache.hungry.com:public_html/hifi/brick-walls/

clean:
	rm -f *~
	rm -rf domain-cleaner
