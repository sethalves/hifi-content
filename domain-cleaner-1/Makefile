#
#
#

all:

upload: domain-cleaner.js domain-cleaner.svg domain-clean.qml
	mkdir -p domain-cleaner
	cp $^ domain-cleaner/
	rsync --delete -avP domain-cleaner/ headache.hungry.com:public_html/hifi/domain-cleaner/

clean:
	rm -f *~
	rm -rf domain-cleaner
