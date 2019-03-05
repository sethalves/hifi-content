#
#
#

all:

upload: puppet.js puppet.qml hit-detector.js
	rm -rf puppet
	mkdir -p puppet
	cp  $^ puppet/
	rsync --delete -avP puppet/ headache.hungry.com:public_html/hifi/puppet/

clean:
	rm -f *~
	rm -rf puppet
