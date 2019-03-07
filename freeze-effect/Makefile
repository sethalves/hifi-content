#
#
#

all:

upload: freeze-effect.js lockdown.js
	rm -rf freeze-effect
	mkdir -p freeze-effect
	cp  $^ freeze-effect/
	rsync --delete -avP freeze-effect/ headache.hungry.com:public_html/hifi/freeze-effect/

clean:
	rm -f *~
	rm -rf freeze-effect
