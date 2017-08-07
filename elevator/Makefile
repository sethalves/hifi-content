#
#
#

all:

upload: elevator.js rez-elevator.js
	mkdir -p elevator
	cp $^ elevator/
	rsync --delete -avP elevator/ headache.hungry.com:public_html/hifi/elevator/

clean:
	rm -rf *~ elevator
