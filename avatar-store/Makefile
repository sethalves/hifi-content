#
#
#

all:

upload: attachmentItemScript.js desktopAttacher.js wearableServer.js attachmentZoneShared.js marketplaceShared.js request.js wearableServer.js
	mkdir -p avatar-store
	cp $^ avatar-store/
	rsync --delete -avP avatar-store/ headache.hungry.com:public_html/hifi/avatar-store/

clean:
	rm -f *~
	rm -rf avatar-store
