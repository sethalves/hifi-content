#
#
#

all:

upload: message-button.js make-button-panel.js
	rm -rf message-button
	mkdir -p message-button
	cp $^ message-button/
	rsync --delete -avP message-button/ seth@headache.hungry.com:public_html/hifi/message-button/

clean:
	rm -f *~
	rm -rf message-button
