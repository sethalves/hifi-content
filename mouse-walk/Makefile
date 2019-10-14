#
#
#

all:

upload: mouse-walk.js circle.svg
	rm -rf mouse-walk
	mkdir -p mouse-walk
	cp $^ mouse-walk
	rsync --delete -avP mouse-walk/ seth@headache.hungry.com:public_html/hifi/mouse-walk/

clean:
	rm -f *~
	rm -rf mouse-walk
