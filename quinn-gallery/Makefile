#
#
#

all:
	(cd picture-c && make export)
	(cd picture-f && make export)
	(cd picture-i && make export)
	(cd picture-j && make export)
	(cd picture-m && make export)
	(cd picture-n && make export)
	(cd picture-s && make export)
	(cd picture-u && make export)
	rsync -avP export/ headache.hungry.com:public_html/hifi/quinn-gallery/

clean:
	rm -f *~
	(cd picture-c && make clean)
	(cd picture-f && make clean)
	(cd picture-i && make clean)
	(cd picture-j && make clean)
	(cd picture-m && make clean)
	(cd picture-n && make clean)
	(cd picture-s && make clean)
	(cd picture-u && make clean)
	rm -rf export/
