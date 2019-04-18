#
#
#

all:


upload:
	rm -rf troll
	mkdir -p troll
	cp export/avatar.fst troll/
	cp export/troll.fbx troll/
	cp export/textures/* troll/
	cp avatar-animation.json troll/
	cp walk_fwd.fbx troll/
	cp walk_fwd_fast.fbx troll/
	cp jog_fwd.fbx troll/
	cp run_fwd.fbx troll/
	ln -s . troll/textures
	rsync --delete -avP troll/ headache.hungry.com:public_html/hifi/avatars/Troll/


clean:
	rm -f *~
	rm -rf troll
