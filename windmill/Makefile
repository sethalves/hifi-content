#
#
#

VHACD_UTIL=/home/seth/src/hifi/build/tools/vhacd-util/vhacd-util


all:


parts: windmillcandidate.obj
	rm -rf parts
	mkdir -p parts
	$(VHACD_UTIL) -i $^ -o parts/windmillCandidate --split

#	$(VHACD_UTIL) -i $^ -o parts/windmillCandidate --split


join-parts:
	wavefront-obj-tool parts/*.obj -o joined.obj

windmillCandidate-hull.obj: windmillCandidate.fbx
	$(VHACD_UTIL) -g -i $^ -o $@ -m 10


download:
	wget https://hifi-public.s3.amazonaws.com/ryan/windmillCandidate.fbx
	wget https://hifi-public.s3.amazonaws.com/ryan/windmillcandidate.obj

dist: windmillCandidate-hull.obj
	scp $^ headache:public_html/hifi/

clean:
	rm -rf *~ parts
