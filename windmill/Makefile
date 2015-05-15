#
#
#

VHACD_UTIL=/home/seth/src/hifi/build/tools/vhacd-util/vhacd-util


all: windmillCandidate-hull.obj


parts: windmillCandidate.fbx
	rm -rf parts
	mkdir -p parts
	$(VHACD_UTIL) -i $^ -o parts/windmillCandidate --split


join-parts:
	wavefront-obj-tool parts/*.obj -o joined.obj

windmillCandidate-hull.obj: windmillCandidate.fbx
	$(VHACD_UTIL) -g -i $^ -o $@ -m 10


download:
	wget https://hifi-public.s3.amazonaws.com/ryan/windmillCandidate.fbx

dist: windmillCandidate-hull.obj
	scp $^ headache:public_html/hifi/

clean:
	rm -rf *~ parts
