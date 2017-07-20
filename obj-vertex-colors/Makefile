#
#
#

DOMAIN=eschatology
ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client

all:

upload: color-cube.obj
	- $(ATP_CLIENT) -T color-cube.obj atp://$(DOMAIN)/color-cube.obj
	scp $^ headache.hungry.com:public_html/hifi/

clean:
	rm -f *~
