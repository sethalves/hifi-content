#
#
#

ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client

all:
	(cd curved-ramp && make $@)
	(cd landing-pad && make $@)
	(cd ring-tower && make $@)
	(cd second-ramp && make $@)
	(cd third-ramp && make $@)
	(cd tower-first-balcony && make $@)
	(cd tower-third-floor && make $@)

upload:
	(cd curved-ramp && make $@)
	(cd landing-pad && make $@)
	(cd ring-tower && make $@)
	(cd second-ramp && make $@)
	(cd third-ramp && make $@)
	(cd tower-first-balcony && make $@)
	(cd tower-third-floor && make $@)
	rsync --delete -avP tower-upload/ headache.hungry.com:public_html/hifi/tower/


# upload-atp: ring-tower.obj.gz ring-tower-hull.obj.gz ring-tower.mtl tower-ramp.png
# 	- $(ATP_CLIENT) -T ring-tower.obj.gz atp://$(DOMAIN)/ring-tower/ring-tower.obj.gz
# 	- $(ATP_CLIENT) -T ring-tower-hull.obj.gz atp://$(DOMAIN)/ring-tower/ring-tower-hull.obj.gz
# 	- $(ATP_CLIENT) -T ring-tower.mtl atp://$(DOMAIN)/ring-tower/ring-tower.mtl
# 	- $(ATP_CLIENT) -T ../tower-ramp/tower-ramp.png atp://$(DOMAIN)/ring-tower/tower-ramp.png


clean:
	(cd curved-ramp && make $@)
	(cd landing-pad && make $@)
	(cd ring-tower && make $@)
	(cd second-ramp && make $@)
	(cd third-ramp && make $@)
	(cd tower-first-balcony && make $@)
	(cd tower-third-floor && make $@)
	rm -rf tower-upload
