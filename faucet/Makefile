
all: faucet.obj

faucet.stl: faucet.scad
	openscad -o $@ $^

faucet.obj: faucet.stl
	wavefront-obj-tool -n -c $^ -o $@ -L faucet.mtl -S faucet_mtl

clean:
	rm -f *~ faucet.stl faucet.obj

upload: faucet.obj faucet.mtl
	scp $^ headache:public_html/hifi/
