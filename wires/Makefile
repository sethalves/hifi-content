#
#
#

SCHEME_LIBS='(seth cout)' '(srfi 60)'


%.obj: %.stl
	wavefront-obj-tool -c $^ -o $@


# TARGETS=wires-1.obj wires-2.obj wires-3.obj wires-4.obj wires-5.obj wires-6.obj wires-7.obj wires-9.obj wires-10.obj wires-11.obj wires-12.obj wires-13.obj wires-14.obj wires-15.obj wires-16.obj wires-17.obj wires-18.obj wires-19.obj wires-20.obj wires-21.obj wires-22.obj wires-23.obj wires-24.obj wires-25.obj wires-26.obj wires-27.obj wires-28.obj wires-29.obj wires-30.obj wires-31.obj wires-32.obj wires-33.obj wires-34.obj wires-35.obj wires-36.obj wires-37.obj wires-38.obj wires-39.obj wires-40.obj wires-41.obj wires-42.obj wires-43.obj wires-44.obj wires-45.obj wires-46.obj wires-47.obj wires-48.obj wires-49.obj wires-50.obj wires-51.obj wires-52.obj wires-53.obj wires-54.obj wires-55.obj wires-56.obj wires-57.obj wires-58.obj wires-59.obj wires-60.obj wires-61.obj wires-62.obj wires-63.obj

TARGETS=$(foreach nth,1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63,models/wires-${nth}.obj)


all: $(TARGETS)


libs:
	snow2 install $(SCHEME_LIBS)


include wires.make

wires.make: make-makefile.scm
	./make-makefile.scm > wires.make

clean:
	rm -f *~
	rm -rf models

very-clean: clean
	rm -rf seth srfi wires.make
