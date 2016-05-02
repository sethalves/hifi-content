#
#
#

GAMBIT_JS=/usr/local/Gambit/lib/_gambit.js

all: Test.js

test_.js test.js: test.scm
	gsc -warnings -target js -link test.scm

Test.js: test_.js test.js
	cat test_.js ${GAMBIT_JS} test.js > Test.js

# gsc -warnings -target js -link -flat chud2.scm
# gsc -c -target js test.scm /home/seth/build/gambit/lib/_std.scm
# gsc -link -l lib/_std.js test.js

clean:
	rm -f *~ test.js test_.js Test.js
