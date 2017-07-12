<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Body</a></li>
<li><a href="#sec-2">2. Hands</a></li>
<li><a href="#sec-3">3. Rezzing</a></li>
<li><a href="#sec-4">4. Script</a></li>
<li><a href="#sec-5">5. Hosting</a></li>
<li><a href="#sec-6">6. Rezzing</a></li>
<li><a href="#sec-7">7. Bugs</a></li>
</ul>
</div>
</div>

A clock for High Fidelity has several parts: A model for the body of
the clock (including a texture for the face), a model for each hand, a
server-side script to update the hands, and a client-side script to
rez the clock.  I'll walk through each part and explain what was done;
the result is in the [clock source](https://github.com/sethalves/hifi-content/tree/master/clock).  The actual commands used for the
recipe can be seen by running make on the [Makefile](https://github.com/sethalves/hifi-content/blob/master/clock/Makefile).

# Body<a id="sec-1" name="sec-1"></a>

OpenSCAD has a cylinder primative (see [cheatsheet](http://www.openscad.org/cheatsheet/)), and one of the
parameters is [how many segments](https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Other_Language_Features#.24fa.2C_.24fs_and_.24fn) will make up the circles at the ends.
This allows an easy way to create some dodecagons:

    cylinder(h=0.08, r1=0.5, r2=0.5, center=true, $fn=12);

The face of the clock should face upward (positive-y in High Fidelity), so the scad file has

    rotate([-90, 0, 0]) { ... }

and the face of the clock should be in positive x and z space, because
that's what `wavefront-obj-tool` expects when it maps a texture onto a
model.  The face is put into position with

    translate([0.45, 0, 0.45]) { ... }

In the `Makefile` is a rule to process the `clock.scad` file and
export `clock.stl`.  High Fidelity doesn't support [STL model files](https://en.wikipedia.org/wiki/STL_(file_format)),
but it's easy to convert from STL to [OBJ](https://en.wikipedia.org/wiki/Wavefront_.obj_file) with `wavefront-obj-tool`.

The image for the face of the clock is made by starting with a blank
(white) png image from `pbmmake` (of [Netpbm](http://netpbm.sourceforge.net/)):

    pbmmake -white 480 480 | pnmtopng > clock-face-blank.png

and then drawing numbers in a circle with `convert` from [ImageMagick](https://www.imagemagick.org/script/convert.php).

    convert -pointsize 40 -fill black
      -draw 'text 320,94 "1"'
      -draw 'text 386,160 "2"'
      ...
      -draw 'text 210,70 "12"'

The face texture is applied to the clock's model with:

    wavefront-obj-tool clock-untextured.obj -o clock-offset.obj
        -U -M 0.9 0.9
        -L clock.mtl -S clock_face_mtl

This reads in `clock-untextured.obj` and maps a texture from the top
down onto the model.  `-M 0.9 0.9` means that the texture should be
stretched to cover a square in the xz plane from (0, 0) to (0.9, 0.9).
This is the bounding size in model space of the clock face, as seen
from above.  The `-U` argument means that only upward-facing (normal
has positive y) faces should have their material set.  OBJ files have
their [material specifications](https://en.wikipedia.org/wiki/Wavefront_.obj_file#Referencing_materials) in a separate file.  These material
files are usually found next to (in the same directory as) the OBJ
files that refer to them.  The `-L clock.mtl -S clock_face_mtl`
arguments set the name of the material file and the name of the
material, respectively.  An mtl file can hold more than one material
definition &#x2013; each has its own name.

If you've run the Makefile, you can see the resulting model with
meshlab

    meshlab clock.obj

Meshlab has many options under the `Render` menu which can be enabled
to check that the model is consistent and water-tight.  It can also
show the model-space axis so it's possible to confirm that the model
is oriented as expected.

The final step in the creation of the OBJ model files is to [gzip](https://www.gnu.org/software/gzip/manual/) them.
`interface` will recognize a file that ends in `.obj.gz` and
uncompress it before reading.  OBJ is a plain-text format and
the files can be quite large for models with many triangles.

# Hands<a id="sec-2" name="sec-2"></a>

OpenSCAD is also used for the clock hands. In the repository
are two files: `hour-hand.scad` and `minute-hand.scad`.  Each contains
a single OpenSCAD [polyhedron](https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#polyhedron) statement.  They produce stretched tetrahedrons
of different lengths, and these will serve as the hands of the clock.  Like
the clock body, they are both processed into STL and then OBJ files (though
they receive no texture).  Again, Meshlab can be used to examine the
intermediate and final model files.

# Rezzing<a id="sec-3" name="sec-3"></a>

High Fidelity can run javascript programs in a [variety](https://wiki.highfidelity.com/wiki/Script_Types_Overview) of places:
-   Interface can load a script from a url: This script will be
    running on the client, and wont necessarily be running in the
    clients of other avatars.
-   Interface can run an entity script: [Entities](https://wiki.highfidelity.com/wiki/Entity) have a [property](https://wiki.highfidelity.com/wiki/EntityItemProperties)
    called `script` which can be the url of a script. Any Interface
    that is told about this entity will download and run the script.
    Every client who is present will have its own copy of that script
    running, independently.
-   Scripted Agent: A domain can run an assignment-client (server)
    which act like a headless version of Interface. This usually
    causes an avatar to appear in-world, and the AC can run a script
    much like Interface can.
-   Server Script: In addition to their `script` property, Entities
    have a `serverScripts` property which will cause an
    assignment-client to run the script, even without any agents
    (Interface or Agent ACs) present.  Only one copy of such a script
    will be running, regardless of how many avatars visit the area.

The clock's [rez script](https://github.com/sethalves/hifi-content/blob/master/clock/rez-clock.js) is the first kind.  It's run from `interface`
by open the `Running Scripts` dialog and selecting `from url` or `from
disk`.  It calls `Entities.addEntity` once for the clock body and once
for each clock hand.  The hour-hand and minute-hand Entities
*children* of the clock body.  Rather than a world-frame `position`
and `rotation`, they are given `localPosition` and `localRotation` and
a `parentID` set to the ID of the clock.  This means that when the
clock moves, the hands will go along with it.  The hands are also
given a non-default `registrationPoint`.  The `registrationPoint` is a
vector that defines a position within the local bounding-box of the
model.  The default is `{ x: 0.5, y: 0.5, z: 0.5 }`, meaning the
center of the bounding-box.  This rez script sets each
`registrationPoint` to `{ x: 0.5, y: 0.0, z: 0.0 }`.  This causes the
center of rotation for the hand entities to be down at the thick end
of the hands.

At the end of the rez script, a Server Script is added to the clock
with `Entities.editEntity`.  This edit also updates the clocks
`userData` property to encode the IDs of the clock hands.

# Script<a id="sec-4" name="sec-4"></a>

The clock uses the server [script](https://github.com/sethalves/hifi-content/blob/master/clock/clock.js) (clock.js) to update the hands about
once per minute.  In the script, `preload` is the first thing to be
called, and will have, as an argument, the [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) of the entity whose
`serverScripts` property caused this script to be run.  This script is
only given the entityID of the main clock body, and needs to know the
entityIDs of the clock hands.  It expects these IDs to be stored in
the `userData` property of the clock.

Once per minute it gets the current (UTC) time and does a bit of math,
before calling `Entities.editEntity` on each hand.  The edits change
the `localRotation` property, causing the hands to rotate relative to
the clock body.

# Hosting<a id="sec-5" name="sec-5"></a>

To be used by a High Fidelity server, all these model and script files need to be
available over the network.  There are 3 main places for these types of files:
-   local filesystem of a user's computer
-   a webserver
-   a [High Fidelity asset-server](https://wiki.highfidelity.com/wiki/Asset_Server_(ATP))

A script on a user's local filesystem can be loaded by that user's
`interface`, but wont be of any use to servers (assuming the servers
aren't also running on the local computer) or to other users.  A
webserver is a good option, but setting up a webserver on the greater
internet isn't easy.  Hifi's asset-server is a good choice, because
once a hifi domain is up and running, the asset-server is already
configured and ready.

The files need to be copied from the local build diretory to the
asset-server.  One way is to launch `interface` and use the `Asset
Browser` dialog to upload each of the needed files: clock.obj.gz,
clock.mtl, clock-face.png, rez-clock.js, hour-hand.obj.gz,
minute-hand.obj.gz, clock.js.  Notice that `rez-clock.js` is missing
from this list &#x2013; client scripts can't be run from an asset-server
(though they can be run from a web-server).  The reason is that
the meaning of the url `atp:/rez-clock` is going to be different
for different domains.

Another way to upload these files is `atp-client`, found in
the tools directory in the [main High Fidelity repository](https://github.com/highfidelity/hifi).  If you
built the software on Linux, it should be in the build directory:
`build/tools/atp-client/atp-client`.  This tool can be used to (among
other things) upload files to an asset-server.

    ./atp-client -u <<username>>:<<password>> -T clock.obj.gz atp://eschatology-dev/clock/clock.obj.gz

The clock's `Makefile` has a target called `upload-atp` which will use
`atp-client` to upload the files.  Before this will work, the
`ATP_CLIENT` near the top of the `Makefile` will need to be correctly
set, and the user will need to have write-permissions for the
asset-server.

Notice this other form of ATP URL &#x2013; it includes the domain-name along
with the path.  atp-client supports these, but `interface` and the
script-server don't (yet?) allow cross-domain ATP urls, they only
accept the shorter form.

# Rezzing<a id="sec-6" name="sec-6"></a>

Finally, once the files are created and placed on the domain's
asset-server, connect to the domain with `interface` and open the
`Running Scripts` dialog.  Use the `from file` button and navigate to
and select `rez-clock.js` script.  If all goes well, the clock should
appear in front of the avatar.  This script runs once and then stops.
If it's reloaded, it will make a new clock.  Once the clock is
created, the script can be deleted from the `Running Scripts` list.
Using the local filesystem for the rez script works, because it
just needs to be run once, by the local `interface`.  No server
or other user needs to run it.

# Bugs<a id="sec-7" name="sec-7"></a>

As of this writing, there is a bug that will keep `clock.js` from
updating the hands of the clock until the agent that rezzed the
clock leaves the domain.  So, once the clock is rezzed, log out and
then back in to see it work.
