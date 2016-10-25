//
//
//

module terrain() {
    include <terrain.scad>
}

module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}


xslice = 0;
zslice = 0;
width = 1024;
depth = 1024;
xstep = width / 8;
zstep = depth / 8;


intersection() {
    terrain();
    place_cuboid(xslice * xstep, (xslice + 1) * xstep,
                 -10, 1000,
                 zslice * zstep, (zslice + 1) * zstep);
}
