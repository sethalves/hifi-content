//
//
//

include <caves.scad>

module terrain() {
    include <terrain.scad>
}

module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}


difference() {
    terrain();
    caves();
}
