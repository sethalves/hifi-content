
module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}


difference() {
    include <terrain.scad>
    translate([-297.98831176757812, 80.132698059082031, -164.12950134277344]) { // position
        translate([512, 161.25750732421875, 512]) { // registration point
            union() {
                include <terrain-caves.scad>
                place_cuboid(0, 50, -1000, 1000, 100, 150);
            }
        }
    }
}
