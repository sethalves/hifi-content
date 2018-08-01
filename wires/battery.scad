
include <common.scad>

module main() {
    union() {
        place_cuboid(-grid_size / 2, grid_size / 2, -grid_size / 2, grid_size / 2, -grid_size / 2, grid_size / 2);
        translate([grid_size, 0, 0]) {
            sphere(r=wire_radius*1.5);
            rotate([0, -90, 0]) {
                rotate([0, 0, 180]) {
                    cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
                }
            }
        }
    }
}

main();
