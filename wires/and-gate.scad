
include <common.scad>


module and_body() {
    intersection() {
        place_cuboid(-grid_size, grid_size * 1.5, -grid_size, grid_size, grid_size * -1.4, grid_size * 1.4);
        translate([-grid_size, 0, 0]) {
            rotate([90, 0, 0]) {
                cylinder(grid_size / 2, r1=grid_size * 1.4, r2=grid_size * 1.4, center=true, $fn=16);
            }
        }
    }
    place_cuboid(grid_size * -2.5, grid_size * -1,
                 -grid_size / 4, grid_size / 4,
                 grid_size * -1.4, grid_size * 1.4);

}

module main() {
    union() {
        difference() {
            union() {
                // body
                and_body();
                // output
                translate([grid_size, 0, 0]) {
                    sphere(r=wire_radius*1.5);
                    rotate([0, -90, 0]) {
                        rotate([0, 0, 180]) {
                            cylinder(grid_size, r1=wire_radius, r2=wire_radius);
                        }
                    }
                }
            }
            // subtract out the center of the body
            translate([-grid_size * 0.15, 0, 0]) {
                scale([0.85, 1.1, 0.85]) {
                    and_body();
                }
            }
        }

        // inputs
        translate([grid_size * -3, 0, grid_size]) {
            sphere(r=wire_radius*1.5);
            rotate([0, 90, 0]) {
                cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
            }
        }
        translate([grid_size * -3, 0, -grid_size]) {
            sphere(r=wire_radius*1.5);
            rotate([0, 90, 0]) {
                cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
            }
        }
    }
}

main();
debug_grid();
