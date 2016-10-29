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


difference() {
    terrain();

    union() {
        translate([420, 240, 300]) {
            rotate([0, 90, 0]) {
                linear_extrude(height = 300, center = true, convexity = 10, twist = -500) {
                    translate([40, 0, 0]) {
                        circle(r = 30, $fn = 24);
                    }
                }
            }
        }

        translate([435, 129, 454]) {
            rotate([30, 20, 0]) {
                linear_extrude(height = 300, center = true, convexity = 10, twist = -500) {
                    translate([40, 0, 0]) {
                        circle(r = 30, $fn = 24);
                    }
                }
            }
        }
    }

    translate([512 - 297.98831176757812, 0, 512 - 164.12950134277344]) {
        place_cuboid(0, 100, -400, 400, 100, 200);
    }

}
