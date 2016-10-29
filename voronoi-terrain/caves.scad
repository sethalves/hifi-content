module caves() {
    union() {
        translate([420, 250, 300]) {
            rotate([180, 90, 0]) {
                linear_extrude(height = 100, center = true, convexity = 10, twist = -240) {
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
