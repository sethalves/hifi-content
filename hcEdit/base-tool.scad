
// handle
difference() {
    union() {
        translate([0, 0.01, 0]) {
            rotate([110, 0, 0]) {
                cylinder(h = 0.15,
                         r1 = 0.02,
                         r2 = 0.02,
                         center = false, $fn=12);
            }
        }
        // edge at bottom of handle
        translate([0, -0.09, -0.0375]) {
            rotate([90, 0, 0]) {
                cylinder(h = 0.1,
                         r1 = 0.025,
                         r2 = 0.025,
                         center = false, $fn=12);
            }
        }
    }


    union() {
        translate([-0.5, -1.1, -0.5]) {
            cube([1, 1, 1]);
        }
        // cutout curve of handle
        translate([0, -0.07, 0.038]) {
            rotate([0, 90, 0]) {
                cylinder(h = 0.1,
                         r1 = 0.06,
                         r2 = 0.06,
                         center = true, $fn=12);
            }
        }
    }
}

// main barrel
translate([0, 0, -0.02]) {
    cylinder(h = 0.15,
             r1 = 0.018,
             r2 = 0.018,
             center = false, $fn=12);
}
