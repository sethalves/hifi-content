
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
translate([0, 0, 0.02]) {
    cylinder(h = 0.1,
             r1 = 0.018,
             r2 = 0.018,
             center = false, $fn=12);
}

// sphere on top of handle
sphere(0.03, $fn=12);

// smaller barrel extrusion
cylinder(h = 0.15,
         r1 = 0.01,
         r2 = 0.01,
         center = false, $fn=12);

// nose cone
difference () {
    translate([0, 0, 0.15]) {
        cylinder(h = 0.05,
                 r1 = 0.01,
                 r2 = 0.045,
                 center = false, $fn=12);
    }
    translate([0, 0, 0.155]) {
        cylinder(h = 0.05,
                 r1 = 0.01,
                 r2 = 0.045,
                 center = false, $fn=12);
    }
}

// large ring
translate([0, 0, 0.05]) {
    rotate_extrude($fn=15) {
        translate([0.05, 0, 0]) {
            circle(r = 0.01, $fn=6);
        }
    }
}

// smaller ring
translate([0, 0, 0.09]) {
    rotate_extrude($fn=15) {
        translate([0.03, 0, 0]) {
            circle(r = 0.01, $fn=6);
        }
    }
}

// small shaft inside nose code
translate([0, 0, 0.03]) {
    cylinder(h = 0.15,
             r1 = 0.005,
             r2 = 0.005,
             center = false, $fn=12);
}

// sphere at end of small shaft
translate([0, 0, 0.19]) {
    sphere(0.0125, $fn=12);
}

// back sights shaft
rotate([-90, 0, 0]) {
    cylinder(h = 0.056,
             r1 = 0.003,
             r2 = 0.003,
             center = false, $fn=12);
}

// back sights
translate([0, 0.07, 0]) {
    rotate_extrude($fn=15) {
        translate([0.015, 0, 0]) {
            circle(r = 0.003, $fn=6);
        }
    }
}

// forward sights shaft
translate([0, 0.0, 0.11]) {
    rotate([-90, 0, 0]) {
        cylinder(h = 0.056,
                 r1 = 0.003,
                 r2 = 0.003,
                 center = false, $fn=12);
    }
}

// forward sights
translate([0, 0.07, 0.11]) {
    rotate_extrude($fn=15) {
        translate([0.015, 0, 0]) {
            circle(r = 0.003, $fn=6);
        }
    }
}

// heat-sinks
translate([0, 0.024, -0.02]) {
    rotate([-25, 0, 0]) {
        cylinder(h = 0.002,
                 r1 = 0.03,
                 r2 = 0.03,
                 center = false, $fn=12);
    }
}
translate([0, 0.015, -0.02]) {
    rotate([-45, 0, 0]) {
        cylinder(h = 0.002,
                 r1 = 0.03,
                 r2 = 0.03,
                 center = false, $fn=12);
    }
}
translate([0, 0.009, -0.02]) {
    rotate([-65, 0, 0]) {
        cylinder(h = 0.002,
                 r1 = 0.03,
                 r2 = 0.03,
                 center = false, $fn=12);
    }
}
