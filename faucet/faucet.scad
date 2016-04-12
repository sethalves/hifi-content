
difference() {
    minkowski() {
        union() {
            difference() {
                difference() {
                    cylinder(h=0.2, r=2, center = true, $fn=24);
                    cylinder(h=0.4, r=1.8, center = true, $fn=24);
                }
                translate([0, -3, 0]) {
                    cube(6, center = true);
                }
            }

            translate([-1.9, -2, 0]) {
                // rotate([90, 0, 0]) {
                // cylinder(h=4, r=0.1, $fn=12);
                //}
                cube([0.2, 4, 0.2], center = true);
            }
        }
        sphere(r = 0.5);
    }

    translate([4, -3, 0]) {
        cube(6, center = true);
    }
}
