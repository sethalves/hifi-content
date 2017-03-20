union() {
    translate([310, -75, 170]) {
        include <terrain-caves.scad>
    }
    translate([0, -8, 100]) {
        cube([100 - 0, 100 - -8, 200 - 100], false);
    };
    translate([60, -30, 103]) {
        cube([100 - 60, -12 - -30, 180 - 103], false);
    };

    translate([222, -50, 68]) {
        difference() {
            sphere(30);
            translate([-50, -50, -50]) {
                cube([50 - -50, 4 - -50, 50 - -50], false);
            };

        }
    }

    // well
    translate([90, 0, 77]) {
        rotate([90, 0, 0]) {
            cylinder(h=16, r=1.2, center=true, $fn=12);
        }
    }

    // origin
    translate([0, 0, 0]) {
        sphere(3);
    }
}
