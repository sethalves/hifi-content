

width = 0.6; // set by Makefile
height = 0.6; // set by Makefile

difference() {
    difference() {
        cube([width + 0.1, height + 0.1, 0.15], center = true);
        cube([width, height, 0.2], center = true);
    }
    translate([0, 0, 0.135]) {
        scale([width, height, 1]) {
            rotate([0, 180, 45]) {
                cylinder(0.25,1,0,$fn=4);
            }
        }
    }
}
