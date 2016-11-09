
difference() {
    rotate([-90, 0, 0]) {
        cylinder(h=1, r=1.2, center=false, $fn=12);
    }
    translate([0, -2, 0]) {
        rotate([-90, 0, 0]) {
            cylinder(h=4, r=1.0, center=false, $fn=12);
        }
    }
}
