rotate([90, 0, 0]) {
    difference() {
        cylinder(h = 0.15, r1 = 10, r2 = 10, center = true, $fn=16);
        cylinder(h = 0.2, r1 = 8, r2 = 8, center = true, $fn=16);
    }
}
