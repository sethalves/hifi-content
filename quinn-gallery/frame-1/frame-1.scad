
edges = 1; // set by Makefile
canvas = 1; // set by Makefile

if (edges == 1) {
    difference() {
        cube([0.9, 1.06, 0.15], center = true);
        cube([0.8, 0.96, 0.2], center = true);
    }
}
