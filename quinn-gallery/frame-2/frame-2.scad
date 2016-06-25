
edges = 1; // set by Makefile
canvas = 1; // set by Makefile

if (edges == 1) {
    difference() {
        cube([0.55, 0.7, 0.15], center = true);
        cube([0.45, 0.6, 0.2], center = true);
    }
}
