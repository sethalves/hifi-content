

width = 0.5; // set by Makefile
height = 0.5; // set by Makefile

difference() {
    cube([width + 0.1, height + 0.1, 0.15], center = true);
    cube([width, height, 0.2], center = true);
}
