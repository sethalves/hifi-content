union() {
    rotate([0, 0, 0]) {
        include <crystal-0.scad>
    }
    rotate([30, 0, 0]) {
        include <crystal-1.scad>
    }
    rotate([0, 0, 30]) {
        include <crystal-1.scad>
    }
}
