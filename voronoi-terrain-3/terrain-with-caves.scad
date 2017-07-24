
difference() {
    include <terrain.scad>
    translate([-298, 80.132698059082031, -164.12950134277344]) { // position
        translate([2250, 80, 2250]) { // registration point
            include <terrain-negative.scad>
        }
    }
}
