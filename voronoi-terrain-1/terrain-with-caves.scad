
difference() {
    include <terrain.scad>
    translate([-297.98831176757812, 80.132698059082031, -164.12950134277344]) { // position
        translate([512, 161.25750732421875, 512]) { // registration point
            include <terrain-negative.scad>
        }
    }
}
