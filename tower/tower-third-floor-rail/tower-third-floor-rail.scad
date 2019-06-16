
width = 12;
wall_thickness = 1.5;
rail_height = 1.2;
rail_thickness = wall_thickness / 2;
cutout_radius = rail_height * 0.8;
cutout_spacing = cutout_radius * 2 + 0.4;

intersection() {
    difference() {
        translate([-width / 2, 0, -rail_thickness / 2]) {
            cube([width, rail_height, rail_thickness], center = false);
        }
        translate([0, 0, -wall_thickness]) {
            union() {
                translate([-cutout_spacing * 2, 0, 0]) {
                    cylinder(h=wall_thickness * 2, r = cutout_radius, $fn=12);
                }
                translate([-cutout_spacing, 0, 0]) {
                    cylinder(h=wall_thickness * 2, r = cutout_radius, $fn=12);
                }
                translate([0, 0, 0]) {
                    cylinder(h=wall_thickness * 2, r = cutout_radius, $fn=12);
                }
                translate([cutout_spacing, 0, 0]) {
                    cylinder(h=wall_thickness * 2, r = cutout_radius, $fn=12);
                }
                translate([cutout_spacing * 2, 0, 0]) {
                    cylinder(h=wall_thickness * 2, r = cutout_radius, $fn=12);
                }
            }
        }
    }

    rotate([0, 90, 0]) {
        translate([0, 0.18, -width / 2]) {
            cylinder(h=width, r = 1.0, $fn=48);
        }
    }
}
