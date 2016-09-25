//
//
//


module boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height) {
    hull_half_length = hull_length / 2.0;
    hull_half_width = hull_width / 2.0;
    hull_half_diff = hull_half_length - hull_half_width;
    H = hull_length * 2.0;
    L = -H;

    // boat-hull
    difference() {
        // full-hull
        difference() {
            // outer-hull
            intersection() {
                translate([0, 0, -hull_half_diff]) { sphere(hull_half_length); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length); }
            }

            // inner-floor
            intersection() {
                // inner-hull-space
                intersection() {
                    translate([0, 0, - hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
                    translate([0, 0, hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
                }
                translate([L, 0, L]) { cube([H - L, H, H - L], false); }
            }
        }

        translate([L, hull_wall_height, L]) { cube([H - L, H - hull_wall_height, H - L], false); }
    }
}


hull_length = 12;
hull_width = 8;
hull_thickness = 0.25;
hull_wall_height = 2;
only_output_rail = 0;
H = hull_length * 2.0;
L = -H;


if (only_output_rail) {
    intersection() {
        boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
        translate([L, 0, L]) { cube([H - L, H - 0, H - L], false); }
    }
} else {
    boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
}
