//
//
//




hull_length = 12;
hull_width = 8;
hull_thickness = 0.25;
hull_wall_height = 2;
H = hull_length * 2.0;
L = -H;

output_rail = 0;
output_deck_collision_hull = 0;
output_hold_floor_collision_hull = 0;


module boat_hull() {
    hull_half_length = hull_length / 2.0;
    hull_half_width = hull_width / 2.0;
    hull_half_diff = hull_half_length - hull_half_width;
    H = hull_length * 2.0;
    L = -H;
    hold_ceiling = 0 - hull_thickness;
    hold_floor = -2.5;

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

                union() {
                    translate([L, 0, L]) { cube([H - L, H, H - L], false); } // main deck
                    translate([L, hold_floor, L]) { cube([H - L, hold_ceiling - hold_floor, H - L], false); } // hold
                    translate([1, -1, -1]) { cube([2 - 1, 1 - -1, 1 - -1], false); } // hatch
                }
            }
        }

        translate([L, hull_wall_height, L]) { cube([H - L, H - hull_wall_height, H - L], false); }
    }
}

if (output_deck_collision_hull) {
    translate([L, hull_thickness, L]) { cube([H - L, 0 - hull_thickness, H - L], false); }
} else if (output_hold_floor_collision_hull) {
    translate([L, L, L]) { cube([H - L, hold_floor - L, H - L], false); }
} else if (output_rail) {
    intersection() {
        boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
        translate([L, 0, L]) { cube([H - L, H - 0, H - L], false); }
    }
} else {
    boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
}
