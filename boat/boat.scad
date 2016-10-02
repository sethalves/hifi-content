//
//
//

hull_length = 60;
hull_width = 16;
hull_thickness = 0.5;
hull_rail_height = 1.0;

hull_half_length = hull_length / 2.0;
hull_half_width = hull_width / 2.0;
hull_half_diff = hull_half_length - hull_half_width;
hold_ceiling = 0 - hull_thickness;
hold_floor = -3.8;

actual_radius = hull_half_length - 10; // XXX

hatch_low_x = 1;
hatch_high_x = 2;
hatch_half_width = 1;

boat_back_squeeze = 0.55;
cabin_offset = 3.0;
cabin_size = 12.0;
cabin_height = 4;
cabin_door_half_width = 0.7;

// H and L are big enough to be outside the boat in any dimension
H = hull_length * 2.0;
L = -H;


// these are overridden by the Makefile to create different parts
output_walls = 0;
output_deck_collision_hull_0 = 0;
output_deck_collision_hull_1 = 0;
output_deck_collision_hull_2 = 0;
output_deck_collision_hull_3 = 0;
output_deck_collision_hull_4 = 0;
output_hold_floor_collision_hull = 0;

module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}


module inner_hull() {
    difference() {
        hull() {
            intersection() {
                translate([0, 0, - hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
            };
            // fatten the back of the boat
            place_cuboid(-actual_radius + cabin_offset + hull_thickness,
                         -actual_radius + hull_thickness + cabin_offset + hull_thickness,
                         -hull_half_length * boat_back_squeeze, hull_rail_height,
                         (-hull_half_width + hull_thickness) * boat_back_squeeze,
                         (hull_half_width - hull_thickness) * boat_back_squeeze);
        }
        translate([L, hull_rail_height, L]) { cube([H - L, H - hull_rail_height, H - L], false); }
    }
}


module outer_hull() {
    difference() {
        hull() {
            intersection() {
                translate([0, 0, -hull_half_diff]) { sphere(hull_half_length); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length); }
            };
            // fatten the back of the boat
            place_cuboid(-actual_radius + cabin_offset, -actual_radius + hull_thickness + cabin_offset,
                         -hull_half_length * boat_back_squeeze, hull_rail_height,
                         -hull_half_width * boat_back_squeeze, hull_half_width * boat_back_squeeze);
        }
        translate([L, hull_rail_height, L]) { cube([H - L, H - hull_rail_height, H - L], false); }
    }
}


module main_hull() {
    union() {
        difference() {
            outer_hull();
            inner_hull();
        };

        // front wall of cabin
        difference() {
            intersection() {
                place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                             0, hull_rail_height,
                             -hull_half_width, hull_half_width);
                outer_hull();
            }
            translate([-hull_half_length,
                       -1,
                       -cabin_door_half_width]) {
                cube([hull_half_length - -hull_half_length,
                      (hull_rail_height + 1) - -1,
                      cabin_door_half_width - -cabin_door_half_width],
                     false);
            }
        }
    }
}


module cabin_cut() {
    // take a sample of the top of rail so it can be extended to be the cabin
    projection(cut = true) {
        rotate([-90, 0 ,0]) { // cut looks at z = 0
            translate([0, -hull_rail_height + 0.0001, 0]) { // almost the very top
                intersection() {
                    main_hull();
                    translate([-actual_radius,
                               0,
                               -hull_half_width]) {
                        cube([(-actual_radius + cabin_size) - -actual_radius,
                              cabin_height - 0,
                              hull_half_width - -hull_half_width],
                             false);
                    }
                }
            }
        }
    }
}


module cabin() {
    translate([0, cabin_height + hull_rail_height, 0]) {
        rotate([90, 0 ,0]) {
            linear_extrude(height = cabin_height) {
                cabin_cut();
            }
        }
    }
}


module cabin_roof() {
    intersection() {
        hull() {
            cabin();
        }
        place_cuboid(L, H, cabin_height - hull_thickness, cabin_height, L, H);
    }
}


module boat_hull() {
    union() {
        main_hull();
        cabin();
        cabin_roof();
    }
}


module floor_part(low_x, low_z, high_x, high_z) {
    intersection() {
        translate([low_x, -hull_thickness, low_z]) {
            cube([high_x - low_x, 0 - -hull_thickness, high_z - low_z], false);
        }
        inner_hull();
    }
}

// the deck has a hatch in it; each section must be output separately.
//
//     +--------------------------+
//     |      3         |         |
//     |-----------+----+         |
//     |           |    |   2     |
//     |           |    |         |
//     |     0     +----+---------|
// Z   |           |       1      |
// ^   +--------------------------+
// |
// +--> X


if (output_deck_collision_hull_0) {
    floor_part(-hull_half_length, -hull_half_width, hatch_low_x, hatch_half_width);
} else if (output_deck_collision_hull_1) {
    floor_part(hatch_low_x, -hull_half_width, hull_half_length, -hatch_half_width);
} else if (output_deck_collision_hull_2) {
    floor_part(hatch_high_x, -hatch_half_width, hull_half_length, hull_half_width);
} else if (output_deck_collision_hull_3) {
    floor_part(-hull_half_length, hatch_half_width, hatch_high_x, hull_half_width);
} else if (output_deck_collision_hull_4) {
    cabin_roof();
} else if (output_hold_floor_collision_hull) {
    intersection() {
        translate([-hull_half_length, (hold_floor - hull_thickness), -hull_half_width]) {
            cube([hull_half_length - -hull_half_length,
                  hold_floor - (hold_floor - hull_thickness),
                  hull_half_width - -hull_half_width],
                 false);
        }
        inner_hull();
    }
} else if (output_walls) {
    intersection() {
        boat_hull(hull_length, hull_width, hull_thickness, hull_rail_height);
        translate([L, 0, L]) { cube([H - L, H - 0, H - L], false); }
    }
    intersection() {
        boat_hull(hull_length, hull_width, hull_thickness, hull_rail_height);
        translate([L, hold_floor, L]) { cube([H - L, hold_ceiling - hold_floor, H - L], false); }
    }
} else {
    boat_hull(hull_length, hull_width, hull_thickness, hull_rail_height);
}
