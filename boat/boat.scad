//
//
//

hull_length = 60;
hull_width = 16;
hull_thickness = 0.5;
hull_rail_height = 1.3;

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
cabin_height = 5.3; // how high above the top of the rail?
cabin_door_half_width = 0.7;
quarterdeck_entry_half_width = hull_half_width * 0.7;

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
output_hull = 0;
output_cabin_wall_0 = 0;
output_cabin_wall_1 = 0;

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
        place_cuboid(L, H, hull_rail_height, H, L, H);
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
        place_cuboid(L, H, hull_rail_height, H, L, H);
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
            // cut out door to cabin
            place_cuboid(-actual_radius + cabin_size - hull_thickness - 1, -actual_radius + cabin_size + 1,
                         0, (hull_rail_height + cabin_height),
                         -cabin_door_half_width, cabin_door_half_width);
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
                    place_cuboid(-actual_radius, (-actual_radius + cabin_size),
                                 0, cabin_height,
                                 -hull_half_width, hull_half_width);
                }
            }
        }
    }
}


module cabin_wall_template_cut() {
    projection(cut = true) {
        rotate([-90, 0 ,0]) { // cut looks at z = 0
            translate([0, -hull_rail_height + 0.0001, 0]) { // almost the very top
                intersection() {
                    inner_hull();
                    place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                                 L, H,
                                 L, H);
                }
            }
        }
    }
}


module cabin_front_wall_template() {
    // a box that encompasses the wall between the cabin and deck
    hull() {
        translate([0, cabin_height + hull_rail_height, 0]) {
            rotate([90, 0 ,0]) {
                linear_extrude(height = cabin_height + hull_rail_height) {
                    cabin_wall_template_cut();
                }
            }
        }
    }
}


module cabin() {
    difference() {
        translate([0, cabin_height + hull_rail_height, 0]) {
            rotate([90, 0 ,0]) {
                linear_extrude(height = cabin_height) {
                    cabin_cut();
                }
            }
        }

        // remove railing between quarterdeck and main deck
        translate([0, cabin_height, 0]) {
            cabin_front_wall_template();
        }

        // doorway into cabin
        place_cuboid(-actual_radius + cabin_size - hull_thickness - 1, -actual_radius + cabin_size + 1,
                     0, (hull_rail_height + cabin_height),
                     -cabin_door_half_width, cabin_door_half_width);
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


module boat_walls() {
    union() {
        main_hull();
        cabin();
    }
}


module floor_part(low_x, low_z, high_x, high_z) {
    intersection() {
        place_cuboid(low_x, high_x, -hull_thickness, 0, low_z, high_z);
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


scale([1.0, 0.75, 1.0]) {
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
            place_cuboid(-hull_half_length, hull_half_length,
                         (hold_floor - hull_thickness), hold_floor,
                         -hull_half_width, hull_half_width);
            inner_hull();
        }
    } else if (output_walls) {
        // walls that can be walked into, minus the front wall of the cabin
        difference() {
            union() {
                intersection() {
                    boat_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
                    place_cuboid(L, H, 0, H, L, H);
                }
                intersection() {
                    boat_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
                    place_cuboid(L, H, hold_floor, hold_ceiling, L, H);
                }
            }
            cabin_front_wall_template();
        }
    } else if (output_cabin_wall_0) {
        intersection() {
            place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                         0, cabin_height,
                         L, -cabin_door_half_width);
            cabin_front_wall_template();
        }
    } else if (output_cabin_wall_1) {
        intersection() {
            place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                         0, cabin_height,
                         cabin_door_half_width, H);
            cabin_front_wall_template();
        }
    } else if (output_hull) {
        boat_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
    }
}
