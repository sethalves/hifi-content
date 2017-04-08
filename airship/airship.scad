//
//
//

hull_length = 60;
hull_width = 16;
hull_thickness = 0.5;
hull_rail_height = 1.3;
balloon_offset = 35;
balloon_radius = 10;

hull_half_length = hull_length / 2.0;
hull_half_width = hull_width / 2.0;
hull_half_diff = hull_half_length - hull_half_width;
hold_ceiling = 0 - hull_thickness;
hold_floor = -6;

actual_radius = hull_half_length - 10; // XXX

hatch_low_x = 4;
hatch_high_x = 6;
hatch_half_width = 1.5;

airship_back_squeeze = 0.55;
cabin_offset = 3.0;
cabin_size = 12.0;
cabin_height = 5.3; // how high above the top of the rail?
cabin_door_half_width = 0.7;
cabin_door_height = 4.5;

cabin_window_elevation = 1;
cabin_window_height = 2;
cabin_window_half_width = 1.7;

stair_count = 8;
stair_base_size = 5; // back to front
stair_offset_from_door = 0.5; // how far from cabin door

hull_fragment_angle = 25;


// H and L are big enough to be outside the airship in any dimension
H = hull_length * 2.0;
L = -H;


// these are overridden by the Makefile to create different parts
output_visual = 0; // everything in a union
output_walls = 0; // main body of airship, but only the parts you can bump into while walking
output_deck_collision_hull_0 = 0;
output_deck_collision_hull_1 = 0;
output_deck_collision_hull_2 = 0;
output_deck_collision_hull_3 = 0;
output_deck_collision_hull_4 = 0;
output_hold_floor = 0;
output_hold_floor_collision_hull = 0;
output_hull = 0; // main body of airship
output_cabin_wall_0 = 0;
output_cabin_wall_1 = 0;
output_door_frame = 0;
output_stair_0 = 0;
output_stair_1 = 0;
output_stair_collision_shape_0 = 0;
output_stair_collision_shape_1 = 0;
output_spars = 0;
output_forward_spars = 0;
output_balloon = 0;

module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}


module inner_hull() {
    difference() {
        hull() {
            intersection() {
                translate([0, 0, - hull_half_diff]) { sphere(hull_half_length - hull_thickness, $fa = hull_fragment_angle); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length - hull_thickness, $fa = hull_fragment_angle); }
            };
            // fatten the back of the airship
            place_cuboid(-actual_radius + cabin_offset + hull_thickness,
                         -actual_radius + hull_thickness + cabin_offset + hull_thickness,
                         -hull_half_length * airship_back_squeeze, hull_rail_height,
                         (-hull_half_width + hull_thickness) * airship_back_squeeze,
                         (hull_half_width - hull_thickness) * airship_back_squeeze);
        }
        place_cuboid(L, H, hull_rail_height, H, L, H);
    }
}


module outer_hull() {
    difference() {
        hull() {
            intersection() {
                translate([0, 0, -hull_half_diff]) { sphere(hull_half_length, $fa = hull_fragment_angle); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length, $fa = hull_fragment_angle); }
            };
            // fatten the back of the airship
            place_cuboid(-actual_radius + cabin_offset, -actual_radius + hull_thickness + cabin_offset,
                         -hull_half_length * airship_back_squeeze, hull_rail_height,
                         -hull_half_width * airship_back_squeeze, hull_half_width * airship_back_squeeze);
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
    }
}


module cabin_cut() {
    // take a sample of the top of rail so it can be extended to be the cabin
    projection(cut = true) {
        rotate([-90, 0 ,0]) { // cut looks at z = 0
            translate([0, -hull_rail_height + 0.000001, 0]) { // almost the very top
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


module main_deck_interior_cut() {
    projection(cut = true) {
        rotate([-90, 0 ,0]) { // cut looks at z = 0
            translate([0, -hull_rail_height + 0.0001, 0]) { // almost the very top
                inner_hull();
            }
        }
    }
}


module main_deck_interior_template() {
    // a box that encompasses the wall between the cabin and deck
    hull() {
        translate([0, cabin_height + hull_rail_height, 0]) {
            rotate([90, 0 ,0]) {
                linear_extrude(height = cabin_height + hull_rail_height) {
                    main_deck_interior_cut();
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
    }
}


module cabin_roof() {
    intersection() {
        main_deck_interior_template();
        hull() {
            cabin();
        }
        place_cuboid(L, H, // -actual_radius + cabin_size - hull_thickness,
                     cabin_height - hull_thickness, cabin_height, L, H);
    }
}


module airship_walls() {
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


module main() {
    if (output_deck_collision_hull_0 || output_visual) {
        floor_part(-hull_half_length, -hull_half_width, hatch_low_x, hatch_half_width);
    }
    if (output_deck_collision_hull_1 || output_visual) {
        floor_part(hatch_low_x, -hull_half_width, hull_half_length, -hatch_half_width);
    }
    if (output_deck_collision_hull_2 || output_visual) {
        floor_part(hatch_high_x, -hatch_half_width, hull_half_length, hull_half_width);
    }
    if (output_deck_collision_hull_3 || output_visual) {
        floor_part(-hull_half_length, hatch_half_width, hatch_high_x, hull_half_width);
    }
    if (output_deck_collision_hull_4 || output_visual) {
        cabin_roof();
    }
    if (output_hold_floor || output_visual) {
        intersection() {
            place_cuboid(-hull_half_length, hull_half_length,
                         (hold_floor - hull_thickness), hold_floor,
                         -hull_half_width, hull_half_width);
            inner_hull();
        }
    }
    if (output_hold_floor_collision_hull) {
        intersection() {
            place_cuboid(-hull_half_length, hull_half_length,
                         (hold_floor - hull_thickness), hold_floor,
                         -hull_half_width, hull_half_width);
            outer_hull();
        }
    }
    if (output_walls || output_visual) {
        // walls that can be walked into, minus the front wall of the cabin
        difference() {
            union() {
                intersection() {
                    airship_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
                    place_cuboid(L, H, 0, H, L, H);
                }
                intersection() {
                    airship_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
                    place_cuboid(L, H, hold_floor, hold_ceiling, L, H);
                }
            }
            main_deck_interior_template();
        }
    }
    if (output_cabin_wall_0 || output_visual) {
        intersection() {
            place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                         0, cabin_height - hull_thickness,
                         L, -cabin_door_half_width);
            main_deck_interior_template();
        }
    }
    if (output_cabin_wall_1 || output_visual) {
        intersection() {
            place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                         0, cabin_height - hull_thickness,
                         cabin_door_half_width, H);
            main_deck_interior_template();
        }
    }
    if (output_door_frame || output_visual) {
        place_cuboid((-actual_radius + cabin_size - hull_thickness), (-actual_radius + cabin_size),
                     cabin_door_height - hull_thickness, cabin_height - hull_thickness,
                     -cabin_door_half_width, cabin_door_half_width);
    }
    if (output_hull || output_visual) {
        difference() {
            airship_walls(hull_length, hull_width, hull_thickness, hull_rail_height);

            translate([-actual_radius + (cabin_size / 2), 0, 0]) {
                place_cuboid(-hull_thickness * 20, -hull_thickness * 2,
                             cabin_window_elevation, cabin_window_elevation + cabin_window_height,
                             -cabin_window_half_width, cabin_window_half_width);
                rotate([0, 60, 0]) {
                    place_cuboid(-hull_thickness * 20, -hull_thickness * 2,
                                 cabin_window_elevation, cabin_window_elevation + cabin_window_height,
                                 -cabin_window_half_width, cabin_window_half_width);
                }
                rotate([0, -60, 0]) {
                    place_cuboid(-hull_thickness * 20, -hull_thickness * 2,
                                 cabin_window_elevation, cabin_window_elevation + cabin_window_height,
                                 -cabin_window_half_width, cabin_window_half_width);
                }
            }
        }
    }
    if (output_stair_0 || output_visual) {
        intersection() {
            union() {
                for (i = [0 : stair_count]) {
                    place_cuboid((-actual_radius + cabin_size),
                                 (-actual_radius + cabin_size) + ((stair_base_size / stair_count) * (stair_count - i)),
                                 0, (cabin_height / stair_count) * (i + 1),
                                 cabin_door_half_width + stair_offset_from_door, H);
                }
            }
            main_deck_interior_template();
        }
    }
    if (output_stair_1 || output_visual) {
        intersection() {
            union() {
                for (i = [0 : stair_count]) {
                    place_cuboid((-actual_radius + cabin_size),
                                 (-actual_radius + cabin_size) + ((stair_base_size / stair_count) * (stair_count - i)),
                                 0, (cabin_height / stair_count) * (i + 1),
                                 L, -cabin_door_half_width - stair_offset_from_door);
                }
            }
            main_deck_interior_template();
        }
    }
    if (output_balloon || output_visual) {
        translate([0, balloon_offset, 0]) {
            scale([4, 2, 2.2]) {
                sphere(balloon_radius, $fn=10);
            }
        }
    }
    if (output_stair_collision_shape_0) {
        intersection() {
            union() {
                // ramp
                translate([(-actual_radius + cabin_size + stair_base_size),
                           -(cabin_height / stair_count), 0]) {
                    rotate([0, 0, -atan(cabin_height / stair_base_size)]) {
                        place_cuboid(-sqrt((cabin_height * cabin_height) + (stair_base_size * stair_base_size)), 0,
                                     0, cabin_height / stair_count,
                                     cabin_door_half_width + stair_offset_from_door, H);
                    }
                }
                // and part of the top stair to fill the gap
                place_cuboid((-actual_radius + cabin_size),
                             (-actual_radius + cabin_size) + ((stair_base_size / stair_count) * 0.7),
                             0, cabin_height,
                             cabin_door_half_width + stair_offset_from_door, H);

            }
            main_deck_interior_template();
        }
    }
    if (output_stair_collision_shape_1) {
        intersection() {
            union() {
                // ramp
                translate([(-actual_radius + cabin_size + stair_base_size),
                           -(cabin_height / stair_count), 0]) {
                    rotate([0, 0, -atan(cabin_height / stair_base_size)]) {
                        place_cuboid(-sqrt((cabin_height * cabin_height) + (stair_base_size * stair_base_size)), 0,
                                     0, cabin_height / stair_count,
                                     L, -cabin_door_half_width - stair_offset_from_door);
                    }
                }
                // and part of the top stair to fill the gap
                place_cuboid((-actual_radius + cabin_size),
                             (-actual_radius + cabin_size) + ((stair_base_size / stair_count) * 0.7),
                             0, cabin_height,
                             L, -cabin_door_half_width - stair_offset_from_door);
            }
            main_deck_interior_template();
        }
    }
}

scale([1.0, 0.75, 1.0]) {
    if (output_visual) {
        union() {
            main();
        }
    } else {
        main();
    }
}
