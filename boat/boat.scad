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
hold_floor = -5;

actual_radius = hull_half_length - 10; // XXX

hatch_low_x = 4;
hatch_high_x = 6;
hatch_half_width = 1.5;

boat_back_squeeze = 0.55;
cabin_offset = 3.0;
cabin_size = 12.0;
cabin_height = 5.3; // how high above the top of the rail?
cabin_door_half_width = 0.7;

mast_forward_offset = -0.5;
mast_height = 40;
mast_radius = 1;
mast_base_height = 0.2;
mast_base_radius = 1.5;
forward_mast_forward_offset = actual_radius - 6;
forward_mast_height = 30;
forward_mast_radius = 0.8;
forward_mast_base_height = 0.2;
forward_mast_base_radius = 1.3;
mast_top_ratio = 0.2;
mast_mid_ratio = 0.4;

hull_fragment_angle = 25;

// H and L are big enough to be outside the boat in any dimension
H = hull_length * 2.0;
L = -H;


// these are overridden by the Makefile to create different parts
output_visual = 0; // everything in a union
output_walls = 0; // main body of boat, but only the parts you can bump into while walking
output_deck_collision_hull_0 = 0;
output_deck_collision_hull_1 = 0;
output_deck_collision_hull_2 = 0;
output_deck_collision_hull_3 = 0;
output_deck_collision_hull_4 = 0;
output_hold_floor_collision_hull = 0;
output_hull = 0; // main body of boat
output_cabin_wall_0 = 0;
output_cabin_wall_1 = 0;
output_mast = 0;
output_mast_base = 0;
output_forward_mast = 0;
output_forward_mast_base = 0;

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
                translate([0, 0, -hull_half_diff]) { sphere(hull_half_length, $fa = hull_fragment_angle); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length, $fa = hull_fragment_angle); }
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
    if (output_hold_floor_collision_hull || output_visual) {
        intersection() {
            place_cuboid(-hull_half_length, hull_half_length,
                         (hold_floor - hull_thickness), hold_floor,
                         -hull_half_width, hull_half_width);
            inner_hull();
        }
    }
    if (output_walls || output_visual) {
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
    if (output_hull || output_visual) {
        boat_walls(hull_length, hull_width, hull_thickness, hull_rail_height);
    }
    if (output_mast || output_visual) {
        translate([mast_forward_offset, 0, 0]) {
            rotate([-90, 0 ,0]) {
                cylinder(mast_height, mast_radius);
            }
        }
        if (output_visual) {
            translate([mast_forward_offset, mast_height * 0.9, 0]) {
                cylinder(h=(mast_height * mast_top_ratio), r=(mast_radius * 0.5), center=true);
            }
            translate([mast_forward_offset, mast_height * 0.5, 0]) {
                cylinder(h=(mast_height * mast_mid_ratio), r=(mast_radius * 0.8), center=true);
            }
        }
    }
    if (output_mast_base || output_visual) {
        translate([mast_forward_offset, 0, 0]) {
            rotate([-90, 0 ,0]) {
                cylinder(mast_base_height, mast_base_radius);
            }
        }
    }
    if (output_forward_mast || output_visual) {
        translate([forward_mast_forward_offset, 0, 0]) {
            rotate([-90, 0 ,0]) {
                cylinder(forward_mast_height, forward_mast_radius);
            }
        }
        if (output_visual) {
            translate([forward_mast_forward_offset, forward_mast_height * 0.9, 0]) {
                cylinder(h=(forward_mast_height * mast_top_ratio), r=(forward_mast_radius * 0.5), center=true);
            }
            translate([forward_mast_forward_offset, forward_mast_height * 0.5, 0]) {
                cylinder(h=(forward_mast_height * mast_mid_ratio), r=(forward_mast_radius * 0.8), center=true);
            }
        }
    }
    if (output_forward_mast_base || output_visual) {
        translate([forward_mast_forward_offset, 0, 0]) {
            rotate([-90, 0 ,0]) {
                cylinder(forward_mast_base_height, forward_mast_base_radius);
            }
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
