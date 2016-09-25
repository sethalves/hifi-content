//
//
//




hull_length = 28;
hull_width = 16;
hull_thickness = 0.5;
hull_wall_height = 1.0;

hull_half_length = hull_length / 2.0;
hull_half_width = hull_width / 2.0;
hull_half_diff = hull_half_length - hull_half_width;
hold_ceiling = 0 - hull_thickness;
hold_floor = -3.8;

hatch_low_x = 1;
hatch_high_x = 2;
hatch_half_width = 1;

H = hull_length * 2.0;
L = -H;


// these are overridden by the Makefile to create different parts
output_walls = 0;
output_deck_collision_hull_0 = 0;
output_deck_collision_hull_1 = 0;
output_deck_collision_hull_2 = 0;
output_deck_collision_hull_3 = 0;
output_hold_floor_collision_hull = 0;


module inner_hull() {
    intersection() {
        translate([0, 0, - hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
        translate([0, 0, hull_half_diff]) { sphere(hull_half_length - hull_thickness); }
    }
}

module boat_hull() {
    // boat-hull
    difference() {
        // full-hull
        difference() {
            // outer-hull
            intersection() {
                translate([0, 0, -hull_half_diff]) { sphere(hull_half_length); }
                translate([0, 0, hull_half_diff]) { sphere(hull_half_length); }
            }

            inner_hull();
        }

        translate([L, hull_wall_height, L]) { cube([H - L, H - hull_wall_height, H - L], false); }
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
        boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
        translate([L, 0, L]) { cube([H - L, H - 0, H - L], false); }
    }
    intersection() {
        boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
        translate([L, hold_floor, L]) { cube([H - L, hold_ceiling - hold_floor, H - L], false); }
    }
} else {
    boat_hull(hull_length, hull_width, hull_thickness, hull_wall_height);
}
