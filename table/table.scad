
// settings -- these should match what's in build-table.js

width = 2.5;
height = 0.95;
depth = 1;
table_surface_height = 0.35;
edge_size = 0.07;
drawer_edge_size = 0.035;
gap = 0.01;
leg_thickness = 0.21;
leg_offset = 0.05;

// derived

table_surface_x = 0;
table_surface_y = (height / 2) - (table_surface_height / 2);
table_surface_z = 0;

drawer_hole_width = (width / 2) - (edge_size * 1.5);
drawer_hole_height = table_surface_height - (edge_size * 2);
drawer_hole_depth = depth - edge_size;

table_surface_hull_top_y = (height / 2) - (edge_size / 2);
table_surface_hull_bottom_y = (height / 2) - edge_size - drawer_hole_height - (edge_size / 2);
table_left_hull_x = (-width / 2) + (edge_size / 2);
table_right_hull_x = (width / 2) - (edge_size / 2);
table_back_hull_z = (-depth / 2) + (edge_size / 2);

left_drawer_center_x = (-edge_size / 2) - (drawer_hole_width / 2);
right_drawer_center_x = (edge_size / 2) + (drawer_hole_width / 2);
drawer_center_y = table_surface_y;
drawer_center_z = (depth / 2) - (drawer_hole_depth / 2);

leg_height = height - table_surface_height;
left_leg_x = (-width / 2) + leg_offset + (leg_thickness / 2);
right_leg_x = (width / 2) - leg_offset - (leg_thickness / 2);
near_leg_z = (depth / 2) - leg_offset - (leg_thickness / 2);
far_leg_z = (-depth / 2) + leg_offset + (leg_thickness / 2);
leg_y = (-height / 2) + (leg_height / 2);

drawer_width = drawer_hole_width - (gap * 2);
drawer_height = drawer_hole_height - (gap * 2);
drawer_depth = depth - edge_size - gap;

drawer_interior_width = drawer_width - (drawer_edge_size * 2);
drawer_interior_height = drawer_height - drawer_edge_size;
drawer_interior_depth = drawer_depth - (drawer_edge_size * 2);

drawer_interior_x = 0;
drawer_interior_y = (drawer_height / 2) - (drawer_interior_height / 2);
drawer_interior_z = 0;

drawer_hull_bottom_y = (-drawer_height / 2) + (drawer_edge_size / 2);
drawer_hull_front_z = (drawer_depth / 2) - (drawer_edge_size / 2);
drawer_hull_back_z = (-drawer_depth / 2) + (drawer_edge_size / 2);
drawer_hull_left_x = (-drawer_width / 2) + (drawer_edge_size / 2);
drawer_hull_right_x = (drawer_width / 2) - (drawer_edge_size / 2);

// control which part is outputted

output_table_visual = 0;
output_table_hull = 0;
output_drawer_visual = 0;
output_drawer_hull = 0;
n = 0;


module drawer_visual() {
    difference() {
        translate([0, 0, 0]) {
            cube([drawer_width, drawer_height, drawer_depth], true);
        };
        translate([drawer_interior_x, drawer_interior_y, drawer_interior_z]) {
            cube([drawer_interior_width, drawer_interior_height, drawer_interior_depth], true);
        };
    }
}

module drawer_hull() {
    if (n == 0) { // bottem
        translate([0, drawer_hull_bottom_y, 0]) {
            cube([drawer_width, drawer_edge_size, drawer_depth], true);
        };
    }
    if (n == 1) { // front
        translate([0, 0, drawer_hull_front_z]) {
            cube([drawer_width, drawer_height, drawer_edge_size], true);
        };
    }
    if (n == 2) { // back
        translate([0, 0, drawer_hull_back_z]) {
            cube([drawer_width, drawer_height, drawer_edge_size], true);
        };
    }
    if (n == 3) { // left
        translate([drawer_hull_left_x, 0, 0]) {
            cube([drawer_edge_size, drawer_height, drawer_depth], true);
        };
    }
    if (n == 4) { // right
        translate([drawer_hull_right_x, 0, 0]) {
            cube([drawer_edge_size, drawer_height, drawer_depth], true);
        };
    }
}

module table_visual() {
    union() {
        difference() {
            translate([table_surface_x, table_surface_y, table_surface_z]) {
                cube([width, table_surface_height, depth], true);
            };
            union() {
                translate([left_drawer_center_x, drawer_center_y, drawer_center_z]) {
                    cube([drawer_hole_width, drawer_hole_height, drawer_hole_depth], true);
                };
                translate([right_drawer_center_x, drawer_center_y, drawer_center_z]) {
                    cube([drawer_hole_width, drawer_hole_height, drawer_hole_depth], true);
                };
            };
        };

        translate([left_leg_x, leg_y, near_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
        translate([left_leg_x, leg_y, far_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
        translate([right_leg_x, leg_y, near_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
        translate([right_leg_x, leg_y, far_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
    };
}

module table_hull() {
    if (n == 0) { // surface top
        translate([table_surface_x, table_surface_hull_top_y, table_surface_z]) {
            cube([width, edge_size, depth], true);
        };
    }
    if (n == 1) { // surface bottom
        translate([table_surface_x, table_surface_hull_bottom_y, table_surface_z]) {
            cube([width, edge_size, depth], true);
        };
    }
    if (n == 2) { // left side
        translate([table_left_hull_x, table_surface_y, 0]) {
            cube([edge_size, table_surface_height, depth], true);
        };
    }
    if (n == 3) { // center
        translate([0, table_surface_y, 0]) {
            cube([edge_size, table_surface_height, depth], true);
        };
    }
    if (n == 4) { // right side
        translate([table_right_hull_x, table_surface_y, 0]) {
            cube([edge_size, table_surface_height, depth], true);
        };
    }
    if (n == 5) { // back
        translate([0, table_surface_y, table_back_hull_z]) {
            cube([width, table_surface_height, edge_size], true);
        };
    }

    if (n == 6) {
        translate([left_leg_x, leg_y, near_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
    }
    if (n == 7) {
        translate([left_leg_x, leg_y, far_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
    }
    if (n == 8) {
        translate([right_leg_x, leg_y, near_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
    }
    if (n == 9) {
        translate([right_leg_x, leg_y, far_leg_z]) {
            cube([leg_thickness, leg_height, leg_thickness], true);
        };
    }
}


if (output_table_visual) {
    table_visual();
}

if (output_table_hull) {
    table_hull();
}

if (output_drawer_visual) {
    drawer_visual();
}

if (output_drawer_hull) {
    drawer_hull();
}
