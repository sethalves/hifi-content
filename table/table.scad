
// settings

width = 2.5;
height = 0.95;
depth = 1;
table_surface_height = 0.35;
drawer_edging_size = 0.07;
gap = 0.01;
leg_thickness = 0.25;
leg_offset = 0.05;

// derived

table_surface_center_x = 0;
table_surface_center_y = (height / 2) - (table_surface_height / 2);
table_surface_center_z = 0;

drawer_hole_width = (width / 2) - (drawer_edging_size * 1.5);
drawer_hole_height = table_surface_height - (drawer_edging_size * 2);
drawer_hole_depth = depth - drawer_edging_size;

left_drawer_center_x = (-drawer_edging_size / 2) - (drawer_hole_width / 2);
right_drawer_center_x = (drawer_edging_size / 2) + (drawer_hole_width / 2);
drawer_center_y = table_surface_center_y;
drawer_center_z = (depth / 2) - (drawer_hole_depth / 2);

leg_height = height - table_surface_height;
left_leg_x = (-width / 2) + leg_offset + (leg_thickness / 2);
right_leg_x = (width / 2) - leg_offset - (leg_thickness / 2);
near_leg_z = (depth / 2) - leg_offset - (leg_thickness / 2);
far_leg_z = (-depth / 2) + leg_offset + (leg_thickness / 2);
leg_y = (-height / 2) + (leg_height / 2);

// control which part is outputted

output_table_visual = 0;

module table_visual() {
    difference() {
        translate([table_surface_center_x, table_surface_center_y, table_surface_center_z]) {
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

}


if (output_table_visual) {
    table_visual();
}
