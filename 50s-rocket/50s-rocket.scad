//
//
//

rocket_vertical_slice_size = 3.0; // meters
rocket_rotational_slice_count = 20; // slices in a full circle -- matches value in 50s-rocket.js

angle_per_slice = 360.0 / rocket_rotational_slice_count;
angle_per_half_slice = angle_per_slice / 2;

rocket_outline = [4.0, // 0 // matches baseRocketRadius in 50s-rocket.js
                  4.6, // 1
                  5.0, // 2
                  5.4, // 3
                  5.6, // 4
                  5.4, // 5
                  5.0, // 6
                  4.2, // 7
                  3.4, // 8
                  2.0, // 9
                  0.2];
rocket_wall_thickness = 0.1; // matches rocketWallThickness in 50s-rocket.js
rocket_thruster_offset = [0, -1.75, -7];
rocket_thruster_height = 2;
rocket_elevator_radius = 2.1;

port_hole_height = 14.4;
port_hole_raidus = 1.8;


module make_rocket_floor_segment(vertical_index, rotational_index) {
    start_angle = angle_per_slice * rotational_index;
    end_angle = angle_per_slice * (rotational_index + 1);
    outer_radius = rocket_outline[vertical_index];

    p03x = 0;
    p03y = 0 - rocket_wall_thickness;
    p03z = 0;

    p12x = 0;
    p12y = 0;
    p12z = 0;

    p4x = outer_radius * sin(start_angle);
    p4y = 0 - rocket_wall_thickness;
    p4z = outer_radius * cos(start_angle);

    p5x = outer_radius * sin(start_angle);
    p5y = 0;
    p5z = outer_radius * cos(start_angle);

    p6x = outer_radius * sin(end_angle);
    p6y = 0;
    p6z = outer_radius * cos(end_angle);

    p7x = outer_radius * sin(end_angle);
    p7y =  0 - rocket_wall_thickness;
    p7z = outer_radius * cos(end_angle);

    section_points = [[p03x, p03y, p03z], // 0
                      [p12x, p12y, p12z], // 1
                      [p4x, p4y, p4z], // 2
                      [p5x, p5y, p5z], // 3
                      [p6x, p6y, p6z], // 4
                      [p7x, p7y, p7z]]; // 5
    polyhedron(points=section_points,
               faces=[[0, 5, 2], // bottom
                      [1, 3, 4], // top
                      [2, 5, 4, 3]]); // outside
}

module make_rocket_floor(vertical_index) {
    for (rotational_index=[0:1:rocket_rotational_slice_count-1]) {
        make_rocket_floor_segment(vertical_index, rotational_index);
    }
}

module make_first_floor() {
    make_rocket_floor(0);
}

module make_second_floor_segment(rotational_index) {
    translate([0, 4 * rocket_vertical_slice_size, 0]) {
        difference() {
            make_rocket_floor_segment(4, rotational_index);
            rotate([90, 0, 0])
                cylinder(h = rocket_wall_thickness*3,
                         r1 = rocket_elevator_radius,
                         r2 = rocket_elevator_radius,
                         center = true, $fs=0.5);
        }
    }
}


module rocket_wall_panel(vertical_index = 0,
                         rotational_index = 0,
                         door = 0,
                         hull = 0) {
    // if hull is one, we pull the edges in a bit.  this is so the door will fit in its doorway without
    // makeing bullet try to resolve interpenetration

    hull_offset = hull * 0.1;
    // if door is 1, center this panel on 0 degrees
    start_angle = angle_per_slice * rotational_index - (door * 0.5 * angle_per_slice) + hull * 2;
    end_angle = angle_per_slice * (rotational_index + 1) - (door * 0.5 * angle_per_slice) - hull * 2;

    // echo(angle_per_slice=angle_per_slice,rotational_index=rotational_index,start_angle=start_angle,end_angle=end_angle);

    low_inner_radius = rocket_outline[vertical_index] - rocket_wall_thickness;
    low_outer_radius = rocket_outline[vertical_index];
    high_inner_radius = rocket_outline[vertical_index + 1] - rocket_wall_thickness;
    high_outer_radius = rocket_outline[vertical_index + 1];


    //                              p2 ----- p6
    //                                /    /
    //                               / p5 / |
    //    inside of rocket       p1  -----  |      outside of rocket
    //                               |   |  | p7
    //                      p3 -->   |   | /
    //                               |   |/
    //                           p0  ----- p4


    p0x = low_inner_radius * sin(start_angle);
    p0y = rocket_vertical_slice_size * vertical_index + hull_offset;
    p0z = low_inner_radius * cos(start_angle);

    p1x = high_inner_radius * sin(start_angle);
    p1y = rocket_vertical_slice_size * (vertical_index + 1) - hull_offset;
    p1z = high_inner_radius * cos(start_angle);

    p2x = high_inner_radius * sin(end_angle);
    p2y = rocket_vertical_slice_size * (vertical_index + 1) - hull_offset;
    p2z = high_inner_radius * cos(end_angle);

    p3x = low_inner_radius * sin(end_angle);
    p3y = rocket_vertical_slice_size * vertical_index + hull_offset;
    p3z = low_inner_radius * cos(end_angle);

    p4x = low_outer_radius * sin(start_angle);
    p4y = rocket_vertical_slice_size * vertical_index + hull_offset;
    p4z = low_outer_radius * cos(start_angle);

    p5x = high_outer_radius * sin(start_angle);
    p5y = rocket_vertical_slice_size * (vertical_index + 1) - hull_offset;
    p5z = high_outer_radius * cos(start_angle);

    p6x = high_outer_radius * sin(end_angle);
    p6y = rocket_vertical_slice_size * (vertical_index + 1) - hull_offset;
    p6z = high_outer_radius * cos(end_angle);

    p7x = low_outer_radius * sin(end_angle);
    p7y = rocket_vertical_slice_size * vertical_index + hull_offset;
    p7z = low_outer_radius * cos(end_angle);

    for (variable = [rocket_outline]) {
        section_points = [[p0x, p0y, p0z],
                          [p1x, p1y, p1z],
                          [p2x, p2y, p2z],
                          [p3x, p3y, p3z],
                          [p4x, p4y, p4z],
                          [p5x, p5y, p5z],
                          [p6x, p6y, p6z],
                          [p7x, p7y, p7z]];
        polyhedron(points=section_points,
                   faces=[[0, 3, 7, 4], // bottom
                          [1, 5, 6, 2], // top
                          [2, 6, 7, 3], // far
                          [0, 4, 5, 1], // near
                          [0, 1, 2, 3], // left
                          [4, 7, 6, 5]]); // right
    }
}

module make_table() {
    table_height = 0.9;
    // table_outer_radius = rocket_outline[1] - rocket_wall_thickness;
    radius_a = rocket_outline[0] - rocket_wall_thickness;
    radius_b = rocket_outline[1] - rocket_wall_thickness;
    table_outer_radius = (radius_b - radius_a) * (table_height / rocket_vertical_slice_size) + radius_a;
    rotate([0, 90, 0])
        translate([0, table_height, 0])
            rotate([0, 90, 0])
            difference() {
                rotate([90, 0, 0])
                    cylinder(h = 0.3, r1 = table_outer_radius, r2 = table_outer_radius, center = true, $fs=0.5);
                translate([-table_outer_radius + 0.8, -40, -40])
                    cube([80, 80, 80]);
            }
}

module make_thruster(angle) {
    rotate([0, angle, 0])
        translate(rocket_thruster_offset)
        rotate([-90, 0, 0])
        cylinder(h = rocket_thruster_height, r1 = 2.5, r2 = 2.0, center = true, $fs=0.5);
}

// these are overridden by the Makefile
combined = 1;
body = 0;
door = 0;
thrusters = 0;
table = 0;


if (combined == 1) {
    vertical_slices = len(rocket_outline) - 1;
    if (body == 1) {
        difference() {
            for (vertical_index=[0:1:vertical_slices]) {
                for (rotational_index=[0:1:rocket_rotational_slice_count-1]) {
                    if (rotational_index >= 1 || vertical_index >= 2) {
                        rocket_wall_panel(vertical_index = vertical_index,
                                          rotational_index = rotational_index,
                                          door = door,
                                          hull = 0);
                    }
                }
            }

            // port-holes
            translate([0, port_hole_height, 0])
                union() {
                rotate([0, angle_per_half_slice, 0])
                        cylinder(h = 20,
                                 r1 = port_hole_raidus,
                                 r2 = port_hole_raidus,
                                 center = false, $fs=0.5);
                rotate([0, 120 + angle_per_half_slice, 0])
                        cylinder(h = 20,
                                 r1 = port_hole_raidus,
                                 r2 = port_hole_raidus,
                                 center = false, $fs=0.5);;
                rotate([0, 240 + angle_per_half_slice, 0])
                        cylinder(h = 20,
                                 r1 = port_hole_raidus,
                                 r2 = port_hole_raidus,
                                 center = false, $fs=0.5);;
            }
        }

        make_first_floor();
        for (rotational_index=[0:1:rocket_rotational_slice_count-1]) {
            make_second_floor_segment(rotational_index);
        }
    } else if (door == 1) {
        rocket_wall_panel(vertical_index = 0,
                          rotational_index = 0,
                          door = door,
                          hull = 0);
        rocket_wall_panel(vertical_index = 1,
                          rotational_index = 0,
                          door = door,
                          hull = 0);
    } else if (thrusters == 1) {
        make_thruster(0);
        make_thruster(120);
        make_thruster(240);
    } else if (table == 1) {
        make_table();
    }
} else {
    if (nth < 200) {
        // do one panel of the rocket
        vertical_slices = len(rocket_outline) - 1;
        rotational_index = floor(nth / vertical_slices);
        vertical_index = nth - (rotational_index * vertical_slices);

        if ((door == 1 && (rotational_index < 1 && vertical_index < 2)) ||
            (door == 0 && (rotational_index >= 1 || vertical_index >= 2))) {
            rocket_wall_panel(vertical_index = vertical_index,
                              rotational_index = rotational_index,
                              door = door,
                              hull = 0); // shrink the door slightly (or not)
        }
    } else if (nth == 200) {
        make_first_floor();
    } else if (nth == 201) {
        make_thruster(0);
    } else if (nth == 202) {
        make_thruster(120);
    } else if (nth == 203) {
        make_thruster(240);
    } else if (nth == 204) {
        make_table();
    } else if (nth < 225) {
        // for (rotational_index=[0:1:rocket_rotational_slice_count-1]) {
            make_second_floor_segment(nth - 205);
            // }
    }
}
