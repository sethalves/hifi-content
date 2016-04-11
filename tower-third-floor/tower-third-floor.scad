//
//
//

inner_radius = 24;
outer_radius = 36;
tower_height = 20;
thickness = 3;

walkway_length = 8;

third_floor_slice_count = 8; // this indirectly controls the size of the pad
third_floor_slice_sweep = 360 / third_floor_slice_count;

width = outer_radius - inner_radius;
third_floor_radius = (width / 2) / sin(third_floor_slice_sweep / 2);
third_floor_edge_radius = cos(third_floor_slice_sweep / 2) * third_floor_radius;

wall_thickness = 1.5;
wall_height = 10;


module make_third_floor() {
    union() {
        for (rotational_index=[0:1:third_floor_slice_count-1]) {
            start_angle = (360.0 / third_floor_slice_count) * rotational_index;
            end_angle = (360.0 / third_floor_slice_count) * (rotational_index + 1);

            p03x = 0;
            p03y = 0 - thickness;
            p03z = 0;

            p12x = 0;
            p12y = 0;
            p12z = 0;

            p4x = third_floor_radius * sin(start_angle);
            p4y = 0 - thickness;
            p4z = third_floor_radius * cos(start_angle);

            p5x = third_floor_radius * sin(start_angle);
            p5y = 0;
            p5z = third_floor_radius * cos(start_angle);

            p6x = third_floor_radius * sin(end_angle);
            p6y = 0;
            p6z = third_floor_radius * cos(end_angle);

            p7x = third_floor_radius * sin(end_angle);
            p7y =  0 - thickness;
            p7z = third_floor_radius * cos(end_angle);

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
    }
}

module make_wall(rotational_index, window) {
    start_angle = (360.0 / third_floor_slice_count) * rotational_index;
    end_angle = (360.0 / third_floor_slice_count) * (rotational_index + 1);
    mid_angle = (start_angle + end_angle) / 2;

    if (rotational_index == 1 ||
        rotational_index == 3 ||
        rotational_index == 4 ||
        rotational_index == 5 ||
        rotational_index == 7) {
        difference() {
            union() {
                p5x = third_floor_radius * sin(start_angle);
                p5y = 0;
                p5z = third_floor_radius * cos(start_angle);

                p6x = third_floor_radius * sin(end_angle);
                p6y = 0;
                p6z = third_floor_radius * cos(end_angle);

                p8x = (third_floor_radius - wall_thickness) * sin(start_angle);
                p8y = 0;
                p8z = (third_floor_radius - wall_thickness) * cos(start_angle);

                p9x = (third_floor_radius - wall_thickness) * sin(end_angle);
                p9y = 0;
                p9z = (third_floor_radius - wall_thickness) * cos(end_angle);

                p10x = p6x;
                p10y = p6y + wall_height;
                p10z = p6z;

                p11x = p8x;
                p11y = p8y + wall_height;
                p11z = p8z;

                p12x = p9x;
                p12y = p9y + wall_height;
                p12z = p9z;

                p13x = p5x;
                p13y = p5y + wall_height;
                p13z = p5z;

                wall_points = [[p5x, p5y, p5z], // 0
                               [p6x, p6y, p6z], // 1
                               [p8x, p8y, p8z], // 2
                               [p9x, p9y, p9z], // 3
                               [p10x, p10y, p10z], // 4
                               [p11x, p11y, p11z], // 5
                               [p12x, p12y, p12z], // 6
                               [p13x, p13y, p13z]]; // 7

                polyhedron(points=wall_points,
                           faces=[[2, 3, 1, 0], // bottom
                                  [4, 6, 5, 7], // top
                                  [0, 1, 4, 7], // outside
                                  [2, 5, 6, 3], // inside
                                  [6, 4, 1, 3], // left
                                  [2, 0, 7, 5] // right
                               ]);
            }

            union() {
                if (rotational_index == 4 && window) {
                    rotate([0, mid_angle, 0]) {
                        translate([0, wall_height / 2, third_floor_radius - 1]) {
                            scale([1.5, 0.8, 1]) {
                                cylinder(h = wall_thickness*5, r1 = wall_height/2, r2 = wall_height/2, center = true, $fn=16);
                            }
                        }
                    }
                }
            }
        }
    }
}


nth = 0;  // this is overridden by Makefile

if (nth == 0 || nth == 1) {
    make_third_floor();
}

if (nth == 0) {
    union() {
        for (rotational_index=[0:1:third_floor_slice_count-1]) {
            make_wall(rotational_index, true);
        }
    }
} else if (nth >= 2 && nth <= 9) {
    make_wall(nth - 2, false);
}

if (nth == 0 || nth == 10) {
    translate([0, wall_height + thickness, 0]) {
        make_third_floor();
    }
}
