//
//
//

rocket_vertical_slice_size = 2; // meters
rocket_rotational_slice_count = 20; // slices in a full circle
rocket_outline = [4.0, // 0
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
rocket_wall_thickness = 0.1;


module rocket_wall_panel(vertical_index = 0,
                         rotational_index = 0) {
    start_angle = (360.0 / rocket_rotational_slice_count) * rotational_index;
    end_angle = (360.0 / rocket_rotational_slice_count) * (rotational_index + 1);
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
    p0y = rocket_vertical_slice_size * vertical_index;
    p0z = low_inner_radius * cos(start_angle);

    p1x = high_inner_radius * sin(start_angle);
    p1y = rocket_vertical_slice_size * (vertical_index + 1);
    p1z = high_inner_radius * cos(start_angle);

    p2x = high_inner_radius * sin(end_angle);
    p2y = rocket_vertical_slice_size * (vertical_index + 1);
    p2z = high_inner_radius * cos(end_angle);

    p3x = low_inner_radius * sin(end_angle);
    p3y = rocket_vertical_slice_size * vertical_index;
    p3z = low_inner_radius * cos(end_angle);

    p4x = low_outer_radius * sin(start_angle);
    p4y = rocket_vertical_slice_size * vertical_index;
    p4z = low_outer_radius * cos(start_angle);

    p5x = high_outer_radius * sin(start_angle);
    p5y = rocket_vertical_slice_size * (vertical_index + 1);
    p5z = high_outer_radius * cos(start_angle);

    p6x = high_outer_radius * sin(end_angle);
    p6y = rocket_vertical_slice_size * (vertical_index + 1);
    p6z = high_outer_radius * cos(end_angle);

    p7x = low_outer_radius * sin(end_angle);
    p7y = rocket_vertical_slice_size * vertical_index;
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



combined = 1; // this can be overridden by the Makefile
door = 0;

if (combined == 1) {
    vertical_slices = len(rocket_outline) - 1;
    for (vertical_index=[0:1:vertical_slices]) {
        for (rotational_index=[0:1:rocket_rotational_slice_count-1]) {
            if ((door == 1 && (rotational_index < 1 && vertical_index < 2)) ||
                (door == 0 && (rotational_index >= 1 || vertical_index >= 2))) {
                echo(vertical_index=vertical_index,rotational_index=rotational_index);
                rocket_wall_panel(vertical_index = vertical_index,
                                  rotational_index = rotational_index);
            }
        }
    }
} else {
    // do one panel of the rocket
    vertical_slices = len(rocket_outline) - 1; // 10
    rotational_index = floor(nth / vertical_slices);
    vertical_index = nth - (rotational_index * vertical_slices);

    if ((door == 1 && (rotational_index < 1 && vertical_index < 2)) ||
        (door == 0 && (rotational_index >= 1 || vertical_index >= 2))) {
        rocket_wall_panel(vertical_index = vertical_index,
                          rotational_index = rotational_index);
    }
}
