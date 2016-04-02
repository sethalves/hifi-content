//
//
//

inner_radius = 24;
outer_radius = 36;
tower_height = 20;
thickness = 3;

walkway_length = 8;

landing_pad_slice_count = 18; // this indirectly controls the size of the pad
landing_pad_slice_sweep = 360 / landing_pad_slice_count;

width = outer_radius - inner_radius;
landing_pad_radius = (width / 2) / sin(landing_pad_slice_sweep / 2);
landing_pad_edge_radius = cos(landing_pad_slice_sweep / 2) * landing_pad_radius;

module make_landing_pad() {
    union() {
        for (rotational_index=[0:1:landing_pad_slice_count-1]) {
            start_angle = (360.0 / landing_pad_slice_count) * rotational_index;
            end_angle = (360.0 / landing_pad_slice_count) * (rotational_index + 1);

            p03x = 0;
            p03y = 0 - thickness;
            p03z = 0;

            p12x = 0;
            p12y = 0;
            p12z = 0;

            p4x = landing_pad_radius * sin(start_angle);
            p4y = 0 - thickness;
            p4z = landing_pad_radius * cos(start_angle);

            p5x = landing_pad_radius * sin(start_angle);
            p5y = 0;
            p5z = landing_pad_radius * cos(start_angle);

            p6x = landing_pad_radius * sin(end_angle);
            p6y = 0;
            p6z = landing_pad_radius * cos(end_angle);

            p7x = landing_pad_radius * sin(end_angle);
            p7y =  0 - thickness;
            p7z = landing_pad_radius * cos(end_angle);

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

///////////////////////////////////////////////////////////////////////////////////


nth = 0;  // this is overridden by Makefile


if (nth == 0 || nth == 1) {
    translate([-width / 2, 0, -width / 2])
        cube([width, tower_height, width], center = false);
}


if (nth == 0 || nth == 2) {
    translate([width / 2, tower_height - thickness, -width / 2])
        cube([walkway_length, thickness, width], center = false);
}


if (nth == 0 || nth == 3) {
    translate([landing_pad_edge_radius + width / 2 + walkway_length, tower_height, 0])
        rotate([0, 90 + landing_pad_slice_sweep / 2, 0])
            make_landing_pad();
}
