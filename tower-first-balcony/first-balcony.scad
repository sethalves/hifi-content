//
//
//

module first_balcony_section(inner_radius = 20,
                             outer_radius = 32,
                             tower_height = 20,
                             thickness = 3,
                             angle_start = 0,
                             angle_sweep = 20,
                             subsection_count = 4)
{
    // p0 and p1 are on the right of the section if the viewer is inside, looking out
    p0x = inner_radius * sin(angle_start);
    p0y = tower_height - thickness;
    p0z = inner_radius * cos(angle_start);

    p1x = outer_radius * sin(angle_start);
    p1y = tower_height - thickness;
    p1z = outer_radius * cos(angle_start);

    // p2 and p3 are on the left of the section
    p2x = outer_radius * sin(angle_start + angle_sweep);
    p2y = tower_height - thickness;
    p2z = outer_radius * cos(angle_start + angle_sweep);

    p3x = inner_radius * sin(angle_start + angle_sweep);
    p3y = tower_height - thickness;
    p3z = inner_radius * cos(angle_start + angle_sweep);

    section_points = [[p0x, p0y, p0z],
                      [p1x, p1y, p1z],
                      [p2x, p2y, p2z],
                      [p3x, p3y, p3z],
                      [p0x, p0y + thickness, p0z],
                      [p1x, p1y + thickness, p1z],
                      [p2x, p2y + thickness, p2z],
                      [p3x, p3y + thickness, p3z],
                      ];

    polyhedron(points=section_points,
               faces=[[3, 2, 1, 0], // bottom
                      [4, 5, 6, 7], // top
                      [1, 2, 6, 5], // far
                      [0, 4, 7, 3], // near
                      [0, 1, 5, 4], // right
                      [7, 6, 2, 3]] // left
               );
}


inner_radius = 20;
outer_radius = 32;
tower_height = 20;
thickness = 3;
angle_start = 0;
subsection_count = 4;
sections = 32;
angle_sweep = 270.0 / sections;

combined = 1; // this can be overridden by the Makefile



if (combined == 1) {
    for(nth=[0:1:(sections-1)]) {
        first_balcony_section(inner_radius = inner_radius,
                              outer_radius = outer_radius,
                              tower_height = tower_height,
                              thickness = thickness,
                              angle_start = angle_start + (nth * angle_sweep),
                              angle_sweep = angle_sweep,
                              subsection_count = 4);
    }
 }
