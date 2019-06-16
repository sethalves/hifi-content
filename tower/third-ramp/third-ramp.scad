//
//
//

module spiral_ramp_section(inner_radius = 10,
                           outer_radius = 20,
                           tower_height = 20,
                           angle_start = 0,
                           angle_sweep = 20,
                           y_start = 0,
                           y_rise = 2,
                           thickness = 1,
                           part = 0)
{
    // p0 and p1 are on the right of the ramp section
    p0x = inner_radius * sin(angle_start);
    p0y = y_start;
    p0z = inner_radius * cos(angle_start);

    p1x = outer_radius * sin(angle_start);
    p1y = y_start;
    p1z = outer_radius * cos(angle_start);

    // p2 and p3 are on the left of the ramp section
    p2x = outer_radius * sin(angle_start + angle_sweep);
    p2y = y_start + y_rise;
    p2z = outer_radius * cos(angle_start + angle_sweep);

    p3x = inner_radius * sin(angle_start + angle_sweep);
    p3y = y_start + y_rise;
    p3z = inner_radius * cos(angle_start + angle_sweep);

    section_points = [[p0x, p0y, p0z], // 0
                      [p1x, p1y, p1z], // 1
                      [p2x, p2y, p2z], // 2
                      [p3x, p3y, p3z], // 3
                      [p0x, p0y + thickness, p0z], // 4
                      [p1x, p1y + thickness, p1z], // 5
                      [p2x, p2y + thickness, p2z], // 6
                      [p3x, p3y + thickness, p3z]  // 7
                      ];

    if (part == 0) {
        polyhedron(points=section_points,
                   faces=[[2, 1, 0], // bottom
                          [4, 5, 6], // top
                          [1, 2, 6, 5], // far
                          [4, 6, 2, 0], // center
                          [0, 1, 5, 4]] // right
                   );
    } else if (part == 1) {
        polyhedron(points=section_points,
                   faces=[[3, 2, 0], // bottom
                          [4, 6, 7], // top
                          [0, 4, 7, 3], // near
                          [0, 2, 6, 4], // center
                          [2, 3, 7, 6]] // left
                   );
    }
}



// these values are from tower-third-floor.scad
width = 12;
third_floor_slice_count = 8; // this indirectly controls the size of the pad
third_floor_slice_sweep = 360 / third_floor_slice_count;
third_floor_radius = (width / 2) / sin(third_floor_slice_sweep / 2);
third_floor_edge_radius = cos(third_floor_slice_sweep / 2) * third_floor_radius;


inner_radius = third_floor_edge_radius - (third_floor_radius * cos(third_floor_slice_sweep * 1.5));
outer_radius = inner_radius + width;

tower_height = 13;
thickness = 3;
angle_start = 0;
subsection_count = 4;
sections = 6;
total_angle_sweep = 180.0;
angle_sweep = total_angle_sweep / sections;
y_rise = tower_height / sections;

combined = 1; // this can be overridden by the Makefile

if (combined == 1) {
    difference() {
        for(nth=[0:1:sections*2-1]) {
            spiral_ramp_section(inner_radius = inner_radius,
                                outer_radius = outer_radius,
                                tower_height = tower_height,
                                angle_start = floor( nth / 2.0 ) * angle_sweep,
                                angle_sweep = angle_sweep,
                                y_start = floor( nth / 2.0 ) * y_rise,
                                y_rise = y_rise,
                                thickness = thickness,
                                part = nth - floor( nth / 2.0 ) * 2);
        }
    }
} else {
    // do part of one pie-slice of the tower
    spiral_ramp_section(inner_radius = inner_radius,
                        outer_radius = outer_radius,
                        tower_height = tower_height,
                        angle_start = floor( nth / 2.0 ) * angle_sweep,
                        angle_sweep = angle_sweep,
                        y_start = floor( nth / 2.0 ) * y_rise,
                        y_rise = y_rise,
                        thickness = thickness,
                        part = nth - floor( nth / 2.0 ) * 2.0);
}
