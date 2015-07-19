//
//
//

module spiral_ramp_section(inner_radius = 10,
                           outer_radius = 20,
                           tower_radius = 22,
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


    // p4 and p5 are the outer edge of the wall
    p4x = tower_radius * sin(angle_start);
    p4y = 0;
    p4z = tower_radius * cos(angle_start);

    p5x = tower_radius * sin(angle_start + angle_sweep);
    p5y = 0;
    p5z = tower_radius * cos(angle_start + angle_sweep);

    section_points = [[p0x, p0y, p0z],
                      [p1x, p1y, p1z],
                      [p2x, p2y, p2z],
                      [p3x, p3y, p3z],
                      [p0x, p0y + thickness, p0z],
                      [p1x, p1y + thickness, p1z],
                      [p2x, p2y + thickness, p2z],
                      [p3x, p3y + thickness, p3z],
                      // outer wall
                      [p1x, 0, p1z],
                      [p2x, 0, p2z],
                      [p4x, 0, p4z],
                      [p5x, 0, p5z],
                      [p1x, tower_height, p1z],
                      [p2x, tower_height, p2z],
                      [p4x, tower_height, p4z],
                      [p5x, tower_height, p5z]
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
    } else {
        polyhedron(points=section_points,
                   faces=[[8, 12, 13, 9], // near face
                          [10, 11, 15, 14], // far face
                          [12, 14, 15, 13], // top
                          [8, 9, 11, 10], // bottom
                          [8, 10, 14, 12], // right
                          [13, 15, 11, 9]] // left
                   );
    }
}


inner_radius = 10;
outer_radius = 20;
tower_radius = 22;
tower_height = 24;
angle_sweep = 22.5;
y_rise = 2;
thickness = 1;

combined = 1; // this can be overridden by the Makefile

if (combined == 1) {
    difference() {
        for(nth=[0:1:23]) {
            spiral_ramp_section(inner_radius = inner_radius,
                                outer_radius = outer_radius,
                                tower_radius = tower_radius,
                                tower_height = tower_height,
                                angle_start = floor( nth / 3.0 ) * angle_sweep,
                                angle_sweep = angle_sweep,
                                y_start = floor( nth / 3.0 ) * y_rise,
                                y_rise = y_rise,
                                thickness = thickness,
                                part = nth - floor( nth / 3.0 ) * 3);
        }
        for(nth=[0:1:7]) {
            rotate([0, angle_sweep * nth + angle_sweep / 2.0, 0])
                translate([0, y_rise * nth + y_rise + 3, outer_radius-1])
                cylinder(h=4, r=2, $fs=0.5);
        }
    }
} else {
    // do part of one pie-slice of the tower
    spiral_ramp_section(inner_radius = inner_radius,
                        outer_radius = outer_radius,
                        tower_radius = tower_radius,
                        tower_height = tower_height,
                        angle_start = floor( nth / 3.0 ) * angle_sweep,
                        angle_sweep = angle_sweep,
                        y_start = floor( nth / 3.0 ) * y_rise,
                        y_rise = y_rise,
                        thickness = thickness,
                        part = nth - floor( nth / 3.0 ) * 3.0);
}
