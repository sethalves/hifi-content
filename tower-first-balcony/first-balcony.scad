//
//
//

module first_balcony_section(inner_radius = 20,
                             outer_radius = 32,
                             tower_height = 20,
                             thickness = 3,
                             angle_start = 0,
                             angle_sweep = 20,
                             subsection,
                             subsection_count = 4)
{
    subsection_angle_sweep = angle_sweep / subsection_count;

    p0x = inner_radius * sin(angle_start + (subsection * subsection_angle_sweep));
    p0y = tower_height - thickness;
    p0z = inner_radius * cos(angle_start + (subsection * subsection_angle_sweep));

    p1x = outer_radius * sin(angle_start + (subsection * subsection_angle_sweep));
    p1y = tower_height - thickness;
    p1z = outer_radius * cos(angle_start + (subsection * subsection_angle_sweep));

    // p2 and p3 are on the left of the section
    p2x = outer_radius * sin(angle_start + (subsection * subsection_angle_sweep) + subsection_angle_sweep);
    p2y = tower_height - thickness;
    p2z = outer_radius * cos(angle_start + (subsection * subsection_angle_sweep) + subsection_angle_sweep);

    p3x = inner_radius * sin(angle_start + (subsection * subsection_angle_sweep) + subsection_angle_sweep);
    p3y = tower_height - thickness;
    p3z = inner_radius * cos(angle_start + (subsection * subsection_angle_sweep) + subsection_angle_sweep);

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
sections = 8;
total_angle_sweep = 270.0;
section_angle_sweep = total_angle_sweep / sections;
half_section_angle_sweep = section_angle_sweep / 2.0;

arch_height_ratio = 0.6;

// radius of arch-cutout cone at the inner side of the wall
arch_radius = (tower_height - thickness) - (tower_height * arch_height_ratio);
// 1/2 horizontal angle sweep to get from one side of arch-cutout to the other
arch_half_angle = asin(arch_radius / inner_radius);
// radius of arch cutout 1 closer than inner_radius
arch_cone_inner_radius = sin(arch_half_angle) * (inner_radius - 1.0);
// radius of arch coutout 1 further than outer_radius
arch_cone_outer_radius = sin(arch_half_angle) * (outer_radius + 1.0);

// do a goatse stretch on the arch cutouts
cylinder_length = (outer_radius - inner_radius + 2.0);
// d = sin(arch_half_angle) * cylinder_length;
d = (arch_cone_outer_radius - arch_cone_inner_radius);
minkowski_points = [[0, 0, 0],
                    [-d, 0, cylinder_length],
                    [d, 0, cylinder_length]];



combined = 1; // this can be overridden by the Makefile

if (combined == 1) {
    difference() {
        for(nth=[0:1:(sections-1)]) {
            for(subsection=[0:1:(subsection_count-1)]) {
                first_balcony_section(inner_radius = inner_radius,
                                      outer_radius = outer_radius,
                                      tower_height = tower_height,
                                      thickness = tower_height,
                                      angle_start = angle_start + (nth * section_angle_sweep),
                                      angle_sweep = section_angle_sweep,
                                      subsection = subsection,
                                      subsection_count = subsection_count);
            }

        }

        // cut out arch
        for(nth=[0:1:(sections-1)]) {
            rotate([0, section_angle_sweep * nth + section_angle_sweep / 2.0, 0])
                translate([0, tower_height * arch_height_ratio, inner_radius-1])
                minkowski() {
                     union() {
                         cylinder(h=cylinder_length,
                                  r=arch_cone_inner_radius,
                                  $fs=0.5);

                         translate([0, - tower_height / 2.0, cylinder_length / 2.0])
                         cube([arch_cone_inner_radius * 2.0,
                               tower_height,
                               cylinder_length],
                              center = true);
                     }
                     polyhedron(points=minkowski_points, faces=[[0, 1, 2]]);
            }
        }
    }
 }
