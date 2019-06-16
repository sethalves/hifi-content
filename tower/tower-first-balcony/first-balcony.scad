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
                             subsection_count = 4,
                             arch_radius,
                             low_collision_box = 0,
                             high_collision_box = 0)
{
    section_width = 2 * inner_radius * sin(angle_sweep / 2);
    collision_box_z = (section_width - (2 * arch_radius)) / 2;

    if (low_collision_box == 0 && high_collision_box == 0) {
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
    }  else if (low_collision_box == 1) {
        rotate([0, 180 - angle_start, 0]) {
            translate([inner_radius, 0, 0]) {
                cube([outer_radius - inner_radius, tower_height - thickness, collision_box_z], center = false);
            }
        }
    } else if (high_collision_box == 1) {
        collision_box_sweep = asin(collision_box_z / inner_radius);
        echo(collision_box_sweep=collision_box_sweep);
        // rotate([0, 180 - (angle_start + angle_sweep - collision_box_sweep), 0]) {
        rotate([0, 180 - (angle_start + angle_sweep), 0]) {
            translate([inner_radius, 0, -collision_box_z]) {
                cube([outer_radius - inner_radius, tower_height - thickness, collision_box_z], center = false);
            }
        }
    }
}


inner_radius = 24;
outer_radius = 36;
tower_height = 20;
thickness = 3;
angle_start = 0;
subsection_count = 4;

sections = 8;
total_angle_sweep = 270.0;

section_angle_sweep = total_angle_sweep / sections;
half_section_angle_sweep = section_angle_sweep / 2.0;

arch_height_ratio = 0.7;

// radius of arch-cutout cone at the inner side of the wall
arch_radius = (tower_height - thickness) - (tower_height * arch_height_ratio);
// 1/2 horizontal angle sweep to get from one side of arch-cutout to the other
arch_half_angle = asin(arch_radius / inner_radius);
// radius of arch cutout 1 closer than inner_radius
arch_cone_inner_radius = sin(arch_half_angle) * (inner_radius - 1.0);

// do a goatse stretch on the arch cutouts so that the supports touch down as a rectagle, despite the curve
cylinder_length = (outer_radius - inner_radius + 2.0);
d = (sin(half_section_angle_sweep) * (inner_radius - 1.0)) -
    (sin(half_section_angle_sweep) * (outer_radius + 1.0));
minkowski_points = [[0, 0, 0],
                    [-d, 0, cylinder_length],
                    [d, 0, cylinder_length],
                    [0, 0.01, 0],
                    [-d, 0.01, cylinder_length],
                    [d, 0.01, cylinder_length]];



combined = 1; // this can be overridden by the Makefile

if (combined == 1) {
    difference() {
        for(nth=[0:1:(sections * subsection_count - 1)]) {
            // subsection = nth - floor(nth / subsection_count);
            // for(subsection=[0:1:(subsection_count-1)]) {
                first_balcony_section(inner_radius = inner_radius,
                                      outer_radius = outer_radius,
                                      tower_height = tower_height,
                                      thickness = tower_height,
                                      angle_start = angle_start + (floor(nth / subsection_count) * section_angle_sweep),
                                      angle_sweep = section_angle_sweep,
                                      subsection = nth - (floor(nth / subsection_count) * subsection_count),
                                      subsection_count = subsection_count,
                                      arch_radius = arch_radius,
                                      low_collision_box = 0,
                                      high_collision_box = 0);
             // }
        }

        // cut out arches
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
                     };
                     polyhedron(points=minkowski_points, faces=[[0, 1, 2],
                                                                [5, 4, 3],
                                                                [0, 3, 4, 1],
                                                                [4, 5, 2, 1],
                                                                [0, 2, 5, 3]]);
            }
        }
    }
 } else {
    // combined isn't 1, so we are making part of the collision hull.  nth will have been set by the invoker of openscad.

    if (nth < 32) {
        first_balcony_section(inner_radius = inner_radius,
                              outer_radius = outer_radius,
                              tower_height = tower_height,
                              thickness = thickness,
                              angle_start = angle_start + (floor(nth / subsection_count) * section_angle_sweep),
                              angle_sweep = section_angle_sweep,
                              subsection = nth - (floor(nth / subsection_count) * subsection_count),
                              subsection_count = subsection_count,
                              arch_radius = arch_radius,
                              low_collision_box = 0,
                              high_collision_box = 0);
    } else if (nth < 40) {
        // collision shape before the arch
        mth = nth - 32;
        first_balcony_section(inner_radius = inner_radius,
                              outer_radius = outer_radius,
                              tower_height = tower_height,
                              thickness = thickness,
                              angle_start = angle_start + mth * section_angle_sweep,
                              angle_sweep = section_angle_sweep,
                              subsection = 0,
                              subsection_count = 1,
                              arch_radius = arch_radius,
                              low_collision_box = 1,
                              high_collision_box = 0);
    } else {
      // collision shape after the arch
        oth = nth - 40;
        first_balcony_section(inner_radius = inner_radius,
                              outer_radius = outer_radius,
                              tower_height = tower_height,
                              thickness = thickness,
                              angle_start = angle_start + oth * section_angle_sweep,
                              angle_sweep = section_angle_sweep,
                              subsection = 0,
                              subsection_count = 1,
                              arch_radius = arch_radius,
                              low_collision_box = 0,
                              high_collision_box = 1);
    }
 }
