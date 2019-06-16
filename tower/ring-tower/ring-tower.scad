
torus_radius = 30;
chamber_radius = 10;
wall_thickness = 1.5;
tor_segments = 15;
pol_segments = 10;


// https://en.wikipedia.org/wiki/Toroidal_and_poloidal
// http://fusionwiki.ciemat.es/wiki/Toroidal_coordinates
// https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/User-Defined_Functions_and_Modules

// R0 is radius from origin to center of circle that is swept to form torus
// r is radius of swept circle
// toroidal_angle controls direction of R0
// poloidal_angle controls direction of r
function toroidal_tor_x(R0, r, toroidal_angle, poloidal_angle) = (R0 + (r * cos(poloidal_angle))) * cos(toroidal_angle);
function toroidal_tor_y(R0, r, toroidal_angle, poloidal_angle) = (R0 + (r * cos(poloidal_angle))) * sin(toroidal_angle);
function toroidal_tor_z(R0, r, toroidal_angle, poloidal_angle) = r * sin(poloidal_angle);


module toroidal_section_by_angle(tor_low, tor_high, pol_low, pol_high) {
    p0x = toroidal_tor_x(torus_radius, chamber_radius - wall_thickness, tor_low, pol_low);
    p0y = toroidal_tor_y(torus_radius, chamber_radius - wall_thickness, tor_low, pol_low);
    p0z = toroidal_tor_z(torus_radius, chamber_radius - wall_thickness, tor_low, pol_low);

    p1x = toroidal_tor_x(torus_radius, chamber_radius - wall_thickness, tor_low, pol_high);
    p1y = toroidal_tor_y(torus_radius, chamber_radius - wall_thickness, tor_low, pol_high);
    p1z = toroidal_tor_z(torus_radius, chamber_radius - wall_thickness, tor_low, pol_high);

    p2x = toroidal_tor_x(torus_radius, chamber_radius - wall_thickness, tor_high, pol_high);
    p2y = toroidal_tor_y(torus_radius, chamber_radius - wall_thickness, tor_high, pol_high);
    p2z = toroidal_tor_z(torus_radius, chamber_radius - wall_thickness, tor_high, pol_high);

    p3x = toroidal_tor_x(torus_radius, chamber_radius - wall_thickness, tor_high, pol_low);
    p3y = toroidal_tor_y(torus_radius, chamber_radius - wall_thickness, tor_high, pol_low);
    p3z = toroidal_tor_z(torus_radius, chamber_radius - wall_thickness, tor_high, pol_low);

    p4x = toroidal_tor_x(torus_radius, chamber_radius, tor_low, pol_low);
    p4y = toroidal_tor_y(torus_radius, chamber_radius, tor_low, pol_low);
    p4z = toroidal_tor_z(torus_radius, chamber_radius, tor_low, pol_low);

    p5x = toroidal_tor_x(torus_radius, chamber_radius, tor_low, pol_high);
    p5y = toroidal_tor_y(torus_radius, chamber_radius, tor_low, pol_high);
    p5z = toroidal_tor_z(torus_radius, chamber_radius, tor_low, pol_high);

    p6x = toroidal_tor_x(torus_radius, chamber_radius, tor_high, pol_high);
    p6y = toroidal_tor_y(torus_radius, chamber_radius, tor_high, pol_high);
    p6z = toroidal_tor_z(torus_radius, chamber_radius, tor_high, pol_high);

    p7x = toroidal_tor_x(torus_radius, chamber_radius, tor_high, pol_low);
    p7y = toroidal_tor_y(torus_radius, chamber_radius, tor_high, pol_low);
    p7z = toroidal_tor_z(torus_radius, chamber_radius, tor_high, pol_low);

    polyhedron(
        points = [[p0x, p0y, p0z],
                  [p1x, p1y, p1z],
                  [p2x, p2y, p2z],
                  [p3x, p3y, p3z],
                  [p4x, p4y, p4z],
                  [p5x, p5y, p5z],
                  [p6x, p6y, p6z],
                  [p7x, p7y, p7z]],
        faces = [[1, 2, 3],
                 [3, 0, 1],
                 [5, 1, 0],
                 [0, 4, 5],
                 [6, 5, 4],
                 [4, 7, 6],
                 [2, 6, 2],
                 [7, 3, 2],
                 [0, 3, 7],
                 [7, 4, 0],
                 [5, 6, 2],
                 [2, 1, 5]
            ],
        convexity = 10);
}


module toroidal_section_by_index(toroidal_segment, poloidal_segment) {
    // tor_low = (toroidal_segment / tor_segments) * 360 + 5;
    // tor_high = ((toroidal_segment + 1) / tor_segments) * 360 - 5;
    // pol_low = (poloidal_segment / pol_segments) * 360 + 5;
    // pol_high = ((poloidal_segment + 1) / pol_segments) * 360 - 5;

    if ((toroidal_segment == 0 && poloidal_segment == 0) ||
        // (toroidal_segment == 5 && poloidal_segment == 0) ||
        (toroidal_segment == 10 && poloidal_segment == 0)
        ) {
        tor_low = (toroidal_segment / tor_segments) * 360;
        tor_high = ((toroidal_segment + 1) / tor_segments) * 360;
        pol_low = (poloidal_segment / pol_segments) * 360;
        pol_high = pol_low + 5;

        hull() {
            toroidal_section_by_angle(tor_low, tor_high, pol_low, pol_high);
        }
    } else {
        tor_low = (toroidal_segment / tor_segments) * 360;
        tor_high = ((toroidal_segment + 1) / tor_segments) * 360;
        pol_low = (poloidal_segment / pol_segments) * 360;
        pol_high = ((poloidal_segment + 1) / pol_segments) * 360;

        hull() {
            toroidal_section_by_angle(tor_low, tor_high, pol_low, pol_high);
        }
    }
}


module ring_section(tor_index, pol_index) {
    if (pol_index == 0) {
        // wall -- outer standing-height section
        toroidal_section_by_index(tor_index, 0);
    } else if (pol_index == 1) {
        // floor
        hull() {
            toroidal_section_by_index(tor_index, 1);
            toroidal_section_by_index(tor_index, 2);
            toroidal_section_by_index(tor_index, 3);
        }
    } else {
        // inner wall and ceiling
        toroidal_section_by_index(tor_index, pol_index + 2);
    }
}

part_index = -1;

union() {
    rotate([90, 0, 0]) {
        if (part_index == -1) {
            for (tor_index = [0 : 1 : tor_segments - 1]) {
                for (pol_index = [0 : 1 : pol_segments - 3]) {
                    ring_section(tor_index, pol_index);
                }
            }
        } else {
            // if tor_segments = 15 and pol_segments = 10,
            // tor_index goes from 0 to 14
            // pol_index goes from 0 to 8
            tor_index = part_index % tor_segments;
            pol_index = (part_index - tor_index) / tor_segments;
            ring_section(tor_index, pol_index);
        }
    }
}
