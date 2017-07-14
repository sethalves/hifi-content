
torus_radius = 30;
chamber_radius = 10;
wall_thickness = 1.5;
to_segments = 16;
po_segments = 10;


// difference() {
//     rotate_extrude(convexity = 10) {
//         translate([center_radius, 0, 0]) {
//             circle(r = outer_radius - inner_radius);
//         };
//     };
//     rotate_extrude(convexity = 10) {
//         translate([center_radius, 0, 0]) {
//             circle(r = outer_radius - inner_radius - (2 * wall_thickness));
//         };
//     };
// };


// https://en.wikipedia.org/wiki/Toroidal_and_poloidal
// http://fusionwiki.ciemat.es/wiki/Toroidal_coordinates
// https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/User-Defined_Functions_and_Modules

// R0 is radius from origin to center of circle that is swept to form torus
// r is radius of swept circle
// toroidal_angle controls direction of R0
// poloidal_angle controls direction of r
function toroidal_to_x(R0, r, toroidal_angle, poloidal_angle) = (R0 + (r * cos(poloidal_angle))) * cos(toroidal_angle);
function toroidal_to_y(R0, r, toroidal_angle, poloidal_angle) = (R0 + (r * cos(poloidal_angle))) * sin(toroidal_angle);
function toroidal_to_z(R0, r, toroidal_angle, poloidal_angle) = r * sin(poloidal_angle);


module toroidal_section(to_low, to_high, po_low, po_high) {
    p0x = toroidal_to_x(torus_radius, chamber_radius - wall_thickness, to_low, po_low);
    p0y = toroidal_to_y(torus_radius, chamber_radius - wall_thickness, to_low, po_low);
    p0z = toroidal_to_z(torus_radius, chamber_radius - wall_thickness, to_low, po_low);

    p1x = toroidal_to_x(torus_radius, chamber_radius - wall_thickness, to_low, po_high);
    p1y = toroidal_to_y(torus_radius, chamber_radius - wall_thickness, to_low, po_high);
    p1z = toroidal_to_z(torus_radius, chamber_radius - wall_thickness, to_low, po_high);

    p2x = toroidal_to_x(torus_radius, chamber_radius - wall_thickness, to_high, po_high);
    p2y = toroidal_to_y(torus_radius, chamber_radius - wall_thickness, to_high, po_high);
    p2z = toroidal_to_z(torus_radius, chamber_radius - wall_thickness, to_high, po_high);

    p3x = toroidal_to_x(torus_radius, chamber_radius - wall_thickness, to_high, po_low);
    p3y = toroidal_to_y(torus_radius, chamber_radius - wall_thickness, to_high, po_low);
    p3z = toroidal_to_z(torus_radius, chamber_radius - wall_thickness, to_high, po_low);

    p4x = toroidal_to_x(torus_radius, chamber_radius, to_low, po_low);
    p4y = toroidal_to_y(torus_radius, chamber_radius, to_low, po_low);
    p4z = toroidal_to_z(torus_radius, chamber_radius, to_low, po_low);

    p5x = toroidal_to_x(torus_radius, chamber_radius, to_low, po_high);
    p5y = toroidal_to_y(torus_radius, chamber_radius, to_low, po_high);
    p5z = toroidal_to_z(torus_radius, chamber_radius, to_low, po_high);

    p6x = toroidal_to_x(torus_radius, chamber_radius, to_high, po_high);
    p6y = toroidal_to_y(torus_radius, chamber_radius, to_high, po_high);
    p6z = toroidal_to_z(torus_radius, chamber_radius, to_high, po_high);

    p7x = toroidal_to_x(torus_radius, chamber_radius, to_high, po_low);
    p7y = toroidal_to_y(torus_radius, chamber_radius, to_high, po_low);
    p7z = toroidal_to_z(torus_radius, chamber_radius, to_high, po_low);

    polyhedron(
        points = [[p0x, p0y, p0z],
                  [p1x, p1y, p1z],
                  [p2x, p2y, p2z],
                  [p3x, p3y, p3z],
                  [p4x, p4y, p4z],
                  [p5x, p5y, p5z],
                  [p6x, p6y, p6z],
                  [p7x, p7y, p7z]],
        faces = [[0, 1, 2, 3],
                 [0, 4, 5, 1],
                 [4, 7, 6, 5],
                 [2, 6, 7, 3],
                 [1, 5, 6, 2],
                 [0, 3, 7, 4]],
        convexity = 10);
}


for (toroidal_segment = [0 : 1 : to_segments - 2]) {
    to_low = (toroidal_segment / to_segments) * 360;
    to_high = ((toroidal_segment + 1) / to_segments) * 360;

    for (poloidal_segment = [0 : 1 : po_segments - 2]) {
        po_low = (poloidal_segment / po_segments) * 360;
        po_high = ((poloidal_segment + 1) / po_segments) * 360;

        toroidal_section(to_low, to_high, po_low, po_high);
    }
}
