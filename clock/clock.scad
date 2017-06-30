

outer_radius = 0.5;
edge_thickness = 0.05;
depth = 0.08;

inner_radius = outer_radius - edge_thickness;

translate([inner_radius, 0, inner_radius]) { // fit clock face along axis
    rotate([90, 0, 0]) {
        difference() {
            cylinder(h=depth, r1=outer_radius, r2=outer_radius, center=true, $fn=12);
            translate([0, 0, edge_thickness]) {
                cylinder(h=depth, r1=inner_radius, r2=inner_radius, center=true, $fn=12);
            }
        }
    }
}
