

axis_radius = 0.05;
axis_length = 3.0;

rotate([0, 90, 0]) {
    color([1, 0, 0]) {
        translate([0, 0, axis_length/2]) {
            cylinder(h=axis_length, r1=axis_radius, r2=axis_radius, center=true, $fn=12);
        }
        translate([0, 0, axis_length]) {
            sphere(r = 0.2, $fn=12);
        }
    }
}


rotate([-90, 0, 0]) {
    color([0, 1, 0]) {
        translate([0, 0, axis_length/2]) {
            cylinder(h=axis_length, r1=axis_radius, r2=axis_radius, center=true, $fn=12);
        }
        translate([0, 0, axis_length]) {
            sphere(r = 0.4, $fn=12);
        }
    }
}


rotate([0, 0, 0]) {
    color([0, 0, 1]) {
        translate([0, 0, axis_length/2]) {
            cylinder(h=axis_length, r1=axis_radius, r2=axis_radius, center=true, $fn=12);
        }
        translate([0, 0, axis_length]) {
            sphere(r = 0.8, $fn=12);
        }
    }
}
