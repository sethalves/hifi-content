width=1;
depth=0.5;
height = 0.5;
wall_thickness = 0.05;

output_box = 0;
output_lid = 0;

output_box_hull = 0;
output_lid_hull = 0;
n = 0;

lid_slats = 5;

module chest_box() {
    difference() {
        translate([0, height / 2, 0]) {
            cube([width, height, depth], true);
        };
        translate([0, height / 2 + wall_thickness, 0]) {
            cube([width - (2 * wall_thickness),
                  height,
                  depth - (2 * wall_thickness)], true);
        };
    }
}

module chest_box_hull(n) {
    if (n == 0) {
        // bottom of box
        translate([0, wall_thickness / 2, 0]) {
            cube([width, wall_thickness, depth], true);
        };
    }
    if (n == 1) {
        // front of box
        translate([0, height / 2, (depth / 2) - (wall_thickness / 2)]) {
            cube([width, height, wall_thickness], true);
        };
    }
    if (n == 2) {
        // right of box
        translate([(width / 2) - (wall_thickness / 2), height / 2, 0]) {
            cube([wall_thickness, height, depth], true);
        };
    }
    if (n == 3) {
        // back of box
        translate([0, height / 2, (-depth / 2) + (wall_thickness / 2)]) {
            cube([width, height, wall_thickness], true);
        };
    }
    if (n == 4) {
        // left of box
        translate([(-width / 2) + (wall_thickness / 2), height / 2, 0]) {
            cube([wall_thickness, height, depth], true);
        };
    }
}

module chest_lid() {
    intersection() {
        difference() {
            translate([0, height, 0]) {
                rotate([0, 90, 0]) {
                    cylinder(h=width, d=depth, center=true, $fn=10);
                }
            }
            translate([0, height, 0]) {
                rotate([0, 90, 0]) {
                    cylinder(h=width - (wall_thickness * 2), d=(depth - (wall_thickness * 2)), center=true, $fn=(lid_slats * 2));
                }
            }
        }
        translate([0, height * 1.5, 0]) {
            cube([width*2, height, depth*2], true);
        };
    }
}

module chest_lid_hull(n) {
    if (n == 0) {
        // left side
        intersection() {
            chest_lid();
            translate([(-width / 2) + (wall_thickness / 2) , height * 1.5, 0]) {
                cube([wall_thickness, height, depth*2], true);
            };
        };
    } else if (n == 1) {
        // right side
        intersection() {
            chest_lid();
            translate([(width / 2) - (wall_thickness / 2) , height * 1.5, 0]) {
                cube([wall_thickness, height, depth*2], true);
            };
        };
    } else {
        // boards in lid
        angle = (n-1) * (180 / lid_slats) - (180 / (lid_slats * 2));
        intersection() {
            translate([0, height, 0]) {
                rotate([-angle, 0, 0]) {
                    translate([0, 0, (depth * 0.476) - (wall_thickness / 2)]) {
                        // half_circumference = depth * 3.1415 / 2;
                        // slat_size = half_circumference / lid_slats;
                        slat_span_angle = 180 / lid_slats;
                        slat_size = sin(slat_span_angle) * (depth / 2);
                        cube([width, slat_size, wall_thickness], true);
                    };
                }
            };
            translate([(width / 2) - (wall_thickness / 2) , height * 1.5, 0]) {
                cube([width*2, height, depth*2], true);
            };
        }
    }
}



if (output_box) {
    chest_box();
}
if (output_box_hull) {
    chest_box_hull(n);
}
if (output_lid) {
    chest_lid();
}
if (output_lid_hull) {
    chest_lid_hull(n);
}
