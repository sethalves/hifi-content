width=1;
depth=0.5;
height = 0.5;
wall_thickness = 0.05;

output_box = 0;
output_lid = 0;

output_box_hull = 0;
n = 0;


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
                rotate([0, 0, 90]) {
                    cylinder(h=width, d=depth, center=true, $fn=10);
                }
            }
            translate([0, height, 0]) {
                rotate([0, 0, 90]) {
                    cylinder(h=width - (wall_thickness * 2), d=(depth - (wall_thickness * 2)), center=true, $fn=10);
                }
            }
        }
        translate([0, height * 1.5, 0]) {
            cube([width*2, height, depth*2], true);
        };
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
