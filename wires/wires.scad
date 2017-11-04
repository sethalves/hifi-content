

grid_size = 0.1; // should match gridSize in wires-shared.js
wire_radius = 0.01;

center = 0;
segment_0 = 0;
segment_1 = 0;
segment_2 = 0;
segment_3 = 0;
segment_4 = 0;
segment_5 = 0;


module main() {

    if (center) {
        sphere(r=wire_radius*1.5);
    }

    if (segment_0) {
        rotate([0, 90, 0]) {
            cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
        }
    }

    if (segment_1) {
        rotate([0, 180, 0]) {
            rotate([0, 0, 180]) {
                cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
            }
        }
    }

    if (segment_2) {
        rotate([0, -90, 0]) {
            rotate([0, 0, 180]) {
                cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
            }
        }
    }

    if (segment_3) {
        rotate([0, 0, 0]) {
            cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
        }
    }

    if (segment_4) {
        rotate([90, 0, 0]) {
            cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
        }
    }

    if (segment_5) {
        rotate([-90, 0, 0]) {
            cylinder(grid_size / 2, r1=wire_radius, r2=wire_radius);
        }
    }
}

main();
