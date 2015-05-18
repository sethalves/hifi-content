
FIELD_WIDTH = 1.21;
FIELD_LENGTH = 1.92;
FLOOR_THICKNESS = 0.20;
EDGE_THICKESS = 0.10;
EDGE_HEIGHT = 0.14;
GOAL_WIDTH = 0.35;
VERT_OFFSET = [0, 0.537, 0]; // 0.535 | 0.54
MODEL_SCALE = 1.52;


scale(1.0 / MODEL_SCALE) {
    translate(VERT_OFFSET) {

        // Makefile currently skips over the floor.  The floor is provided by airHockey.js, instead.
        if (nth == 0) {
            // floor
            translate([0, 0, 0])
                cube([FIELD_WIDTH, FLOOR_THICKNESS, FIELD_LENGTH], center=true);
        }

        if (nth == 1) {
            // edge
            translate([FIELD_WIDTH / 2.0, FLOOR_THICKNESS / 2.0, 0])
                cube([EDGE_THICKESS, EDGE_HEIGHT, FIELD_LENGTH + EDGE_THICKESS], center=true);
        }

        if (nth == 2) {
            // edge
            translate([-FIELD_WIDTH / 2.0, FLOOR_THICKNESS / 2.0, 0])
                cube([EDGE_THICKESS, EDGE_HEIGHT, FIELD_LENGTH + EDGE_THICKESS], center=true);
        }

        if (nth == 3) {
            // edge
            translate([FIELD_WIDTH / 4.0 + (GOAL_WIDTH / 4.0), FLOOR_THICKNESS / 2.0, -FIELD_LENGTH / 2.0])
                cube([FIELD_WIDTH / 2.0 - GOAL_WIDTH / 2.0, EDGE_HEIGHT, EDGE_THICKESS], center=true);
        }

        if (nth == 4) {
            // edge
            translate([-FIELD_WIDTH / 4.0 - (GOAL_WIDTH / 4.0), FLOOR_THICKNESS / 2.0, -FIELD_LENGTH / 2.0])
                cube([FIELD_WIDTH / 2.0 - GOAL_WIDTH / 2.0, EDGE_HEIGHT, EDGE_THICKESS], center=true);
        }


        if (nth == 5) {
            // edge
            translate([FIELD_WIDTH / 4.0 + (GOAL_WIDTH / 4.0), FLOOR_THICKNESS / 2.0, FIELD_LENGTH / 2.0])
                cube([FIELD_WIDTH / 2.0 - GOAL_WIDTH / 2.0, EDGE_HEIGHT, EDGE_THICKESS], center=true);
        }

        if (nth == 6) {
            // edge
            translate([-FIELD_WIDTH / 4.0 - (GOAL_WIDTH / 4.0), FLOOR_THICKNESS / 2.0, FIELD_LENGTH / 2.0])
                cube([FIELD_WIDTH / 2.0 - GOAL_WIDTH / 2.0, EDGE_HEIGHT, EDGE_THICKESS], center=true);
        }


        if (nth == 7) {
            // corner
            translate([-FIELD_WIDTH / 2.0 + EDGE_THICKESS, FLOOR_THICKNESS / 2.0, FIELD_LENGTH / 2.0 - EDGE_THICKESS])
                difference() {
                cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS], center=true);
                rotate([0, 45, 0])
                    translate([EDGE_THICKESS / 2.0, 0, EDGE_THICKESS / 2.0])
                    cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS * 3], center=true);
            }
        }

        if (nth == 8) {
            // corner
            translate([FIELD_WIDTH / 2.0 - EDGE_THICKESS, FLOOR_THICKNESS / 2.0, FIELD_LENGTH / 2.0 - EDGE_THICKESS])
                difference() {
                cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS], center=true);
                rotate([0, -45, 0])
                    translate([-EDGE_THICKESS / 2.0, 0, EDGE_THICKESS / 2.0])
                    cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS * 3], center=true);
            }
        }

        if (nth == 9) {
            // corner
            translate([-FIELD_WIDTH / 2.0 + EDGE_THICKESS, FLOOR_THICKNESS / 2.0, -FIELD_LENGTH / 2.0 + EDGE_THICKESS])
                difference() {
                cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS], center=true);
                rotate([0, -45, 0])
                    translate([EDGE_THICKESS / 2.0, 0, -EDGE_THICKESS / 2.0])
                    cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS * 3], center=true);
            }
        }

        if (nth == 10) {
            // corner
            translate([FIELD_WIDTH / 2.0 - EDGE_THICKESS, FLOOR_THICKNESS / 2.0, -FIELD_LENGTH / 2.0 + EDGE_THICKESS])
                difference() {
                cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS], center=true);
                rotate([0, 45, 0])
                    translate([-EDGE_THICKESS / 2.0, 0, -EDGE_THICKESS / 2.0])
                    cube([EDGE_THICKESS, EDGE_HEIGHT, EDGE_THICKESS * 3], center=true);
            }
        }
    }
}
