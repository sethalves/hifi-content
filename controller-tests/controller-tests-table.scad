table_sections = 32;
table_radius = 9;

rotate([0, (360.0 / table_sections) / 2, 0]) {
    rotate([90, 0, 0]) {
        difference() {
            cylinder(h = 0.15, r1 = table_radius + 1, r2 = table_radius + 1, center = true, $fn=table_sections);
            cylinder(h = 0.2, r1 = table_radius - 1, r2 = table_radius - 1, center = true, $fn=table_sections);
        }
    }
}
