grid_size = 0.1;
wire_radius = 0.01;

module place_cuboid(low_x, high_x, low_y, high_y, low_z, high_z) {
    translate([low_x, low_y, low_z]) {
        cube([high_x - low_x, high_y - low_y, high_z - low_z], false);
    };
}

module debug_grid() {
    for (x = [-3,-2,-1,0,1,2]) {
        for (z = [-3,-2,-1,0,1,2]) {
            place_cuboid(x * grid_size - 0.005, x * grid_size + 0.005,
                         -grid_size, grid_size,
                         z * grid_size - 0.005, z * grid_size + 0.005);
        }
    }
}
