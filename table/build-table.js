"use strict";

/* global Entities, Script, MyAvatar, Vec3 */

(function() { // BEGIN LOCAL_SCOPE
    // these should match what's in table.scad
    var width = 2.5;
    var height = 0.95;
    var depth = 1;
    var table_surface_height = 0.35;
    var edge_size = 0.07;
    var gap = 0.01;

    var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.1, z: -2}));
    var lifetime = 600;


    var table_surface_y = (height / 2) - (table_surface_height / 2);

    var drawer_hole_width = (width / 2) - (edge_size * 1.5);
    var drawer_hole_height = table_surface_height - (edge_size * 2);
    var drawer_hole_depth = depth - edge_size;

    var left_drawer_center_x = (-edge_size / 2) - (drawer_hole_width / 2);
    var right_drawer_center_x = (edge_size / 2) + (drawer_hole_width / 2);
    var drawer_center_y = table_surface_y;
    var drawer_center_z = (depth / 2) - (drawer_hole_depth / 2);

    var drawer_width = drawer_hole_width - (gap * 2);
    var drawer_height = drawer_hole_height - (gap * 2);
    var drawer_depth = depth - edge_size - gap;


    var tableID = Entities.addEntity({
        name: "table with drawers",
        type: "Model",
        modelURL: Script.resolvePath("table.obj"),
        shapeType: "compound",
        compoundShapeURL: Script.resolvePath("table-hull.obj"),
        dimensions: { x: width, y: height, z: depth },
        position: pos,
        dynamic: true,
        collisionless: false,
        gravity: { x: 0, y: 0, z: 0 },
        lifetime: lifetime,
        userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
    });

    var leftDrawerPos = Vec3.sum(pos, { x: left_drawer_center_x, y: drawer_center_y, z: drawer_center_z });
    var leftDrawerID = Entities.addEntity({
        name: "table left drawer",
        type: "Model",
        modelURL: Script.resolvePath("table-drawer.obj"),
        shapeType: "compound",
        compoundShapeURL: Script.resolvePath("table-drawer-hull.obj"),
        dimensions: { x: drawer_width, y: drawer_height, z: drawer_depth },
        position: leftDrawerPos,
        dynamic: true,
        collisionless: false,
        gravity: { x: 0, y: 0, z: 0 },
        lifetime: lifetime,
        userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
    });

    var rightDrawerPos = Vec3.sum(pos, { x: right_drawer_center_x, y: drawer_center_y, z: drawer_center_z });
    var rightDrawerID = Entities.addEntity({
        name: "table right drawer",
        type: "Model",
        modelURL: Script.resolvePath("table-drawer.obj"),
        shapeType: "compound",
        compoundShapeURL: Script.resolvePath("table-drawer-hull.obj"),
        dimensions: { x: drawer_width, y: drawer_height, z: drawer_depth },
        position: Vec3.sum(pos, { x: right_drawer_center_x, y: drawer_center_y, z: drawer_center_z }),
        dynamic: true,
        collisionless: false,
        gravity: { x: 0, y: 0, z: 0 },
        lifetime: lifetime,
        userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
    });

    Entities.addAction("slider", leftDrawerID, {
        point: { x: 0, y: 0, z: 0 },
        axis: { x: 0, y: 0, z: 1 },
        otherEntityID: tableID,
        otherPoint: leftDrawerPos,
        otherAxis: { x: 0, y: 0, z: 1 },
        linearLow: 0,
        linearHigh: 0.6,
        tag: "left drawer slider"
    });

    Entities.addAction("slider", rightDrawerID, {
        point: { x: 0, y: 0, z: 0 },
        axis: { x: 0, y: 0, z: 1 },
        otherEntityID: tableID,
        otherPoint: rightDrawerPos,
        otherAxis: { x: 0, y: 0, z: 1 },
        linearLow: 0,
        linearHigh: 0.6,
        tag: "right drawer slider"
    });
}()); // END LOCAL_SCOPE
