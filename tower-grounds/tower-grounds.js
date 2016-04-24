

Script.include([
    "voxel-ground-utils.js"
]);

// var center = {x: 0, y: -16, z: 0};
// var center = {x: 64, y: 50.2, z: 112};
var center = {x: 64, y: -0.5, z: 112};
var zeroVec = {x: 0, y: 0, z: 0};

var largePlotSize = 16; // 256;
var smallPlotSize = 16;

var SHOW_TOOL_BAR = true;
var toolBar;

if (SHOW_TOOL_BAR) {
    var HIFI_PUBLIC_BUCKET = "http://s3.amazonaws.com/hifi-public/";
    Script.include(HIFI_PUBLIC_BUCKET + "scripts/libraries/toolBars.js");

    var toolIconUrl = "http://headache.hungry.com/~seth/hifi/";
    var toolHeight = 50;
    var toolWidth = 50;
    var offAlpha = 0.8;
    var onAlpha = 1.0;
    var BUTTON_SIZE = 32;
    var PADDING = 3;

    toolBar = new ToolBar(0, 0, ToolBar.VERTICAL, "highfidelity.attachedEntities.toolbar", function(screenSize) {
        return {
            x: (BUTTON_SIZE + PADDING),
            y: (screenSize.y / 2 - BUTTON_SIZE * 2 + PADDING)
        };
    });
    var setTerrainButton = this.toolBar.addOverlay("image", {
        width: BUTTON_SIZE,
        height: BUTTON_SIZE,
        imageURL: HIFI_PUBLIC_BUCKET + "images/close.png",
        color: {
            red: 255,
            green: 255,
            blue: 255
        },
        alpha: 1
    });
}

function mousePressEvent(event) {
    var clickedOverlay = Overlays.getOverlayAtPoint({
        x: event.x,
        y: event.y
    });

    if (clickedOverlay == setTerrainButton) {
        if (false) {
            deleteTerrain();
        } else if (false) {
            addTerrain();
        } else {
            setTerrain();
        }
    }
}

function cleanup() {
    toolBar.cleanup();
}

function worldCoordsToMapCoords(worldCoords, voxelTerrainMin, voxelTerrainMax) {
    var offset = Vec3.subtract(worldCoords, center);
    var plotSize = largePlotSize;
    var vec = Vec3.multiply(offset, 1.0 / plotSize);
    return {
        x: vec.x,
        y: Math.sin(vec.x) * Math.cos(vec.z) * (16.0 / 64.0),
        z: vec.z
    };
}
function mapCoordsToWorldCoords(mapCoords, voxelTerrainMin, voxelTerrainMax) {
    var plotSize = largePlotSize;
    var vec = Vec3.multiply(mapCoords, plotSize);
    var blah = Vec3.sum(vec, center);
    return blah;
}

function deleteTerrain() {
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var props = Entities.getEntityProperties(polyVoxID, ["name", "position"]);
        if (props.name == "terrain") {
            Entities.editEntity(polyVoxID, {locked: false});
            Entities.deleteEntity(polyVoxID);
        }
    }

    addTerrain();
}

function addTerrain() {
    var voxelX = 0;
    var voxelZ = 0;
    var worker = function() {
        print("addTerrain: " + voxelX + " " + voxelZ);
        var position = {
            x: (voxelX - 2) * largePlotSize,
            y: 0,
            z: (voxelZ - 2) * largePlotSize
        };

        if (position.x == -256 && position.y == 0 && position.z == 0) {
            // skip this spot, fill it in with addFineTerrain
        } else {
            addTerrainAtPosition(Vec3.sum(center, position), largePlotSize);
        }

        if (voxelZ == 5) {
            voxelZ = 0;
            voxelX += 1;
        } else {
            voxelZ += 1;
        }
        if (voxelX == 5) {
            voxelX = 0;
            voxelZ = 0;
            addFineTerrain();
            return;
        }

        Script.setTimeout(worker, 100);
    }
    worker();
}

function addFineTerrain() {
    var voxelX = 0;
    var voxelY = 0;
    var voxelZ = 0;
    var fineCenter = { x: -256, y: -80, z: 0 };
    var worker = function() {
        print("addFineTerrain: " + voxelX + " " + voxelY + " " + voxelZ);
        var position = {
            x: (voxelX - 8) * smallPlotSize,
            y: (voxelY - 1) * smallPlotSize,
            z: (voxelZ - 8) * smallPlotSize
        };

        addTerrainAtPosition(Vec3.sum(fineCenter, position), smallPlotSize);

        voxelX += 1;
        if (voxelX == 16) {
            voxelX = 0;
            voxelY += 1;
        }
        if (voxelY == 3) {
            voxelY = 0;
            voxelZ += 1;
        }
        if (voxelZ == 16) {
            setTerrain();
            return;
        }
        Script.setTimeout(worker, 100);
    }
    worker();
}


function setTerrain() {
    unLockTerrain();
    var voxelTerrainMin = { x: 100000, y: 100000, z: 100000 };
    var voxelTerrainMax = { x: -100000, y: -100000, z: -100000 };
    var voxelTerrains = [];
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var properties = Entities.getEntityProperties(polyVoxID);
        if (properties.name == "terrain") {
            var plotSize = properties.dimensions.x;
            var halfPlotSize = { x: plotSize / 2, y: plotSize / 2, z: plotSize / 2 };

            voxelTerrains.push(polyVoxID);

            voxelTerrainMin = minVector(voxelTerrainMin, Vec3.subtract(properties.position, halfPlotSize));
            voxelTerrainMax = maxVector(voxelTerrainMax, Vec3.sum(properties.position, halfPlotSize));

            // link neighbors to this plot
            var imXNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: plotSize, y: 0, z: 0}), plotSize);
            var imYNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: plotSize, z: 0}), plotSize);
            var imZNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: plotSize}), plotSize);
            var imXPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: -plotSize, y: 0, z: 0}), plotSize);
            var imYPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: -plotSize, z: 0}), plotSize);
            var imZPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: -plotSize}), plotSize);

            if (imXNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imXNNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.xNNeighborID = polyVoxID;
                    Entities.editEntity(imXNNeighborFor, neighborProperties);
                }
            }
            if (imYNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imYNNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.yNNeighborID = polyVoxID;
                    Entities.editEntity(imYNNeighborFor, neighborProperties);
                }
            }
            if (imZNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imZNNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.zNNeighborID = polyVoxID;
                    Entities.editEntity(imZNNeighborFor, neighborProperties);
                }
            }

            if (imXPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imXPNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.xPNeighborID = polyVoxID;
                    Entities.editEntity(imXPNeighborFor, neighborProperties);
                }
            }
            if (imYPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imYPNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.yPNeighborID = polyVoxID;
                    Entities.editEntity(imYPNeighborFor, neighborProperties);
                }
            }
            if (imZPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imZPNeighborFor);
                if (neighborProperties.dimensions.x == plotSize) {
                    neighborProperties.zPNeighborID = polyVoxID;
                    Entities.editEntity(imZPNeighborFor, neighborProperties);
                }
            }

            // link this plot to its neighbors
            var xNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: -plotSize, y: 0, z: 0}), plotSize);
            var yNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: -plotSize, z: 0}), plotSize);
            var zNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: -plotSize}), plotSize);
            var xPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: plotSize, y: 0, z: 0}), plotSize);
            var yPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: plotSize, z: 0}), plotSize);
            var zPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: plotSize}), plotSize);

            if (xNNeighborID) {
                xNNeighborIDProps = Entities.getEntityProperties(xNNeighborID);
                if (xNNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.xNNeighborID = xNNeighborID;
                }
            }
            if (yNNeighborID) {
                yNNeighborIDProps = Entities.getEntityProperties(yNNeighborID);
                if (yNNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.yNNeighborID = yNNeighborID;
                }
            }
            if (zNNeighborID) {
                zNNeighborIDProps = Entities.getEntityProperties(zNNeighborID);
                if (zNNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.zNNeighborID = zNNeighborID;
                }
            }
            if (xPNeighborID) {
                xPNeighborIDProps = Entities.getEntityProperties(xPNeighborID);
                if (xPNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.xPNeighborID = xPNeighborID;
                }
            }
            if (yPNeighborID) {
                yPNeighborIDProps = Entities.getEntityProperties(yPNeighborID);
                if (yPNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.yPNeighborID = yPNeighborID;
                }
            }
            if (zPNeighborID) {
                zPNeighborIDProps = Entities.getEntityProperties(zPNeighborID);
                if (zPNeighborIDProps.dimensions.x == properties.dimensions.x) {
                    properties.zPNeighborID = zPNeighborID;
                }
            }

            Entities.editEntity(polyVoxID, properties);
        }
    }

    var voxelX = 0;
    var voxelZ = 0;
    var process = function () {
        print("setTerrain: " + voxelX + " " + voxelZ);

        var voxelLocation = {x: voxelX, y: 8, z: voxelZ};
        for (voxelIndex in voxelTerrains) {
            var onResult, offResult;
            var voxelID = voxelTerrains[voxelIndex];

            var worldLocation = Entities.voxelCoordsToWorldCoords(voxelID, voxelLocation);
            var mapLocation = worldCoordsToMapCoords(worldLocation, voxelTerrainMin, voxelTerrainMax);

            worldLocation = mapCoordsToWorldCoords(mapLocation, voxelTerrainMin, voxelTerrainMax);
            var inPolyVoxLocation = Entities.worldCoordsToVoxelCoords(voxelID, worldLocation);
            var voxelY = inPolyVoxLocation.y;

            var onCuboidLow = {x: voxelX, y: 0, z: voxelZ};
            var onCuboidSize = {x: 1, y: voxelY, z: 1};
            clampVector(zeroVec, {x: 1, y: 64, z: 1}, onCuboidSize)

            var offCuboidLow = {x: voxelX, y: voxelY, z: voxelZ};
            var offCuboidSize = {x: 1, y: 64 - voxelY, z: 1};
            clampVector(zeroVec, {x: 1, y: 64, z: 1}, offCuboidSize)

            if (vecHasVolume(onCuboidSize)) {
                onResult = Entities.setVoxelsInCuboid(voxelID, onCuboidLow, onCuboidSize, 255);
            }
            if (vecHasVolume(offCuboidSize)) {
                offResult = Entities.setVoxelsInCuboid(voxelID, offCuboidLow, offCuboidSize, 0);
            }
        }

        if (voxelZ == 15) {
            voxelZ = 0;
            voxelX += 1;
        } else {
            voxelZ += 1;
        }
        if (voxelX == 16) {
            lockTerrain();
            return;
        }

        Script.setTimeout(process, 4000);
    }
    process();
}

Controller.mousePressEvent.connect(mousePressEvent);
// Controller.keyPressEvent.connect(keyPressEvent);
// Controller.keyReleaseEvent.connect(keyReleaseEvent);
Script.scriptEnding.connect(cleanup);
