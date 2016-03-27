

Script.include([
    "voxel-ground-utils.js"
]);

var center = {x: 0, y: -16, z: 0};
var zeroVec = {x: 0, y: 0, z: 0};

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
        deleteTerrain();
        // addTerrain();
        // setTerrain();
    }
}

function cleanup() {
    toolBar.cleanup();
}

function worldCoordsToMapCoords(worldCoords) {
    var offset = Vec3.subtract(worldCoords, center);
    var vec = Vec3.multiply(offset, 1.0 / 16.0);
    return {
        x: vec.x,
        y: Math.sin(vec.x) * Math.cos(vec.z) * (16.0 / 64.0),
        z: vec.z
    };
}
function mapCoordsToWorldCoords(mapCoords) {
    var vec = Vec3.multiply(mapCoords, 16.0);
    var blah = Vec3.sum(vec, center);
    return blah;
}

function deleteTerrain() {
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var props = Entities.getEntityProperties(polyVoxID, ["name", "position"]);
        if (props.name == "terrain") {
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
            x: (voxelX - 2) * getPlotSize(),
            y: 0,
            z: (voxelZ - 2) * getPlotSize()
        };

        addTerrainAtPosition(Vec3.sum(center, position));

        if (voxelZ == 5) {
            voxelZ = 0;
            voxelX += 1;
        } else {
            voxelZ += 1;
        }
        if (voxelX == 5) {
            voxelX = 0;
            voxelZ = 0;
            setTerrain();
            return;
        }

        Script.setTimeout(worker, 100);
    }
    worker();
}


function setTerrain() {
    var voxelTerrains = [];
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var properties = Entities.getEntityProperties(polyVoxID);
        if (properties.name == "terrain") {
            // XXX and it's a polyvox with the correct dimensions?
            voxelTerrains.push(polyVoxID);

            var plotSize = getPlotSize();

            // link neighbors to this plot
            var imXNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: plotSize, y: 0, z: 0}));
            var imYNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: plotSize, z: 0}));
            var imZNNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: plotSize}));
            var imXPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: -plotSize, y: 0, z: 0}));
            var imYPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: -plotSize, z: 0}));
            var imZPNeighborFor = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: -plotSize}));
            var neighborProperties

            if (imXNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imXNNeighborFor);
                neighborProperties.xNNeighborID = polyVoxID;
                Entities.editEntity(imXNNeighborFor, neighborProperties);
            }
            if (imYNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imYNNeighborFor);
                neighborProperties.yNNeighborID = polyVoxID;
                Entities.editEntity(imYNNeighborFor, neighborProperties);
            }
            if (imZNNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imZNNeighborFor);
                neighborProperties.zNNeighborID = polyVoxID;
                Entities.editEntity(imZNNeighborFor, neighborProperties);
            }

            if (imXPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imXPNeighborFor);
                neighborProperties.xPNeighborID = polyVoxID;
                Entities.editEntity(imXPNeighborFor, neighborProperties);
            }
            if (imYPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imYPNeighborFor);
                neighborProperties.yPNeighborID = polyVoxID;
                Entities.editEntity(imYPNeighborFor, neighborProperties);
            }
            if (imZPNeighborFor) {
                neighborProperties = Entities.getEntityProperties(imZPNeighborFor);
                neighborProperties.zPNeighborID = polyVoxID;
                Entities.editEntity(imZPNeighborFor, neighborProperties);
            }

            // link this plot to its neighbors
            properties.xNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: -plotSize, y: 0, z: 0}));
            properties.yNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: -plotSize, z: 0}));
            properties.zNNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: -plotSize}));
            properties.xPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: plotSize, y: 0, z: 0}));
            properties.yPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: plotSize, z: 0}));
            properties.zPNeighborID = lookupTerrainForLocation(Vec3.sum(properties.position, {x: 0, y: 0, z: plotSize}));
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
            var mapLocation = worldCoordsToMapCoords(worldLocation);

            worldLocation = mapCoordsToWorldCoords(mapLocation);
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
            return;
        }

        Script.setTimeout(process, 600);
    }
    process();
}


Controller.mousePressEvent.connect(mousePressEvent);
// Controller.keyPressEvent.connect(keyPressEvent);
// Controller.keyReleaseEvent.connect(keyReleaseEvent);
Script.scriptEnding.connect(cleanup);
