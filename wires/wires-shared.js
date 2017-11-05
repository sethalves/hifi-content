"use strict";


/* global
   Script,
   module,
   Entities,
   Vec3,
   gridSize:true,
   wireValueToModelURL:true,
   segmentIndexToBitValue:true,
   worldPositionToGridCoordinates:true,
   gridCoordinatesToWorldPosition:true,
   getGridWireValue:true
*/

gridSize = 0.1; // should match grid_size in wires.scad


segmentIndexToBitValue = [1, 2, 4, 8, 16, 32];


function extractWireValueFromModelURL(modelURL) {
    var re = new RegExp(".*/models/wires-([0-9]+).obj$");
    var reResult = re.exec(modelURL);
    if (reResult) {
        return reResult[1];
    }
    return null;
}


wireValueToModelURL = function(wireValue) {
    return Script.resolvePath("models/wires-" + wireValue + ".obj");
};


worldPositionToGridCoordinates = function(worldPosition) {
    return {
        x: Math.floor(worldPosition.x / gridSize),
        y: Math.floor(worldPosition.y / gridSize),
        z: Math.floor(worldPosition.z / gridSize)
    };
};


gridCoordinatesToWorldPosition = function(gridCoords) {
    return {
        x: gridCoords.x * gridSize + gridSize / 2,
        y: gridCoords.y * gridSize + gridSize / 2,
        z: gridCoords.z * gridSize + gridSize / 2
    };
};


getGridWireValue = function (gridCoords) {
    var worldPos = gridCoordinatesToWorldPosition(gridCoords);
    var possibleWireIDs = Entities.findEntities(worldPos, gridSize);
    for (var j = 0; j < possibleWireIDs.length; j++) {
        var possibleWireID = possibleWireIDs[j];
        var possibleWireProps = Entities.getEntityProperties(possibleWireID, ["name", "position", "modelURL"]);
        if (possibleWireProps &&
            Vec3.distance(possibleWireProps.position, worldPos) < 0.001) {
            var wireValue = extractWireValueFromModelURL(possibleWireProps.modelURL);
            if (wireValue) {
                return [possibleWireID, wireValue];
            }
        }
    }

    return [null, 0];
};


if (typeof module !== "undefined") {
    module.exports = {
        gridSize: gridSize,
        wireValueToModelURL: wireValueToModelURL,
        segmentIndexToBitValue: segmentIndexToBitValue,
        worldPositionToGridCoordinates: worldPositionToGridCoordinates,
        gridCoordinatesToWorldPosition: gridCoordinatesToWorldPosition,
        getGridWireValue: getGridWireValue
    };
}
