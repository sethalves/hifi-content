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
    // TODO rewrite with RegExp
    if (!modelURL) {
        print("extractWireValueFromModelURL -- " + modelURL + " --> null 0");
        return null;
    }
    var modelURLSplit = modelURL.split("/");
    if (modelURLSplit.length < 2) {
        print("extractWireValueFromModelURL -- " + modelURL + " --> null 1");
        return null;
    }
    var modelURLFilename = modelURLSplit[ modelURLSplit.length - 1 ];
    if (!modelURLFilename.startsWith("wires-")) {
        print("extractWireValueFromModelURL -- " + modelURL + " --> null 2");
        return null;
    }
    var modelURLLastPart = modelURLFilename.slice(6);
    var modelURLLastPartSplit = modelURLLastPart.split(".");
    if (modelURLLastPartSplit.length != 2 || modelURLLastPartSplit[1] != 'obj') {
        print("extractWireValueFromModelURL -- " + modelURL + " --> null 3");
        return null;
    }
    print("extractWireValueFromModelURL -- " + modelURL + " --> " + parseInt(modelURLLastPartSplit[0]));
    return parseInt(modelURLLastPartSplit[0]);
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
        var possibleWireProps = Entities.getEntityProperties(possibleWireID, ['name', 'position', 'modelURL']);
        if (possibleWireProps.name.startsWith('wires-') &&
            Vec3.distance(possibleWireProps.position, worldPos) < 0.001) {
            var wireValue = extractWireValueFromModelURL(possibleWireProps.modelURL);
            if (wireValue) {
                return [possibleWireID, wireValue];
            }
        } else {
            print("HERE distance = " + Vec3.distance(possibleWireProps.position, worldPos));
        }
    }

    return [null, 0];
};


if (typeof module !== 'undefined') {
    module.exports = {
        gridSize: gridSize,
        wireValueToModelURL: wireValueToModelURL,
        segmentIndexToBitValue: segmentIndexToBitValue,
        worldPositionToGridCoordinates: worldPositionToGridCoordinates,
        gridCoordinatesToWorldPosition: gridCoordinatesToWorldPosition,
        getGridWireValue: getGridWireValue
    };
}
