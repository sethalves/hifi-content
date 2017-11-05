
/* jslint bitwise: true */
/* global Entities, Script */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    var wires = Script.require(Script.resolvePath("wires-shared.js"));
    var wireRegistrationPoints = Script.require(Script.resolvePath("wire-reg-points.js"));

    var wireAdder = genericTool.genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
            this.previousGridCoords = null;
        },
        function() { // continue
            var ironTipProps = Entities.getEntityProperties(this.brush, ["position"]);
            var gridCoords = wires.worldPositionToGridCoordinates(ironTipProps.position);

            if (this.previousGridCoords) {
                if (gridCoords.x == this.previousGridCoords.x - 1) {
                    this.addWire(this.previousGridCoords, 2, gridCoords, 0);
                }
                if (gridCoords.x == this.previousGridCoords.x + 1) {
                    this.addWire(this.previousGridCoords, 0, gridCoords, 2);
                }
                if (gridCoords.y == this.previousGridCoords.y - 1) {
                    this.addWire(this.previousGridCoords, 5, gridCoords, 4);
                }
                if (gridCoords.y == this.previousGridCoords.y + 1) {
                    this.addWire(this.previousGridCoords, 4, gridCoords, 5);
                }
                if (gridCoords.z == this.previousGridCoords.z - 1) {
                    this.addWire(this.previousGridCoords, 1, gridCoords, 3);
                }
                if (gridCoords.z == this.previousGridCoords.z + 1) {
                    this.addWire(this.previousGridCoords, 3, gridCoords, 1);
                }
            }

            this.previousGridCoords = gridCoords;
        },
        null); // stop


    wireAdder.addWire = function (previousGridCoords, previousSegmentIndex, gridCoords, segmentIndex) {
        // gather data about where the brush was before
        var previousBitValue = wires.segmentIndexToBitValue[previousSegmentIndex];
        var previousIDAndOldValue = wires.getGridWireValue(previousGridCoords);
        var previousOldID = previousIDAndOldValue[0];
        var previousOldValue = previousIDAndOldValue[1];
        var previousNewValue;

        // gather data about where the brush is now
        var bitValue = wires.segmentIndexToBitValue[segmentIndex];
        var IDAndOldValue = wires.getGridWireValue(gridCoords);
        var oldID = IDAndOldValue[0];
        var oldValue = IDAndOldValue[1];
        var newValue;

        if ((previousOldValue & previousBitValue > 0) && (oldValue & bitValue > 0)) {
            // this wire segment already existed, so we're erasing
            print("erase");
            previousNewValue = previousOldValue & ~previousBitValue;
            newValue = oldValue & ~bitValue;
        } else {
            // we're adding
            previousNewValue = previousOldValue | previousBitValue;
            newValue = oldValue | bitValue;
        }

        var previousPositionData = wireRegistrationPoints[previousNewValue];
        var previousRegistrationPoint = previousPositionData[0];
        var previousDimensions = previousPositionData[1];
        var previousRotation = previousPositionData[2];

        var positionData = wireRegistrationPoints[newValue];
        var registrationPoint = positionData[0];
        var dimensions = positionData[1];
        var rotation = positionData[2];

        if (!previousOldID) {
            // var newWireID =
            Entities.addEntity({
                name: "wires-" + previousGridCoords.x + "," + previousGridCoords.y + "," + previousGridCoords.z,
                type: "Model",
                modelURL: wires.wireValueToModelURL(previousNewValue),
                position: wires.gridCoordinatesToWorldPosition(previousGridCoords),
                dimensions: previousDimensions,
                rotation: previousRotation,
                registrationPoint: previousRegistrationPoint,
                userData: JSON.stringify({
                })
            });
        } else {
            Entities.editEntity(previousOldID, {
                modelURL: wires.wireValueToModelURL(previousNewValue),
                dimensions: previousDimensions,
                rotation: previousRotation,
                registrationPoint: previousRegistrationPoint
            });
        }

        if (!oldID) {
            // var newWireID =
            Entities.addEntity({
                name: "wires-" + gridCoords.x + "," + gridCoords.y + "," + gridCoords.z,
                type: "Model",
                modelURL: wires.wireValueToModelURL(newValue),
                position: wires.gridCoordinatesToWorldPosition(gridCoords),
                dimensions: dimensions,
                rotation: rotation,
                registrationPoint: registrationPoint,
                userData: JSON.stringify({
                })
            });
        } else {
            Entities.editEntity(oldID, {
                modelURL: wires.wireValueToModelURL(newValue),
                dimensions: dimensions,
                rotation: rotation,
                registrationPoint: registrationPoint
            });
        }
    };


    return wireAdder;
});
