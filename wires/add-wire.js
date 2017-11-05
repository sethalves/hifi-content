
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
                var changedAxisCount =
                    (gridCoords.x == this.previousGridCoords.x ? 0 : 1) +
                    (gridCoords.y == this.previousGridCoords.y ? 0 : 1) +
                    (gridCoords.z == this.previousGridCoords.z ? 0 : 1);
                if (changedAxisCount == 1) {
                    if (gridCoords.x == this.previousGridCoords.x - 1) {
                        this.addWire(this.previousGridCoords, 2);
                        this.addWire(gridCoords, 0);
                    }
                    if (gridCoords.x == this.previousGridCoords.x + 1) {
                        this.addWire(this.previousGridCoords, 0);
                        this.addWire(gridCoords, 2);
                    }
                    if (gridCoords.y == this.previousGridCoords.y - 1) {
                        this.addWire(this.previousGridCoords, 5);
                        this.addWire(gridCoords, 4);
                    }
                    if (gridCoords.y == this.previousGridCoords.y + 1) {
                        this.addWire(this.previousGridCoords, 4);
                        this.addWire(gridCoords, 5);
                    }
                    if (gridCoords.z == this.previousGridCoords.z - 1) {
                        this.addWire(this.previousGridCoords, 1);
                        this.addWire(gridCoords, 3);
                    }
                    if (gridCoords.z == this.previousGridCoords.z + 1) {
                        this.addWire(this.previousGridCoords, 3);
                        this.addWire(gridCoords, 1);
                    }
                }
            }

            this.previousGridCoords = gridCoords;
        },
        null); // stop


    wireAdder.addWire = function (gridCoords, segmentIndex) {
        var bitValue = wires.segmentIndexToBitValue[segmentIndex];
        var IDAndOldValue = wires.getGridWireValue(gridCoords);
        var oldID = IDAndOldValue[0];
        var oldValue = IDAndOldValue[1];
        var newValue;

        if (oldID && ((oldValue & bitValue) > 0)) {
            // we're deleting connections
            newValue = oldValue & ~bitValue;
        } else {
            // we're adding new connections
            newValue = oldValue | bitValue;
        }

        var positionData = wireRegistrationPoints[newValue];
        var registrationPoint = positionData[0];
        var dimensions = positionData[1];
        var rotation = positionData[2];

        if (oldID && newValue === 0) {
            // this grid point no longer has any connections
            Entities.deleteEntity(oldID);
        } else if (!oldID) {
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
