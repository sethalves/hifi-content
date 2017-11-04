
/* jslint bitwise: true */
/* global Entities, Script */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    var wires = Script.require(Script.resolvePath("wires-shared.js"));
    var wireRegistrationPoints = Script.require(Script.resolvePath("wire-reg-points.js"));

    var wireAdder = genericTool.genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
            this.previousBrushPositionSet = false;
        },
        function() { // continue
            var ironTipProps = Entities.getEntityProperties(this.brush, ["position"]);
            var gridCoords = wires.worldPositionToGridCoordinates(ironTipProps.position);

            if (this.previousGridCoords) {
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

            this.previousGridCoords = gridCoords;
        },
        null); // stop


    wireAdder.addWire = function (gridCoords, segmentIndex) {
        var bitValue = wires.segmentIndexToBitValue[segmentIndex];
        var IDAndOldValue = wires.getGridWireValue(gridCoords);
        var oldID = IDAndOldValue[0];
        var oldValue = IDAndOldValue[1];
        var newValue = oldValue | bitValue;
        if (!oldID) {
            // var newWireID =
            Entities.addEntity({
                name: "wires-" + gridCoords.x + "," + gridCoords.y + "," + gridCoords.z,
                type: "Model",
                modelURL: wires.wireValueToModelURL(newValue),
                position: wires.gridCoordinatesToWorldPosition(gridCoords),
                rotation: { "w": 1, "x": 0, "y": 0, "z": 0 },
                registrationPoint: wireRegistrationPoints[newValue],
                userData: JSON.stringify({
                })
            });
        } else {
            Entities.editEntity(oldID, {
                modelURL: wires.wireValueToModelURL(newValue),
                registrationPoint: wireRegistrationPoints[newValue]
            });
        }
    };


    return wireAdder;
});
