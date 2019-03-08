"use strict";

/* global Vec3, Quat, Entities, Script, Uuid */

(function() {

    var plotSize = { x: 128, y: 64, z: 128 }; // dimensions of one polyvox entity
    var halfPlotSize = Vec3.multiply(plotSize, 0.5);
    // var worldCenter = { x: 450, y: 1.5 + plotSize.y / 2, z: 100 };
    var worldCenter = { x: 0, y: 0, z: 0 };
    // var voxelVolumeSize = { x: 12, y: 48, z: 12 };
    var voxelVolumeSize = { x: 16, y: 120, z: 16 };
    var lowPlotIndex = { x: -2, y: 0, z: -2 }; // inclusive
    var highPlotIndex = { x: 2, y: 0, z: 2 }; // inclusive
    var funcLow = { x: -5, y: -0.5, z: -5 };
    var funcHigh = { x: 5, y: 0.5, z: 5 };
    var funcSize = Vec3.subtract(funcHigh, funcLow);
    var lifetime = 12000;

    var worldLowCoords = Vec3.sum(Vec3.subtract(Vec3.multiplyVbyV(lowPlotIndex, plotSize), halfPlotSize), worldCenter);
    var worldHighCoords = Vec3.sum(Vec3.sum(Vec3.multiplyVbyV(highPlotIndex, plotSize), halfPlotSize), worldCenter);
    var worldSize = Vec3.subtract(worldHighCoords, worldLowCoords);

    var oneOverWorldSize = { x: 1.0 / worldSize.x, y: 1.0 / worldSize.y, z: 1.0 / worldSize.z };
    var oneOverFuncSize = { x: 1.0 / funcSize.x, y: 1.0 / funcSize.y, z: 1.0 / funcSize.z };

    var polyVoxIDsByIndex = {};

    function heightFunction(worldCoords) {

        if (Vec3.length(worldCoords) < 80) {
            return -5;
        }

        var worldOffset = Vec3.subtract(worldCoords, worldLowCoords);
        var inputRatio = Vec3.multiplyVbyV(worldOffset, oneOverWorldSize);
        var funcOffset = Vec3.multiplyVbyV(funcSize, inputRatio);
        var funcCoords = Vec3.sum(funcLow, funcOffset);

        var funcResult = {
            x: funcCoords.x,
            // y: Math.cos(funcCoords.x) + Math.cos(funcCoords.z),
            y: Math.cos(funcCoords.x) * Math.sin(funcCoords.z - 5) / (funcCoords.x - 5),
            z: funcCoords.z
        };

        var funcResultOffset = Vec3.subtract(funcResult, funcLow);
        var resultRatio = Vec3.multiplyVbyV(funcResultOffset, oneOverFuncSize);
        var worldResultOffset = Vec3.multiplyVbyV(resultRatio, worldSize);
        var worldResultCoords = Vec3.sum(worldResultOffset, worldLowCoords);

        return worldResultCoords.y;
    }


    function setPolyVox(plotIndex, polyVoxID) {
        if (!polyVoxIDsByIndex[plotIndex.x]) {
            polyVoxIDsByIndex[plotIndex.x] = {};
        }
        if (!polyVoxIDsByIndex[plotIndex.x][plotIndex.y]) {
            polyVoxIDsByIndex[plotIndex.x][plotIndex.y] = {};
        }
        polyVoxIDsByIndex[plotIndex.x][plotIndex.y][plotIndex.z] = polyVoxID;
    }


    function getPolyVox(plotIndex) {
        if (!polyVoxIDsByIndex[plotIndex.x]) {
            return Uuid.NULL;
        }
        if (!polyVoxIDsByIndex[plotIndex.x][plotIndex.y]) {
            return Uuid.NULL;
        }
        return polyVoxIDsByIndex[plotIndex.x][plotIndex.y][plotIndex.z];
    }


    function worldPositionToPolyVoxIndex(pos, polyVoxPos, polyVoxRot) {
        var posOffsetFromAether = Vec3.subtract(pos, polyVoxPos);
        var polyVoxToWorldRotation = polyVoxRot;
        var worldToPolyVox = Quat.inverse(polyVoxToWorldRotation);
        var posInAetherFrame = Vec3.multiplyQbyV(worldToPolyVox, posOffsetFromAether);
        return {
            x: Math.floor(posInAetherFrame.x / plotSize.x),
            y: Math.floor(posInAetherFrame.y / plotSize.y),
            z: Math.floor(posInAetherFrame.z / plotSize.z)
        };
    }


    function polyVoxIndextoLocalPosition(voxelIndex) {
        var posInAetherFrame = {
            x: voxelIndex.x * plotSize.x + (plotSize.x / 2.0),
            y: voxelIndex.y * plotSize.y + (plotSize.y / 2.0),
            z: voxelIndex.z * plotSize.z + (plotSize.z / 2.0)
        };
        return posInAetherFrame;
    }


    function forEachPlotIndex(thunk) {
        for (var x = lowPlotIndex.x; x <= highPlotIndex.x; x++) {
            for (var y = lowPlotIndex.y; y <= highPlotIndex.y; y++) {
                for (var z = lowPlotIndex.z; z <= highPlotIndex.z; z++) {
                    thunk({ x: x, y: y, z: z });
                }
            }
        }
    }

    function slowForEachPlotIndex(delayMS, thunk, continuation) {
        var x = lowPlotIndex.x;
        var y = lowPlotIndex.y;
        var z = lowPlotIndex.z;
        var worker = function () {
            if (x > highPlotIndex.x) {
                x = lowPlotIndex.x;
                y++;
            }
            if (y > highPlotIndex.y) {
                y = lowPlotIndex.y;
                z++;
            }
            if (z <= highPlotIndex.z) {
                thunk({ x: x, y: y, z: z });
                x++;
                Script.setTimeout(worker, delayMS);
            } else {
                if (continuation) {
                    continuation();
                }
            }
        };
        worker();
    }


    function forEachXZIndex(thunk) {
        for (var x = 0; x < voxelVolumeSize.x; x++) {
            for (var z = 0; z < voxelVolumeSize.z; z++) {
                thunk({ x: x, y: 0, z: z });
            }
        }
    }


    function slowForEachXZIndex(delayMS, thunk, continuation) {
        var x = 0;
        var z = 0;
        var worker = function () {
            if (x >= voxelVolumeSize.x) {
                x = 0;
                z++;
            }
            if (z < voxelVolumeSize.z) {
                thunk({ x: x, y: 0, z: z });
                x++;
                Script.setTimeout(worker, delayMS);
            } else {
                if (continuation) {
                    continuation();
                }
            }
        };
        worker();
    }


    var createPolyVox = function(plotIndex) {
        var position = Vec3.sum(worldCenter, Vec3.multiplyVbyV(plotIndex, plotSize));

        var plotID = Entities.addEntity({
            type: "PolyVox",
            name: "terrain",
            position: position,
            dimensions: plotSize,
            voxelVolumeSize: voxelVolumeSize,
            voxelSurfaceStyle: 0,
            lifetime: lifetime,
            grab: { grabbable: false },
            xTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg",
            yTextureURL: "http://headache.hungry.com/~seth/hifi/grass.png",
            zTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg"
            // xTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png",
            // yTextureURL: "http://headache.hungry.com/~seth/hifi/green.png",
            // zTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png"
        });
        setPolyVox(plotIndex, plotID);
        Script.setTimeout(function() {
            Entities.setAllVoxels(plotID, 255);
        }, 100);
    };

    var linkPolyVox = function(plotIndex) {
        // link neighbors to this plot
        var plotID = getPolyVox(plotIndex);
        var xNNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: -1, y: 0, z: 0 }));
        var yNNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: 0, y: -1, z: 0 }));
        var zNNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: 0, y: 0, z: -1 }));
        var xPNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: 1, y: 0, z: 0 }));
        var yPNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: 0, y: 1, z: 0 }));
        var zPNeighborID = getPolyVox(Vec3.sum(plotIndex, { x: 0, y: 0, z: 1 }));

        Entities.editEntity(plotID, {
            xNNeighborID: xNNeighborID,
            yNNeighborID: yNNeighborID,
            zNNeighborID: zNNeighborID,
            xPNeighborID: xPNeighborID,
            yPNeighborID: yPNeighborID,
            zPNeighborID: zPNeighborID
        });
    };

    var setHeights = function(voxelIndex) {
        forEachPlotIndex(function(plotIndex) {
            var plotID = getPolyVox(plotIndex);
            var worldCoords = Entities.voxelCoordsToWorldCoords(plotID, voxelIndex);
            worldCoords.y = heightFunction(worldCoords);
            var topFilledVoxelCoord = Entities.worldCoordsToVoxelCoords(plotID, worldCoords);
            if (topFilledVoxelCoord.y < 0) {
                topFilledVoxelCoord.y = 0;
            }
            var emptyHeight = Math.round(voxelVolumeSize.y - topFilledVoxelCoord.y);
            if (emptyHeight > 0) {
                var emptySize = { x: 1, y: emptyHeight, z: 1 };
                Entities.setVoxelsInCuboid(plotID, topFilledVoxelCoord, emptySize, 0);
            }
        });
    };

    slowForEachPlotIndex(600, createPolyVox,
                         // continue...
                         function() {
                             slowForEachPlotIndex(600, linkPolyVox,
                                                  // continue...
                                                  function () {
                                                      slowForEachXZIndex(300, setHeights);
                                                  });
                         });

}());
