
"use strict";
/* globals $, EventBridge */

function getParameterByName(name, url) {
    if (!url) {
        url = window.location.href;
    }
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}


function addCommandParameters(params) {
    params["brick-width"] = $("#brick-width").val();
    params["brick-height"] = $("#brick-height").val();
    params["brick-length"] = $("#brick-length").val();
    params["max-bricks-per-row"] = $("#max-bricks-per-row").val();
    params["gap"] = $("#gap").val();
    return params;
}

$(document).ready(function() {
    $("#add-brick-button").click(function() {
        EventBridge.emitWebEvent(JSON.stringify(addCommandParameters({ "brick-walls-command": "add-brick" })));
    });
    $("#add-brick-row-button").click(function() {
        EventBridge.emitWebEvent(JSON.stringify(addCommandParameters({ "brick-walls-command": "add-brick-row" })));
    });
    $("#reset-bricks-button").click(function() {
        EventBridge.emitWebEvent(JSON.stringify(addCommandParameters({ "brick-walls-command": "reset-bricks" })));
    });

    var brickWidth = getParameterByName('brick-width');
    var brickHeight = getParameterByName('brick-height');
    var brickLength = getParameterByName('brick-length');
    var maxBricksPerRow = getParameterByName('max-bricks-per-row');
    var gap = getParameterByName('gap');

    $("#brick-width").val(brickWidth);
    $("#brick-height").val(brickHeight);
    $("#brick-length").val(brickLength);
    $("#max-bricks-per-row").val(maxBricksPerRow);
    $("#gap").val(gap);
});
