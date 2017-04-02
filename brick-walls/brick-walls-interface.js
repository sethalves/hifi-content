"use strict";
/* globals $, EventBridge */

var parameterNames = ["brick-width", "brick-height", "brick-length", "max-bricks-per-row", "gap"];

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


function getParameterType(parameterName) {
    if (parameterName == "max-bricks-per-row") {
        return "integer";
    }
    return "float";
}


function addCommandParameters(params) {
    // copy from html elements into an associative-array which will get passed (as JSON) through the EventBridge
    parameterNames.forEach(function(parameterName) {
        var parameterType = getParameterType(parameterName);
        var strVal = $("#" + parameterName).val();
        if (parameterType == "integer") {
            params[parameterName] = parseInt(strVal);
        } else if (parameterType == "float") {
            params[parameterName] = parseFloat(strVal);
        } else {
            params[parameterName] = strVal;
        }
    });
    return params;
}


$(document).ready(function() {
    // hook all buttons to EventBridge
    $(":button").each(function(index) {
        $(this).click(function() {
            EventBridge.emitWebEvent(JSON.stringify(addCommandParameters({ "brick-walls-command": this.id })));
        });
    });

    // copy parameters from query-args into elements
    parameterNames.forEach(function(parameterName) {
        var val = getParameterByName(parameterName);
        if (val) {
            var parameterType = getParameterType(parameterName);
            if (parameterType == "integer") {
                val = parseInt(val);
            } else if (parameterType == "float") {
                val = parseFloat(val);
            }
            $("#" + parameterName).val(val.toString());
        }
    });
});
