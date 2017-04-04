"use strict";
/* globals $, EventBridge */

$(document).ready(function() {
    // hook all buttons to EventBridge
    $(":button").each(function(index) {
        $(this).click(function() {
            EventBridge.emitWebEvent(JSON.stringify({ "dynamic-load-js-test-command": this.id }));
        });
    });
});
