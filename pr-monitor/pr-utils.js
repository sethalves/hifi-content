"use strict";

/* global module */

function getPRDetails(prNumber, thunk) {
    try {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if (request.readyState === request.DONE && request.status === 200) {
                var response = JSON.parse(request.responseText);

                var labelNames = [];
                response.labels.forEach(function (label) {
                    labelNames.push(label.name);
                });
                response.labelNames = labelNames;

                thunk(response);
            }
        };

        request.open("GET", "https://api.github.com/repos/highfidelity/hifi/pulls/" + prNumber);
        request.timeout = 10000;
        request.send();
    } catch (err) {
    }
}


module.exports = {
    getPRDetails: getPRDetails
};
