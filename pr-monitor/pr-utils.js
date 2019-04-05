"use strict";

/* global module */


function getRateLimit() {

    var url = "https://api.github.com/rate_limit";
    try {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if (request.readyState === request.DONE) {
                var data = JSON.parse(request.responseText);

                var d = new Date();
                var seconds = d.getTime() / 1000;

                print("Github Rate Limiting: " +
                      data.resources.core.remaining + " / " + data.resources.core.limit + ", " +
                      (data.resources.core.reset - seconds) + " seconds");
                // "reset = " + data.resources.core.reset + ", " + "seconds = " + seconds);

            }
        };
        request.onerror = function () {
            print("pr-utils.js -- getPRDetails failed: " + request.statusText);
        };
        request.ontimeout = function () {
            print("pr-utils.js -- getPRDetails timed-out: " + request.statusText);
        };
        request.setRequestHeader("Accept", "application/vnd.github.antiope-preview+json");
        request.setRequestHeader("User-Agent", "High-Fidelity-PR-Watcher");
        request.open("GET", url, true);
        request.timeout = 20000;
        request.send();
    } catch (err) {
        print("pr-utils.js -- error loading " + url);
    }
}


function cachingGithubAPIRequest(url, dataCache, thunk) {
    var request = new XMLHttpRequest();
    request.onreadystatechange = function() {
        if (request.readyState === request.DONE) {
            if (request.status === 200) {
                print("QQQQ 200 for cachingGithubAPIRequest: " + url);
                var etag = request.getResponseHeader("ETag");
                dataCache[url] = { etag: etag, data: request.responseText }; // save in cache
                thunk(JSON.parse(request.responseText));
            } else if (request.status == 304) {
                print("QQQQ 304 for cachingGithubAPIRequest: " + url);
                thunk(JSON.parse(dataCache[url].data)); // get from cache
            } else {
                print("pr-utils.js -- cachingGithubAPIRequest failed: " +
                      request.status + " " + request.statusText + ", " + url);
            }
        }
    };

    request.setRequestHeader("Accept", "application/vnd.github.antiope-preview+json");
    request.setRequestHeader("User-Agent", "High-Fidelity-PR-Watcher");

    if (dataCache[url]) {
        var prevEtag = dataCache[url].etag;
        request.setRequestHeader("If-None-Match", prevEtag);
    }

    request.onerror = function () {
        print("pr-utils.js -- cachingGithubAPIRequest failed: " + request.statusText);
        thunk(null);
    };
    request.ontimeout = function () {
        print("pr-utils.js -- cachingGithubAPIRequest timed-out: " + request.statusText);
        thunk(null);
    };
    request.open("GET", url);
    request.timeout = 20000;
    request.send();
}



function newestStatusPerContext(statusesResponse) {
    // only keep the newest status for a given context
    var checks = {};
    for (var i = 0; i < statusesResponse.length; i++) {
        var status = statusesResponse[i];
        if (checks[status.context]) {
            if (status.id > checks[status.context].id) {
                checks[status.context] = status;
            }
        } else {
            checks[status.context] = status;
        }
    }

    var finalChecks = [];
    for (var context in checks) {
        if (checks.hasOwnProperty(context)) {
            finalChecks.push(checks[context]);
        }
    }

    return finalChecks;
}


function getPRStatuses(detailsResponse, dataCache, thunk) {
    var url = detailsResponse.statuses_url;
    try {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if (request.readyState === request.DONE) {
                var statusesResponse;
                if (request.status === 200) {

                    print("QQQQ getPRStatuses got 200");

                    // save in cache
                    var etag = request.getResponseHeader("ETag");
                    dataCache[url] = { etag: etag, data: request.responseText };

                    statusesResponse = JSON.parse(request.responseText);
                    detailsResponse.statuses = newestStatusPerContext(statusesResponse);
                    thunk(detailsResponse);
                } else if (request.status == 304) {

                    print("QQQQ getPRStatuses got 304");

                    // get from cache
                    statusesResponse = JSON.parse(dataCache[url].data);
                    detailsResponse.statuses = newestStatusPerContext(statusesResponse);
                    thunk(detailsResponse);
                } else {
                    print("pr-utils.js -- getPRStatuses failed: " + request.status + " " + request.statusText + ", " + url);
                }
            }
        };

        request.setRequestHeader("Accept", "application/vnd.github.antiope-preview+json");
        request.setRequestHeader("User-Agent", "High-Fidelity-PR-Watcher");

        if (dataCache[url]) {
            var prevEtag = dataCache[url].etag;
            request.setRequestHeader("If-None-Match", prevEtag);
        }

        request.onerror = function () {
            print("pr-utils.js -- getPRStatuses failed: " + request.statusText);
        };
        request.ontimeout = function () {
            print("pr-utils.js -- getPRStatuses timed-out: " + request.statusText);
        };
        request.open("GET", url);
        request.timeout = 20000;
        request.send();
    } catch (err) {
        print("pr-utils.js -- error loading " + url);
    }
}

function getLabelNames(detailsResponse) {
    var labelNames = [];
    detailsResponse.labels.forEach(function (label) {
        labelNames.push(label.name);
    });
    return labelNames;
}


function getPRDetails(prNumber, dataCache, thunk) {

    getRateLimit(); // XXX


    var url = "https://api.github.com/repos/highfidelity/hifi/pulls/" + prNumber;
    try {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if (request.readyState === request.DONE) {
                var detailsResponse;
                if (request.status === 200) {

                    print("QQQQ getPRDetails got 200");

                    var etag = request.getResponseHeader("ETag");
                    dataCache[url] = { etag: etag, data: request.responseText };

                    detailsResponse = JSON.parse(request.responseText);
                    detailsResponse.labelNames = getLabelNames(detailsResponse);
                    getPRStatuses(detailsResponse, dataCache, thunk);
                }  else if (request.status == 304) {

                    print("QQQQ getPRDetails got 304");

                    // get from cache
                    detailsResponse = JSON.parse(dataCache[url].data);
                    detailsResponse.labelNames = getLabelNames(detailsResponse);
                    getPRStatuses(detailsResponse, dataCache, thunk);
                } else {
                    print("pr-utils.js -- getPRDetails failed: " + request.status + " " + request.statusText + ", " + url);
                }
            }
        };
        request.onerror = function () {
            print("pr-utils.js -- getPRDetails failed: " + request.statusText);
        };
        request.ontimeout = function () {
            print("pr-utils.js -- getPRDetails timed-out: " + request.statusText);
        };
        request.setRequestHeader("Accept", "application/vnd.github.antiope-preview+json");
        request.setRequestHeader("User-Agent", "High-Fidelity-PR-Watcher");

        if (dataCache[url]) {
            var prevEtag = dataCache[url].etag;
            request.setRequestHeader("If-None-Match", prevEtag);
        }

        request.open("GET", url, true);
        request.timeout = 20000;
        request.send();
    } catch (err) {
        print("pr-utils.js -- error loading " + url);
    }
}


module.exports = {
    getRateLimit: getRateLimit,
    getPRDetails: getPRDetails
};
