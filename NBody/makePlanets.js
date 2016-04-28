//  makePlanets.js
//
//  Created by Philip Rosedale on March 29, 2016
//  Copyright 2016 High Fidelity, Inc.
//  
//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html
//
//  Make an earth and moon, where you can grab and throw moon into orbit.  Entity 
//  script attached to moon gives it gravitation behavior and will also make it attracted to 
//  other spheres placed nearby.
//   

var SCALE = 3.0;
var EARTH_SIZE = 3.959 / SCALE;
var MOON_SIZE = 1.079 / SCALE;

var BUTTON_SIZE = 32;
var PADDING = 3;

// TODO -- use Controller.getRecommendedOverlayRect()
var screenSize = Controller.getViewportDimensions();

var earth = null;
var moon = null;

var SCRIPT_URL = Script.resolvePath("gravity.js");

HIFI_PUBLIC_BUCKET = "http://s3.amazonaws.com/hifi-public/";
Script.include(["/~/system/libraries/toolBars.js"]);
var toolBar = new ToolBar(0, 0, ToolBar.HORIZONTAL, "highfidelity.makePlanets.js", function(screenSize) {
    return {
        x: (screenSize.x / 2 - BUTTON_SIZE * 2 + PADDING),
        y: (screenSize.y - (BUTTON_SIZE + PADDING))
    };
});

// var diceIconURL = "https://s3-us-west-1.amazonaws.com/hifi-content/eric/images/dice.png"
var diceIconURL = "http://headache.hungry.com/~seth/hifi/NBody/gravity.svg"
var button = toolBar.addOverlay("image", {
    x: screenSize.x / 2 + PADDING,
    y: screenSize.y - (BUTTON_SIZE + PADDING),
    width: BUTTON_SIZE,
    height: BUTTON_SIZE,
    imageURL: diceIconURL,
    color: {
        red: 255,
        green: 255,
        blue: 255
    },
    alpha: 1
});

var deleteButton = toolBar.addOverlay("image", {
    x: screenSize.x / 2 - BUTTON_SIZE,
    y: screenSize.y - (BUTTON_SIZE + PADDING),
    width: BUTTON_SIZE,
    height: BUTTON_SIZE,
    imageURL: HIFI_PUBLIC_BUCKET + "images/delete.png",
    color: {
        red: 255,
        green: 255,
        blue: 255
    },
    alpha: 1
});

function inFrontOfMe(distance) {
    return Vec3.sum(Camera.getPosition(), Vec3.multiply(distance, Quat.getFront(Camera.getOrientation())));
}

function onButtonClick() {
    earth = Entities.addEntity({
        type: "Model",
	name: "Earth",
        modelURL: "http://headache.hungry.com/~seth/hifi/NBody/earth.fbx",
	position: inFrontOfMe(2 * EARTH_SIZE),
	dimensions: { x: EARTH_SIZE, y: EARTH_SIZE, z: EARTH_SIZE },
        shapeType: "sphere"
    });
    moon = Entities.addEntity({
        type: "Model",
	name: "Moon",
        modelURL: "http://headache.hungry.com/~seth/hifi/NBody/moon.fbx",
	position: inFrontOfMe(EARTH_SIZE - MOON_SIZE),
	dimensions: { x: MOON_SIZE, y: MOON_SIZE, z: MOON_SIZE },
	dynamic: true,
	damping: 0.01,
	angularDamping: 0.01,
	script: SCRIPT_URL,
        shapeType: "sphere"
    });
}

function onDeleteButton() {
	Entities.deleteEntity(earth);
	Entities.deleteEntity(moon);
}

function mousePressEvent(event) {
  var clickedText = false;
  var clickedOverlay = Overlays.getOverlayAtPoint({
    x: event.x,
    y: event.y
  });
  if (clickedOverlay == button) {
    onButtonClick();
  } else if (clickedOverlay == deleteButton) {
  	onDeleteButton();
  }
}

function scriptEnding() {
  toolBar.cleanup();
}

Controller.mousePressEvent.connect(mousePressEvent);
Script.scriptEnding.connect(scriptEnding);
