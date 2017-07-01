(function() { // BEGIN LOCAL_SCOPE

    var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.1, z: -4 }));

    clockID = Entities.addEntity({
        name: 'clock',
        type: 'Model',
        modelURL: Script.resolvePath('clock.obj'),
        position: pos,
        // script: 'http://headache.hungry.com/~seth/hifi/50s-rocket.js',
        userData: JSON.stringify({
            "grabbableKey":{"grabbable":false}, "soundKey":{"url":"http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav","volume":0.4,"loop":true,"playbackGap":0,"playbackGapRange":0}
        }),
    });

    hourHandID = Entities.addEntity({
        name: 'clock hour hand',
        type: 'Model',
        modelURL: Script.resolvePath('hour-hand.obj'),
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: -0.2, z: 0 },
        parentID: clockID
    });

    minuteHandID = Entities.addEntity({
        name: 'clock minute hand',
        type: 'Model',
        modelURL: Script.resolvePath('minute-hand.obj'),
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: -0.2, z: 0 },
        parentID: clockID
    });

}()); // END LOCAL_SCOPE
