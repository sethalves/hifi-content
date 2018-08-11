#!/usr/bin/nodejs

var fs = require('fs');

// var argv = require('minimist')(process.argv.slice(2));

// before -- 110.3 1.5 72.5
//  after -- 0, -5, 0


var file = fs.readFileSync('models.json', 'utf8');
var models = JSON.parse(file);
var entities = models.Entities;

output = {
    Version: 92,
    Entities: []
};
for (var i = 0; i < entities.length; i++) {
    var props = entities[i];
    if (props.locked &&
        props.id != "{df662b84-8273-444a-bd14-b7dca86ef3ab}" &&
        props.id != "{b75cd8c5-5203-4fc6-a1de-26173c789d8b}" &&
        props.id != "{b5151e2f-bd84-44f5-a9b9-84fc542007a3}" &&
        props.name != "Alan water" &&
        props.name != "Trees Skybox" &&
        props.name != "Moon" // &&
        // props.hasOwnProperty("position")
       ) {
        // console.log(JSON.stringify(props, null, 2));
        if (! props.parentID) {
            props.position.x += -110.3;
            props.position.y += -6.4356;
            props.position.z += -72.5;
            delete props.position.red;
            delete props.position.green;
            delete props.position.blue;
        }

        // XXX
        if (props.hasOwnProperty("position") || props.hasOwnProperty("localPosition")) {
            continue;
        }
        props.localPosition = { x: 0, y: 0, z: 0 };

        // props.lifetime = 120;
        // delete props.locked;

        delete props.queryAACube;
        output.Entities.push(props);
    }
}


function sizeCompare(sortPropsA, sortPropsB) {
    var largestDimA = sortPropsA.dimensions.x;
    largestDimA = sortPropsA.dimensions.y > largestDimA ? sortPropsA.dimensions.y : largestDimA;
    largestDimA = sortPropsA.dimensions.z > largestDimA ? sortPropsA.dimensions.z : largestDimA;

    var largestDimB = sortPropsB.dimensions.x;
    largestDimB = sortPropsB.dimensions.y > largestDimB ? sortPropsB.dimensions.y : largestDimB;
    largestDimB = sortPropsB.dimensions.z > largestDimB ? sortPropsB.dimensions.z : largestDimB;

    return largestDimB - largestDimA;
}


function distanceCompare(sortPropsA, sortPropsB) {
    var furthestPosA = sortPropsA.position.x;
    furthestPosA = sortPropsA.position.y > furthestPosA ? sortPropsA.position.y : furthestPosA;
    furthestPosA = sortPropsA.position.z > furthestPosA ? sortPropsA.position.z : furthestPosA;

    var furthestPosB = sortPropsB.position.x;
    furthestPosB = sortPropsB.position.y > furthestPosB ? sortPropsB.position.y : furthestPosB;
    furthestPosB = sortPropsB.position.z > furthestPosB ? sortPropsB.position.z : furthestPosB;

    return furthestPosB - furthestPosA;
}


output.Entities.sort(distanceCompare);

console.log(JSON.stringify(output, null, 2));
