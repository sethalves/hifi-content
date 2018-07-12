
/* global textureIndexToURLs, paintBucketColors, PALETTE_COLORS, TEXTURE_PATH, VOXEL_MANIPULATOR_LIFETIME */

DEFAULT_COLOR_DIMENSIONS = {
    x: 0.083504512906074524,
    y: 0.024401858448982239,
    z: 0.096756651997566223
};

TEXTURE_PATH = Script.resolvePath('textures') + '/';

PALETTE_COLORS = [
    {
        name: 'black',
        color: { red: 0, green: 0, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-0-0-0.png',
        position: {
            x: -0.061158061027526855,
            y: 0.00213623046875,
            z: 0.056712150573730469
        }
    },
    {
        name: 'blue',
        color: { red: 0, green: 0, blue: 255 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-0-0-ff.png',
        position: {
            x: -0.064543604850769043,
            y: 0.00213623046875,
            z: -0.075875282287597656
        }
    },
    {
        name: 'green',
        color: { red: 0, green: 255, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-0-ff-0.png',
        position: {
            x: -0.22693264484405518,
            y: 0.00213623046875,
            z: -0.063619613647460938
        }
    },
    {
        name: 'cyan',
        color: { red: 0, green: 255, blue: 255 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-0-ff-ff.png',
        position: {
            x: 0.023909330368041992,
            y: 0.00213623046875,
            z: -0.10207223892211914
        }
    },
    {
        name: 'red',
        color: { red: 255, green: 0, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-ff-0-0.png',
        position: {
            x: 0.030942916870117188,
            y: 0.00213623046875,
            z: -0.20352029800415039
        }
    },
    {
        name: 'magenta',
        color: { red: 255, green: 0, blue: 255 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-ff-0-ff.png',
        position: {
            x: -0.1491849422454834,
            y: 0.00213623046875,
            z: 0.00017118453979492188
        }
    },
    {
        name: 'yellow',
        color: { red: 255, green: 255, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-ff-ff-0.png',
        position: {
            x: -0.15239584445953369,
            y: 0.00213623046875,
            z: -0.14830589294433594
        }
    },
    {
        name: 'white',
        color: { red: 255, green: 255, blue: 255 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-ff-ff-ff.png',
        position: {
            x: 0.027637481689453125,
            y: 0.00213623046875,
            z: -0.0022187232971191406
        }
    },
    {
        name: 'orange',
        color: { red: 255, green: 165, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'color-ff-a5-0.png',
        position: {
            x: -0.064569234848022461,
            y: 0.00213623046875,
            z: -0.19738531112670898
        }
    },
    {
        name: 'wood',
        color: { red: 153, green: 76, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: TEXTURE_PATH + 'wood.png',
        position: {
            x: -0.17113709449768066,
            y: 0.00213623046875,
            z: 0.10360097885131836
        }
    },
    {
        name: 'grass and dirt',
        color: { red: 0, green: 76, blue: 0 },
        dimensions: DEFAULT_COLOR_DIMENSIONS,
        textures: [
            TEXTURE_PATH + 'dirt.png',
            TEXTURE_PATH + 'grass.png?v=2',
            TEXTURE_PATH + 'dirt.png'
        ],
        position: {
            x: -0.25371217727661133,
            y: 0.00213623046875,
            z: 0.03507232666015625
        }
    }
];
