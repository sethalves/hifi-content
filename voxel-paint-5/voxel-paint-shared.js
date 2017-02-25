
/* global textureIndexToURLs, paintBucketColors, PALETTE_COLORS, VOXEL_TOOLS PALETTE_LIFETIME, TEXTURE_PATH, VOXEL_MANIPULATOR_LIFETIME */

DEFAULT_COLOR_DIMENSIONS = {
    x: 0.083504512906074524,
    y: 0.024401858448982239,
    z: 0.096756651997566223
};

TEXTURE_PATH = Script.resolvePath('textures') + '/';
MODELS_PATH = Script.resolvePath('models') + '/';

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

VOXEL_TOOLS = [
    {
        name: 'smallBrush',
        type: 'brush',
        toolRadius: 0.025,
        toolLength: 0.275,
        properties: {
            dimensions: {
                x: 0.012468120083212852,
                y: 0.60412204265594482,
                z: 0.012788690626621246
            },
            modelURL: MODELS_PATH + "smallBrush.fbx"
        },
        paletteProperties: {
            position: {
                x: 0.097681999206542969,
                y: 0.0062408447265625,
                z: 0.10758876800537109
            },
            leftRotation: {"x":0.6448094844818115,"y":0.6511309146881104,"z":-0.26214462518692017,"w":-0.3025384545326233},
            rightRotation: {
                w: -0.63289844989776611,
                x: 0.31538867950439453,
                y: 0.31548023223876953,
                z: -0.63280689716339111
            },
            dimensions: {
                x: 0.0031751182395964861,
                y: 0.15384508669376373,
                z: 0.0032567542511969805
            }
        }
    },
    {
        name: 'medBrush',
        type: 'brush',
        toolRadius: 0.08,
        toolLength: 0.3,
        properties: {
            dimensions: {
                x: 0.057361073791980743,
                y: 0.65550220012664795,
                z: 0.054657310247421265
            },
            modelURL: MODELS_PATH + "medBrush.fbx"
        },
        paletteProperties: {
            position: {
                x: 0.041584968566894531,
                y: 0.0052490234375,
                z: 0.13003635406494141
            },
            leftRotation: {"x":0.6448094844818115,"y":0.6511309146881104,"z":-0.26214462518692017,"w":-0.3025384545326233},
            rightRotation: {
                w: -0.64486151933670044,
                x: 0.33617150783538818,
                y: 0.31566333770751953,
                z: -0.60949110984802246
            },
           dimensions: {
                x: 0.018763428553938866,
                y: 0.21442186832427979,
                z: 0.017878998070955276
            }
        }
    },
    {
        name: 'largeBrush',
        type: 'brush',
        toolRadius: 0.16,
        toolLength: 0.225,
        properties: {
            dimensions: {
                x: 0.021168617531657219,
                y: 0.37896084785461426,
                z: 0.17102941870689392
            },
            modelURL: MODELS_PATH + "largeBrush.fbx"
        },
        paletteProperties: {
            position: {
                x: -0.020652294158935547,
                y: 0.0073699951171875,
                z: 0.17883014678955078
            },
            leftRotation: {"x":0.6448094844818115,"y":0.6511309146881104,"z":-0.26214462518692017,"w":-0.3025384545326233},
            rightRotation: {
                w: 0.63280689716339111,
                x: -0.31551080942153931,
                y: -0.31563287973403931,
                z: 0.63271534442901611
            },
            dimensions: {
                x: 0.0074541284702718258,
                y: 0.13344390690326691,
                z: 0.060224775224924088
            }
        }
    },
    {
        name: 'smallpaintRag',
        type: 'eraser',
        toolRadius: 0.08,
        toolLength: 0.05,
        properties: {
            dimensions: {
                x: 0.19501656293869019,
                y: 0.023836053907871246,
                z: 0.18799637258052826
            },
            modelURL: MODELS_PATH + "smallpaintRag.fbx"
        },
        paletteProperties: {
            position: {
                x: 0.25367879867553711,
                y: 0.00518798828125,
                z: -0.046645164489746094
            },
            leftRotation: {"x":0.0653543695807457,"y":0.043366167694330215,"z":-0.002849785378202796,"w":-0.9969152808189392},
            rightRotation: {
                w: -1.52587890625e-05,
                x: -0.11326771974563599,
                y: -4.57763671875e-05,
                z: 0.99356067180633545
            },
            dimensions: {
                x: 0.19501656293869019,
                y: 0.023836053907871246,
                z: 0.18799637258052826
            }
        }
    }/*,
    {
        name: 'medpaintRag',
        type: 'eraser',
        toolRadius: 0.08,
        toolLength: 0.05,
        properties: {
            dimensions: {
                x: 0.25618726015090942,
                y: 0.035295568406581879,
                z: 0.26880133152008057
            },
            modelURL: MODELS_PATH + "medpaintRag.fbx"
        }
    },
    {
        name: 'largepaintRag',
        type: 'eraser',
        toolRadius: 0.15,
        toolLength: 0.05,
        properties: {
            dimensions: {
                x: 0.49643242359161377,
                y: 0.050745047628879547,
                z: 0.30312520265579224
            },
            modelURL: MODELS_PATH + "largepaintRag.fbx"
        }
    }*/
];

PALETTE_LIFETIME = 20;
VOXEL_MANIPULATOR_LIFETIME = 20;

PALETTE_MODEL_LEFT_HAND = MODELS_PATH + 'painter_Palette2.fbx';
PALETTE_MODEL_RIGHT_HAND = MODELS_PATH + 'painter_Palette.fbx';
