
/* global Script, PALETTE_COLORS:true, TEXTURE_PATH:true */

TEXTURE_PATH = Script.resolvePath('textures') + '/';

PALETTE_COLORS = [
    {
        name: 'black',
        color: { red: 0, green: 0, blue: 0 },
        textures: TEXTURE_PATH + 'color-0-0-0.png'
    },
    {
        name: 'blue',
        color: { red: 0, green: 0, blue: 255 },
        textures: TEXTURE_PATH + 'color-0-0-ff.png'
    },
    {
        name: 'green',
        color: { red: 0, green: 255, blue: 0 },
        textures: TEXTURE_PATH + 'color-0-ff-0.png'
    },
    {
        name: 'cyan',
        color: { red: 0, green: 255, blue: 255 },
        textures: TEXTURE_PATH + 'color-0-ff-ff.png'
    },
    {
        name: 'red',
        color: { red: 255, green: 0, blue: 0 },
        textures: TEXTURE_PATH + 'color-ff-0-0.png'
    },
    {
        name: 'magenta',
        color: { red: 255, green: 0, blue: 255 },
        textures: TEXTURE_PATH + 'color-ff-0-ff.png'
    },
    {
        name: 'yellow',
        color: { red: 255, green: 255, blue: 0 },
        textures: TEXTURE_PATH + 'color-ff-ff-0.png'
    },
    {
        name: 'white',
        color: { red: 255, green: 255, blue: 255 },
        textures: TEXTURE_PATH + 'color-ff-ff-ff.png'
    },
    {
        name: 'orange',
        color: { red: 255, green: 165, blue: 0 },
        textures: TEXTURE_PATH + 'color-ff-a5-0.png'
    },
    {
        name: 'wood',
        color: { red: 153, green: 76, blue: 0 },
        textures: TEXTURE_PATH + 'wood.png'
    },
    {
        name: 'grass and dirt',
        color: { red: 0, green: 76, blue: 0 },
        textures: [
            TEXTURE_PATH + 'dirt.png',
            TEXTURE_PATH + 'grass.png?v=2',
            TEXTURE_PATH + 'dirt.png'
        ]
    }
];
