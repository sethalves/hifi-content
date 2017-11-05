// each element is: [registration-point, dimensions, rotation]

wireRegistrationPoints = [
    [{ x: 0.5, y: 0.5, z: 0.5 }, { x: 1.0, y: 1.0, z: 1.0 }, { x: 0.0, y: 0.0, z: 0.0, w: 1.0 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.5 }, { x: 0.0621338, y: 0.0285302, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.5, z: 0.7937741708102342 }, { x: 0.027134, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.7937741708102342 }, { x: 0.0621338, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.5 }, { x: 0.0649986, y: 0.0285302, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.5527731863587867 }, { x: 0.0999984, y: 0.01902008, z: 0.0180893 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.7937741708102342 }, { x: 0.0649986, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.7937741708102342 }, { x: 0.0999984, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.5, z: 0.20622582918976584 }, { x: 0.027134, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.20622582918976584 }, { x: 0.0621338, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.4472268136412133, y: 0.5, z: 0.5 }, { x: 0.0180893, y: 0.01902008, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.5 }, { x: 0.0621338, y: 0.0285302, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.20622582918976584 }, { x: 0.0649986, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.20622582918976584 }, { x: 0.0999984, y: 0.0285302, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.5 }, { x: 0.0649986, y: 0.0285302, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.5 }, { x: 0.0999984, y: 0.0285302, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.22197549806035383, z: 0.5 }, { x: 0.027134, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.22197549806035383, z: 0.5 }, { x: 0.0621338, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.22197549806035383, z: 0.7937741708102342 }, { x: 0.027134, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.22197549806035383, z: 0.7937741708102342 }, { x: 0.0621338, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.22197549806035383, z: 0.5 }, { x: 0.0649986, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.22197549806035383, z: 0.5 }, { x: 0.0999984, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.22197549806035383, z: 0.7937741708102342 }, { x: 0.0649986, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.22197549806035383, z: 0.7937741708102342 }, { x: 0.0999984, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.22197549806035383, z: 0.20622582918976584 }, { x: 0.027134, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.22197549806035383, z: 0.20622582918976584 }, { x: 0.0621338, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.22197549806035383, z: 0.5 }, { x: 0.027134, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.22197549806035383, z: 0.5 }, { x: 0.0621338, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.22197549806035383, z: 0.20622582918976584 }, { x: 0.0649986, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.22197549806035383, z: 0.20622582918976584 }, { x: 0.0999984, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.22197549806035383, z: 0.5 }, { x: 0.0649986, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.22197549806035383, z: 0.5 }, { x: 0.0999984, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.7780245019396461, z: 0.5 }, { x: 0.027134, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.7780245019396461, z: 0.5 }, { x: 0.0621338, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.7780245019396461, z: 0.7937741708102342 }, { x: 0.027134, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.7780245019396461, z: 0.7937741708102342 }, { x: 0.0621338, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.7780245019396461, z: 0.5 }, { x: 0.0649986, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.7780245019396461, z: 0.5 }, { x: 0.0999984, y: 0.0642643, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.7780245019396461, z: 0.7937741708102342 }, { x: 0.0649986, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.7780245019396461, z: 0.7937741708102342 }, { x: 0.0999984, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.7780245019396461, z: 0.20622582918976584 }, { x: 0.027134, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.7780245019396461, z: 0.20622582918976584 }, { x: 0.0621338, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.7780245019396461, z: 0.5 }, { x: 0.027134, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.7780245019396461, z: 0.5 }, { x: 0.0621338, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.7780245019396461, z: 0.20622582918976584 }, { x: 0.0649986, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.7780245019396461, z: 0.20622582918976584 }, { x: 0.0999984, y: 0.0642643, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.7780245019396461, z: 0.5 }, { x: 0.0649986, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.7780245019396461, z: 0.5 }, { x: 0.0999984, y: 0.0642643, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.4472268136412133, y: 0.5, z: 0.5 }, { x: 0.0180893, y: 0.0999984, z: 0.01902008 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.5 }, { x: 0.0621338, y: 0.0999984, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.5, z: 0.7937741708102342 }, { x: 0.027134, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.7937741708102342 }, { x: 0.0621338, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.5 }, { x: 0.0649986, y: 0.0999984, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.5 }, { x: 0.0999984, y: 0.0999984, z: 0.02598 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.7937741708102342 }, { x: 0.0649986, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.7937741708102342 }, { x: 0.0999984, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.5, z: 0.20622582918976584 }, { x: 0.027134, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.20622582918976584 }, { x: 0.0621338, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.447210142256947, y: 0.5, z: 0.5 }, { x: 0.027134, y: 0.0999984, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.19529788939353465, y: 0.5, z: 0.5 }, { x: 0.0621338, y: 0.0999984, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.20622582918976584 }, { x: 0.0649986, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.20622582918976584 }, { x: 0.0999984, y: 0.0999984, z: 0.0629892 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.7692350296775623, y: 0.5, z: 0.5 }, { x: 0.0649986, y: 0.0999984, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
    [{ x: 0.5, y: 0.5, z: 0.5 }, { x: 0.0999984, y: 0.0999984, z: 0.0999984 }, { x: 0, y: 0, z: 0, w: 1 }],
];

module.exports = wireRegistrationPoints;