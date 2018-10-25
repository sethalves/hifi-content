
#include <iostream>
using namespace std;

#include <string.h>
#include <ctime>
#include <map>
#include <vector>

extern "C" {
#include "cspice/include/SpiceCK.h"
#include "cspice/include/SpiceZpr.h"
}


int main (int argc, char *argv[]) {

    furnsh_c("data/pck00011.tpc");
    furnsh_c("data/naif0012.tls");
    furnsh_c("data/de438.bsp");

    // char *erractAction = strdup("RETURN");
    // char *erractAction = strdup("IGNORE");
    char *erractAction = strdup("REPORT");
    erract_c("SET", 0, erractAction);

    time_t now = time(0);
    tm *gmtm = gmtime(&now);
    char utc[128];
    strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);

    SpiceDouble et;
    utc2et_c(utc, &et);


    // "MOON"

    const char* bodies[] { "SUN", "MERCURY", "VENUS", "EARTH", "MARS", "JUPITER",
            "SATURN", "URANUS", "NEPTUNE", "PLUTO", "" };

    std::map<std::string, std::string> barycenterNames = {
        { "SUN", "SUN"},
        { "MERCURY", "MERCURY"},
        { "VENUS", "VENUS"},
        { "EARTH", "EARTH"},
        { "MARS", "MARS BARYCENTER"},
        { "JUPITER", "JUPITER BARYCENTER"},
        { "SATURN", "SATURN BARYCENTER"},
        { "URANUS", "URANUS BARYCENTER"},
        { "NEPTUNE", "NEPTUNE BARYCENTER"},
        { "PLUTO", "PLUTO BARYCENTER"}
    };


    std::map<std::string, std::vector<float>> radii = {
        // { "MARS BARYCENTER", { 3396.2, 3396.2, 3376.2}}, // https://en.wikipedia.org/wiki/Mars
        // { "JUPITER BARYCENTER", { 71492, 71492, 66854}}, // https://en.wikipedia.org/wiki/Jupiter
        // { "SATURN BARYCENTER", { 60268, 60268, 54364}}, // https://en.wikipedia.org/wiki/Saturn
        // { "URANUS BARYCENTER", { 25559, 25559, 24973}} // https://en.wikipedia.org/wiki/Uranus

    };

    cout << "{";
    for (int i = 0; ; i++) {
        if (strlen(bodies[i]) == 0) break;

        SpiceDouble ptarg[3];
        SpiceDouble lt;
        spkpos_c("SUN", et, "IAU_SUN", "NONE", barycenterNames[bodies[i]].c_str(), ptarg, &lt);

        float x = ptarg[0];
        float y = ptarg[1];
        float z = ptarg[2];

        float radiusX, radiusY, radiusZ;
        auto findRadii = radii.find(bodies[i]);
        if (findRadii != radii.end()) {
            // std::vector<float>& rs = radii[bodies[i]];
            std::vector<float>& rs = findRadii->second;
            radiusX = rs[0];
            radiusY = rs[1];
            radiusZ = rs[2];
        } else {
            SpiceDouble radii[3];
            SpiceInt n;
            bodvrd_c(bodies[i], "RADII", 3, &n, radii);
            radiusX = radii[0];
            radiusY = radii[1];
            radiusZ = radii[2];
        }

        SpiceDouble orientationMatrix[3][3];
        pxform_c("IAU_SUN", "IAU_MARS", et, orientationMatrix);


        char buffer[1024];
        snprintf(buffer, sizeof(buffer), "\"%s\": { "
                 "position: { x: %f, y: %f, z: %f }, "
                 "size: { x: %f, y: %f, z: %f }, "
                 "orientation: { }",
                 bodies[i],
                 x, y, z,
                 radiusX, radiusY, radiusZ
            );
        bool first = (i == 0);
        cout << (first ? "" : ",") << endl << "    " << buffer;
    }
    cout << endl << "}" << endl;

    kclear_c();
}

/*

  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/info/mostused.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/erract_c.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvar_c.html

 */
