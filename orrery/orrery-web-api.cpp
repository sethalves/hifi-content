
#include <iostream>
using namespace std;

#include <string.h>
#include <ctime>

extern "C" {
#include "cspice/include/SpiceCK.h"
#include "cspice/include/SpiceZpr.h"
}


int main (int argc, char *argv[]) {

    furnsh_c("data/pck00011.tpc");
    furnsh_c("data/naif0012.tls");
    furnsh_c("data/de438.bsp");

    time_t now = time(0);
    tm *gmtm = gmtime(&now);
    char utc[128];
    strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);

    SpiceDouble et;
    utc2et_c(utc, &et);

    SpiceDouble ptarg[3];
    SpiceDouble lt;

    const char* bodies[] { "SUN", "MERCURY", "VENUS", "MOON", "EARTH", "MARS BARYCENTER", "JUPITER BARYCENTER",
            "SATURN BARYCENTER", "URANUS BARYCENTER", "NEPTUNE BARYCENTER", "PLUTO BARYCENTER", "" };

    float SCALE = 0.0000001;

    cout << "{";
    for (int i = 0; ; i++) {
        if (strlen(bodies[i]) == 0) break;
        spkpos_c("SUN", et, "IAU_SUN", "NONE", bodies[i], ptarg, &lt);

        float x = ptarg[0] * SCALE;
        float y = ptarg[1] * SCALE;
        float z = ptarg[2] * SCALE;

        char buffer[1024];
        snprintf(buffer, sizeof(buffer), "\"%s\": { x: %f, y: %f, z: %f }", bodies[i], x, y, z);
        bool first = (i == 0);
        cout << (first ? "" : ",") << endl << "    " << buffer;
    }
    cout << endl << "}" << endl;

    kclear_c();
}
