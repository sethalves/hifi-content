
#include <iostream>
using namespace std;

extern "C" {
#include "cspice/include/SpiceCK.h"
#include "cspice/include/SpiceZpr.h"
}


int main (int argc, char *argv[]) {

    // furnsh_c("data/earth_assoc_itrf93.tf");
    // furnsh_c("data/naif0012.tls");
    // furnsh_c("data/de438.bsp");

    // furnsh_c("data/earth_assoc_itrf93.tf");
    // furnsh_c("data/EARTH_000101_070620_070329.BPC");
    furnsh_c("data/pck00011.tpc");
    furnsh_c("data/naif0012.tls");
    furnsh_c("data/de438.bsp");

    const char* utc { "Sun Jun  3 15:26:35 UTC 2018" };
    SpiceDouble et;
    utc2et_c(utc, &et);

    SpiceDouble ptarg[3];
    SpiceDouble lt;
    // spkpos_c("SUN", et, "MOON_ME", "NONE", "MOON", ptarg, &lt);
    // spkpos_c("SUN", et, "ITRF93", "NONE", "MOON", ptarg, &lt);

    spkpos_c("SUN", et, "IAU_EARTH", "NONE", "MOON", ptarg, &lt);

    cout << (ptarg[0] * 1000) << std::endl;
    cout << (ptarg[1] * 1000) << std::endl;
    cout << (ptarg[2] * 1000) << std::endl;

    kclear_c();
}
