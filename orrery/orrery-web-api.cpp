
#include <iostream>
using namespace std;

#include <string.h>
#include <math.h>
#include <ctime>
#include <map>
#include <vector>
#include <sstream>
#include <cstdlib>

extern "C" {
#include "cspice/include/SpiceCK.h"
#include "cspice/include/SpiceZpr.h"
}


// http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
void matrixToQuaternion(SpiceDouble a[][3], float q[]) {
  float trace = a[0][0] + a[1][1] + a[2][2]; // I removed + 1.0f; see discussion with Ethan
  if( trace > 0 ) {// I changed M_EPSILON to 0
    float s = 0.5f / sqrtf(trace+ 1.0f);
    q[0] = 0.25f / s;
    q[1] = ( a[2][1] - a[1][2] ) * s;
    q[2] = ( a[0][2] - a[2][0] ) * s;
    q[3] = ( a[1][0] - a[0][1] ) * s;
  } else {
    if ( a[0][0] > a[1][1] && a[0][0] > a[2][2] ) {
      float s = 2.0f * sqrtf( 1.0f + a[0][0] - a[1][1] - a[2][2]);
      q[0] = (a[2][1] - a[1][2] ) / s;
      q[1] = 0.25f * s;
      q[2] = (a[0][1] + a[1][0] ) / s;
      q[3] = (a[0][2] + a[2][0] ) / s;
    } else if (a[1][1] > a[2][2]) {
      float s = 2.0f * sqrtf( 1.0f + a[1][1] - a[0][0] - a[2][2]);
      q[0] = (a[0][2] - a[2][0] ) / s;
      q[1] = (a[0][1] + a[1][0] ) / s;
      q[2] = 0.25f * s;
      q[3] = (a[1][2] + a[2][1] ) / s;
    } else {
      float s = 2.0f * sqrtf( 1.0f + a[2][2] - a[0][0] - a[1][1] );
      q[0] = (a[1][0] - a[0][1] ) / s;
      q[1] = (a[0][2] + a[2][0] ) / s;
      q[2] = (a[1][2] + a[2][1] ) / s;
      q[3] = 0.25f * s;
    }
  }
}


class body {
public:
    body(string readableName, string name, string barycenterName, string orbitsAround) :
        _readableName(readableName),
        _name(name),
        _barycenterName(barycenterName),
        _orbitsAround(orbitsAround)
        {
            _frameName = string("IAU_") + _name;
        }

    const char* getReadableName() { return _readableName.c_str(); }
    const char* getName() { return _name.c_str(); }
    const char* getBarycenterName() { return _barycenterName.c_str(); }
    const char* getOrbitsAround() { return _orbitsAround.c_str(); }
    const char* getFrameName() { return _frameName.c_str(); }

private:
    string _readableName;
    string _name;
    string _barycenterName;
    string _orbitsAround;
    string _frameName; // this body's own frame
};


int main (int argc, char *argv[]) {

    furnsh_c("data/pck00011.tpc");
    furnsh_c("data/naif0012.tls");
    furnsh_c("data/de438.bsp");

    char erractAction[] {"REPORT"};
    erract_c("SET", 0, erractAction);

    time_t now = time(0);

    char* queryString = getenv("QUERY_STRING");
    if (queryString && strlen(queryString) > 0) {
        vector<string> queryArgs;
        istringstream f(queryString);
        string s;
        while (getline(f, s, '&')) {
            queryArgs.push_back(s);
        }
        if (queryArgs.size() > 0) {
            string arg = queryArgs[0];
            int eqPos = arg.find('=');
            if (eqPos > 0) {
                string var = arg.substr(0, eqPos);
                string val = arg.substr(eqPos+1);
                if (var == "time") {
                    now = stoi(val);
                }
            }
        }
    }

    tm *gmtm = gmtime(&now);
    char utc[128];
    strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);

    SpiceDouble et;
    utc2et_c(utc, &et);

    vector<body> bodies = {
        { "Sun", "SUN", "SUN", "SUN" },
        { "Mercury", "MERCURY", "MERCURY", "SUN" },
        { "Venus", "VENUS", "VENUS", "SUN" },
        { "Earth", "EARTH", "EARTH", "SUN" },
        { "Moon", "MOON", "MOON", "EARTH" },
        { "Mars", "MARS", "MARS BARYCENTER", "SUN" },
        { "Jupiter", "JUPITER", "JUPITER BARYCENTER", "SUN" },
        { "Saturn", "SATURN", "SATURN BARYCENTER", "SUN" },
        { "Uranus", "URANUS", "URANUS BARYCENTER", "SUN" },
        { "Neptune", "NEPTUNE", "NEPTUNE BARYCENTER", "SUN" },
        { "Pluto", "PLUTO", "PLUTO BARYCENTER", "SUN" },
    };

    stringstream output;
    output << "{\n";

    output << "    \"bodies\": {\n";

    bool first = true;
    for (auto& body : bodies) {

        SpiceDouble ptarg[3];
        SpiceDouble lt;
        spkpos_c(body.getBarycenterName(), et, "IAU_SUN", "NONE", body.getOrbitsAround(), ptarg, &lt);

        float x = ptarg[0];
        float y = ptarg[1];
        float z = ptarg[2];

        float radiusX, radiusY, radiusZ;
        SpiceDouble radii[3];
        SpiceInt n;
        bodvrd_c(body.getName(), "RADII", 3, &n, radii);
        radiusX = radii[0];
        radiusY = radii[1];
        radiusZ = radii[2];

        SpiceDouble orientationMatrix[3][3];
        pxform_c("IAU_SUN", body.getFrameName(), et, orientationMatrix);
        float orientation[4];
        matrixToQuaternion(orientationMatrix, orientation);

        if (first) {
            first = false;
        } else {
            output << ",\n";
        }

        char buffer[4096];
        snprintf(buffer, sizeof(buffer), "    \"%s\": {\n"
                 "            \"name\": \"%s\",\n"
                 "            \"position\": { \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"distance\": %f,\n"
                 "            \"size\": { \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"orbits\": \"%s\",\n"
                 "            \"orientation\": { \"w\": %f, \"x\": %f, \"y\": %f, \"z\": %f }\n",
                 body.getName(),
                 body.getReadableName(),
                 x, z, y, sqrt(x*x + y*y + z+z),
                 radiusX, radiusZ, radiusY,
                 body.getOrbitsAround(),
                 orientation[0], orientation[1], orientation[3], orientation[2]
            );
        output << "    " << buffer << "        }";
    }

    output << "\n    },\n";

    output << "    \"time\": " << now << "\n";

    output << "}" << endl;

    cout << "Content-Type: application/json\r\n";
    cout << "Content-Length: " << output.tellp() << "\r\n";
    cout << "\r\n";
    cout << output.str();

    kclear_c();
}

/*
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/info/mostused.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/erract_c.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvar_c.html
 */
