
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
        // cerr << "QQQQ queryString = " << queryString << endl;

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
                    now = stol(val);
                    // cerr << "QQQQ now = " << now << endl;
                }
            }
        }
    }

    tm *gmtm = gmtime(&now);
    char utc[128];
    strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);
    SpiceDouble et;
    utc2et_c(utc, &et);


    // also create a Observer epoch one hour in the future
    time_t future = now + 3600;
    tm *gmtmFuture = gmtime(&future);
    char utcFuture[128];
    strftime(utcFuture, sizeof(utcFuture), "%a %b %d %H:%M:%S UTC %Y", gmtmFuture);
    SpiceDouble etFuture;
    utc2et_c(utcFuture, &etFuture);


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

        // SpiceDouble ptarg[3];
        // SpiceDouble lt;
        // spkpos_c(body.getBarycenterName(), et, "IAU_SUN", "NONE", body.getOrbitsAround(), ptarg, &lt);
        // float x = ptarg[0];
        // float y = ptarg[1];
        // float z = ptarg[2];



        SpiceDouble state[6];
        SpiceDouble lt;
        spkezr_c(body.getBarycenterName(), et, "IAU_SUN", "NONE", body.getOrbitsAround(), state, &lt);
        vector<float> position { (float)state[0], (float)state[1], (float)state[2] };
        vector<float> velocity { (float)state[3], (float)state[4], (float)state[5] };


        SpiceDouble futureState[6];
        SpiceDouble futureLt;
        spkezr_c(body.getBarycenterName(), etFuture, "IAU_SUN", "NONE", body.getOrbitsAround(), futureState, &futureLt);
        vector<float> positionFuture { (float)futureState[0], (float)futureState[1], (float)futureState[2] };


        float radiusX, radiusY, radiusZ;
        SpiceDouble radii[3];
        SpiceInt n;
        bodvrd_c(body.getName(), "RADII", 3, &n, radii);


        SpiceDouble orientationMatrix[3][3];
        pxform_c("IAU_SUN", body.getFrameName(), et, orientationMatrix);
        float orientation[4];
        matrixToQuaternion(orientationMatrix, orientation);


        SpiceDouble orientationMatrixFuture[3][3];
        pxform_c("IAU_SUN", body.getFrameName(), etFuture, orientationMatrixFuture);
        float orientationFuture[4];
        matrixToQuaternion(orientationMatrixFuture, orientationFuture);



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
                 "            \"velocity\": { \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"size\": { \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"orbits\": \"%s\",\n"
                 "            \"orientation\": { \"w\": %f, \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"positionInOneHour\": { \"x\": %f, \"y\": %f, \"z\": %f },\n"
                 "            \"orientationInOneHour\": { \"w\": %f, \"x\": %f, \"y\": %f, \"z\": %f }\n",
                 body.getName(),
                 body.getReadableName(),
                 position[0], position[1], position[2],
                 sqrt(position[0]*position[0] + position[1]*position[1] + position[2]+position[2]),
                 velocity[0], velocity[1], velocity[2],
                 radii[0], radii[1], radii[2],
                 body.getOrbitsAround(),
                 orientation[0], orientation[1], orientation[2], orientation[3],
                 positionFuture[0], positionFuture[1], positionFuture[2],
                 orientationFuture[0], orientationFuture[1], orientationFuture[2], orientationFuture[3]
            );
        output << "    " << buffer << "        }";
    }

    output << "\n    },\n";

    output << "    \"time\": " << now << ",\n";

    const char *pictur { "Wkd Mon DD HR:MN:SC UTC YYYY ::UTC-0" };
    char readableTime[1024];
    timout_c(et, pictur, sizeof(readableTime), readableTime);
    // cerr << "QQQQ now readable = " << readableTime << endl;

    output << "    \"timeReadable\": \"" << readableTime << "\"\n";

    output << "}" << endl;

    cout << "Content-Type: application/json\r\n";
    cout << "Content-Length: " << output.tellp() << "\r\n";
    cout << "\r\n";
    cout << output.str();

    kclear_c();
}

/*
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/info/mostused.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/erract_c.html
  https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvar_c.html
  ftp://naif.jpl.nasa.gov/pub/naif/misc/Quaternion_White_Paper/Quaternions_White_Paper.pdf
 */
