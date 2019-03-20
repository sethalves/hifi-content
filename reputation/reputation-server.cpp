
#include <iostream>
using namespace std;

#include <string.h>
#include <math.h>
#include <ctime>
#include <map>
#include <vector>
#include <sstream>
#include <cstdlib>
#include <sqlite3.h>

#include "picojson.h"

int main (int argc, char *argv[]) {

    // time_t now = time(0);

    char* queryString = getenv("QUERY_STRING");
    if (queryString && strlen(queryString) > 0) {
        cerr << "QQQQ queryString = " << queryString << endl;

        vector<string> queryArgs;
        istringstream f(queryString);
        string s;
        while (getline(f, s, '&')) {
            queryArgs.push_back(s);
        }
        // if (queryArgs.size() > 0) {
        //     string arg = queryArgs[0];
        //     int eqPos = arg.find('=');
        //     if (eqPos > 0) {
        //         string var = arg.substr(0, eqPos);
        //         string val = arg.substr(eqPos+1);
        //         if (var == "time") {
        //             now = stol(val);
        //             // cerr << "QQQQ now = " << now << endl;
        //         }
        //     }
        // }
    }

    std::string jsonBody;

    char *contentLengthString = getenv("CONTENT_LENGTH");
    cerr << "HERE length is -- " << contentLengthString << "\n";
    if (contentLengthString) {
        unsigned int contentLength = atoi(contentLengthString);

        // char buffer[contentLength + 1];
        // cerr.get(buffer, contentLength);
        // jsonBody = buffer;

        cerr << "HERE contentLength is -- " << contentLength;
        while (jsonBody.length() < contentLength) {
            char c;
            // cin >> c;
            cin.get(c);
            jsonBody += c;
        }
    }

    cerr << "HERE -- '" << jsonBody << "'\n";

    // tm *gmtm = gmtime(&now);
    // char utc[128];
    // strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);

    stringstream output;

    output << "{\n";
    output << "    \"status\": \"success\",\n";
    output << "    \"data\": {\n";
    output << "        \"user\": \"someone\",\n";
    output << "        \"reputation\": -0.6\n";
    output << "    }\n";
    output << "}\n";

    cout << "Content-Type: application/json\r\n";
    cout << "Content-Length: " << output.tellp() << "\r\n";
    cout << "\r\n";
    cout << output.str();
}
