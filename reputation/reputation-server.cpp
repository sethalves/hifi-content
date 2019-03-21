
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


std::string sanitize(std::string v) {

    std::string safe = v;
    std::replace(safe.begin(), safe.end(), '\\', ' ');
    std::replace(safe.begin(), safe.end(), '\'', '"');
    return safe;
}


static int updateDBCallback(void* data, int argc, char** argv, char** azColName) {
    fprintf(stderr, "%s: ", (const char*)data);

    for (int i = 0; i < argc; i++) {
        fprintf(stderr, "%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
    }

    fprintf(stderr, "\n");
    return 0;
}


void updateDB(time_t now, std::string voter, std::string votee, bool isUp, bool isCancel) {
    sqlite3 *db { nullptr };
    char *zErrMsg = 0;

    if(sqlite3_open("/var/tmp/rep.db", &db)) {
        cerr << "reputation-server can't open database: " << sqlite3_errmsg(db) << endl;
        return;
    }

    // FIXME http://www.adp-gmbh.ch/sqlite/bind_insert.html
    std::string updateSQL = std::string("INSERT INTO Votes (voter, votee, isUp, isCancel, timestamp) VALUES ")
        + "('" + sanitize(voter) + "','" + sanitize(votee) + "'," +
        (isUp ? "1" : "0") + "," +
        (isCancel ? "1" : "0") + "," +
        std::to_string(now) + ");";

    // cerr << "update SQL is " << updateSQL;

    if (sqlite3_exec(db, updateSQL.c_str(), updateDBCallback, 0, &zErrMsg) != SQLITE_OK) {
        cerr << "reputation-server error during update: " << zErrMsg << endl;
        sqlite3_free(zErrMsg);
        sqlite3_close(db);
        return;
    }

    sqlite3_close(db);
}


float repQueryResult { 0.0f };


static int getUsersReputationCallback(void* data, int argc, char** argv, char** azColName) {
    // fprintf(stderr, "%s: ", (const char*)data);

    for (int i = 0; i < argc; i++) {
        // fprintf(stderr, "%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
        if (atoi(argv[i]) == 1) {
            repQueryResult++;
        } else {
            repQueryResult--;
        }
    }

    // fprintf(stderr, "\n");
    return 0;
}


float getUsersReputation(std::string votee) {
    sqlite3 *db { nullptr };
    char *zErrMsg = 0;

    if(sqlite3_open("/var/tmp/rep.db", &db)) {
        cerr << "reputation-server can't open database: " << sqlite3_errmsg(db) << endl;
        return 0.0f;
    }

    std::string querySQL = std::string("select isUp from Votes where votee='") + sanitize(votee) + "'";

    // cerr << "query SQL is " << querySQL << endl;

    if (sqlite3_exec(db, querySQL.c_str(), getUsersReputationCallback, 0, &zErrMsg) != SQLITE_OK) {
        cerr << "reputation-server error during query: " << zErrMsg << endl;
        sqlite3_free(zErrMsg);
        sqlite3_close(db);
        return 0.0f;
    }

    sqlite3_close(db);

    return repQueryResult;
}


int main (int argc, char *argv[]) {

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
    bool isCancel { false };
    bool isUp { false };
    bool isChange { false };
    std::string votee;
    std::string voter;

    char *contentLengthString = getenv("CONTENT_LENGTH");
    if (contentLengthString) {
        unsigned int contentLength = atoi(contentLengthString);
        while (jsonBody.length() < contentLength) {
            char c;
            // cin >> c;
            cin.get(c);
            jsonBody += c;
        }

        // cerr << "HERE body -- '" << jsonBody << "'\n";

        istringstream bodyStream;
        bodyStream.str(jsonBody);
        picojson::value v;
        bodyStream >> v;

        if (v.is<picojson::object>()) {
            // cerr << "input is an object" << std::endl;
            const picojson::object &o = v.get<picojson::object>();
            for (picojson::object::const_iterator i = o.begin(); i != o.end(); ++i) {
                if (i->first == "isCancel") isCancel = i->second.get<bool>();
                else if (i->first == "isUp") {isUp = i->second.get<bool>(); isChange = true; }
                else if (i->first == "votee") votee = i->second.get<std::string>();
                else if (i->first == "voter") voter = i->second.get<std::string>();
                // cerr << i->first << "  " << i->second << std::endl;
            }
        } else {
            cerr << "reputation-server post data is not a json object.\n";
        }
    }

    // tm *gmtm = gmtime(&now);
    // char utc[128];
    // strftime(utc, sizeof(utc), "%a %b %d %H:%M:%S UTC %Y", gmtm);

    // cerr << "OKAY, "
    //      << "isCancel=" << isCancel
    //      << "isUp=" << isUp
    //      << "votee=" << votee
    //      << "voter=" << voter;


    if (isChange) {
        updateDB(now, voter, votee, isUp, isCancel);
    }

    float reputation = getUsersReputation(votee);

    // cerr << "rep is " << reputation;

    stringstream output;
    output << "{\n";
    output << "    \"status\": \"success\",\n";
    output << "    \"data\": {\n";
    output << "        \"user\": \"" << votee << "\",\n";
    output << "        \"reputation\": " << std::to_string(reputation) << "\n";
    output << "    }\n";
    output << "}\n";

    cout << "Content-Type: application/json\r\n";
    cout << "Content-Length: " << output.tellp() << "\r\n";
    cout << "\r\n";
    cout << output.str();
}
