#!/usr/bin/python3
#
# Launch a High Fidelity domain-server and set of assignment-clients,
# and collect their logs into separate text files.
#
# Ctrl-C can be used to shutdown
# the domain server and all assignment clients.
#
# TODO: the directories are hard coded, get them from environment variables instead.
#

import subprocess, signal, shlex

# root of the built from source hifi project
HIFI_PROJECT_DIR = '/home/seth/src/hifi'

# where server logs are stored
SERVER_LOG_DIR = '/home/seth/hifi-logs'

SERVER_TYPE = '' # 'Release/' or 'Debug/' or empty string on Linux

def sig_handler(signum, frame):
    # TODO: we might want to flush the logs or 'gracefully terminate' servers here.
    print('Ctrl-C')

signal.signal(signal.SIGINT, sig_handler)

def spawn(cmd, log):
    print("  %s" % cmd)
    args = shlex.split(cmd)
    return subprocess.Popen(args, stdout=log, stderr=log)

print('launching servers...')

ASSIGNMENT_CLIENT = '/build/assignment-client/' + SERVER_TYPE + 'assignment-client'
DOMAIN_SERVER = '/build/domain-server/' + SERVER_TYPE + 'domain-server'

servers = [
    {
        'cmd': HIFI_PROJECT_DIR + '/build/domain-server/' + SERVER_TYPE + 'domain-server',
        'log': open(SERVER_LOG_DIR + '/domain-server.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t0',
        'log': open(SERVER_LOG_DIR + '/audio-mixer.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t1',
        'log': open(SERVER_LOG_DIR + '/avatar-mixer.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t2',
        'log': open(SERVER_LOG_DIR + '/agent-0.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t2',
        'log': open(SERVER_LOG_DIR + '/agent-1.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t3',
        'log': open(SERVER_LOG_DIR + '/asset-server.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t4',
        'log': open(SERVER_LOG_DIR + '/messages-mixer.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t5',
        'log': open(SERVER_LOG_DIR + '/script-server.txt', 'w')
    },
    {
        'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t6',
        'log': open(SERVER_LOG_DIR + '/entity-server.txt', 'w')
    }
]

NUM_AGENTS = 0;
[servers.append({ 'cmd': HIFI_PROJECT_DIR + ASSIGNMENT_CLIENT + ' -t2',
                  'log': open(SERVER_LOG_DIR + '/agent-%02d.txt' % i, 'w') }) for i in range(NUM_AGENTS)]


# for server in servers:
#     print(server)

procs = [spawn(server['cmd'], server['log']) for server in servers]
[proc.wait() for proc in procs]
