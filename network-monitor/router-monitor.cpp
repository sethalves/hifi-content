#include <iostream>
#include <stdlib.h>
#include <pcap.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


bool lookupNetwork(std::string& device, std::string& network, std::string& netmask) {
    // https://eecs.wsu.edu/~sshaikot/docs/lbpcap/libpcap-tutorial.pdf
    char errbuf[PCAP_ERRBUF_SIZE];
    bpf_u_int32 netp; /* ip */
    bpf_u_int32 maskp;/* subnet mask */
    struct in_addr addr; /* ask pcap to find a valid device for use to sniff on */

    /* name of the device to use */
    char* dev = pcap_lookupdev(errbuf);
    if (dev == NULL) {
        std::cerr << errbuf << std::endl;
        return false;
    }

    /* ask pcap for the network address and mask of the device */
    int ret = pcap_lookupnet(dev, &netp, &maskp, errbuf);
    if (ret == -1) {
        std::cerr << errbuf << std::endl;
        return false;
    }

    /* get the network address in a human readable form (dot notation) */
    addr.s_addr = netp;
    char* net = inet_ntoa(addr);
    if (net == NULL) { /* thanks Scott :-P */
        perror("inet_ntoa");
        return false;
    }

    /* do the same as above for the device's mask (dot notation) */
    addr.s_addr = maskp;
    char* mask = inet_ntoa(addr);
    if (mask == NULL) {
        perror("inet_ntoa");
        return false;
    }

    device = std::string(dev);
    network = std::string(net);
    netmask = std::string(mask);
    return true;
}



int main(int argc, char **argv) {

    std::string device, network, netmask;
    bool lookupNetworkResult = lookupNetwork(device, network, netmask);
    if (lookupNetworkResult) {
        std::cout << "device=" << device << ", network=" << network << ", netmask=" << netmask << std::endl;
    }

    return 0;
}
