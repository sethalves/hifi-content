#
#
#

CXX=clang++
CXXFLAGS=-g -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined

%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $^ -o $@


all: router-monitor


router-monitor: router-monitor.o
	$(CXX) $^ -o $@ $(CXXFLAGS) -lpcap


# router-monitor.o: router-monitor.cpp
# 	$(CXX) -c $^ -o $@


clean:
	rm -f *~ *.o
	rm -f router-monitor
