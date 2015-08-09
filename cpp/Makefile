
all: play_icfp2015
clean:
	rm -vf $(PLAY_ICFP2015_OBJECTS) $(PLAY_ICFP2015_DEPFILES) play_icfp2015

CXX = g++ -std=c++11

CXXFLAGS = \
	-O3 -Wall -DNDEBUG \
	$(shell pkg-config --cflags jsoncpp)

LIBS = \
	$(shell pkg-config --libs jsoncpp)

%.o: %.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $<
%.o.deps: %.cc
	$(CXX) $(CXXFLAGS) -c -M -MF $@ $<

PLAY_ICFP2015_SOURCES = \
	board.cc \
	problem_descriptor.cc \
	string_matcher.cc \
	pathfinder.cc \
	ai.cc \
	main.cc
PLAY_ICFP2015_OBJECTS = $(PLAY_ICFP2015_SOURCES:%.cc=%.o)
PLAY_ICFP2015_DEPFILES = $(PLAY_ICFP2015_OBJECTS:%.o=%.o.deps)

play_icfp2015: $(PLAY_ICFP2015_OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LIBS)

-include $(PLAY_ICFP2015_DEPFILES)
