CPP=g++
CMDLINE_PATH=../../cmdline/
PBBSLIB_PATH=../../pbbslib/
PBBSBENCH_PATH=../../pbbsbench/
INSTALL_FOLDER=.

ifeq (, $(shell which jemalloc-config))
JEMALLOC =
else
JEMALLOCLD = $(shell jemalloc-config --libdir)
JEMALLOC = -L$(JEMALLOCLD) -ljemalloc
endif

HOMEGROWN_COMMON_PREFIX=-DHOMEGROWN -pthread

HWLOC_PREFIX=$(HWLOC_CFLAGS) $(HWLOC_LDFLAGS)
ifdef USE_HWLOC
	HWLOC_PREFIX+=-DUSE_HWLOC
endif

INCLUDE_FILES=\
	$(wildcard *.hpp) \
	$(wildcard $(CMDLINE_PATH)/include/*.hpp) \
	$(wildcard $(PBBSLIB_PATH)/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/common/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/breadthFirstSearch/ndBFS/*.C) \
	$(wildcard $(PBBSBENCH_PATH)/convexHull/quickHull/*.C)
INCLUDE_DIRECTIVES=\
	-I $(CMDLINE_PATH)/include \
	-I $(PBBSLIB_PATH) \
	-I $(PBBSBENCH_PATH) \
	-I $(PBBSBENCH_PATH)/common

WARNINGS_PREFIX=-Wno-subobject-linkage -Wno-overflow
COMMON_PREFIX=-std=c++17 -mcx16 $(WARNINGS_PREFIX) $(HWLOC_PREFIX)
OPT_PREFIX=-O3 -march=native -DNDEBUG $(COMMON_PREFIX)
PBBSLIBCPPHG_PREFIX=$(OPT_PREFIX) $(HOMEGROWN_COMMON_PREFIX)

%.hg.bin: %.cpp $(INCLUDE_FILES)
	$(CPP) $(PBBSLIBCPPHG_PREFIX) $(INCLUDE_DIRECTIVES) -o $@ $< $(JEMALLOC)

PROGRAMS=\
	quicksort

install:
	cp run $(INSTALL_FOLDER)
	cp *.bin $(INSTALL_FOLDER)/

clean:
	rm -f *.bin
