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

CILK_COMMON_PREFIX=-fcilkplus -lcilkrts -DCILK

HWLOC_PREFIX=$(HWLOC_CFLAGS) $(HWLOC_LDFLAGS)
ifdef USE_HWLOC
	HWLOC_PREFIX+=-DUSE_HWLOC
endif

INCLUDE_FILES=\
	$(wildcard $(CMDLINE_PATH)/include/*.hpp) \
	$(wildcard $(PBBSLIB_PATH)/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/common/*.h)
INCLUDE_DIRECTIVES=\
	-I $(CMDLINE_PATH)/include \
	-I $(PBBSLIB_PATH) \
	-I $(PBBSBENCH_PATH) \
	-I $(PBBSBENCH_PATH)/common

WARNINGS_PREFIX=-Wno-subobject-linkage -Wno-overflow
COMMON_PREFIX=-std=c++17 -mcx16 $(WARNINGS_PREFIX) $(HWLOC_PREFIX)

OPT_PREFIX=-O3 -march=native -DNDEBUG $(COMMON_PREFIX)
DBG_PREFIX=-O0 -g3 $(COMMON_PREFIX)

PARDBG_PREFIX=$(DBG_PREFIX) $(CILK_COMMON_PREFIX)
SEQDBG_PREFIX=$(DBG_PREFIX)

PBBSLIBCPP_PREFIX=$(OPT_PREFIX) $(CILK_COMMON_PREFIX)

%.seqdbg.bin: %.cpp 
	$(CPP) $(SEQDBG_PREFIX) $(INCLUDE_DIRECTIVES) -o $@ $<

%.pbbscpp.bin: %.cpp $(INCLUDE_FILES)
	$(CPP) $(PBBSLIBCPP_PREFIX) $(INCLUDE_DIRECTIVES) -o $@ $< $(JEMALLOC)

all:

install:
	cp run-* $(INSTALL_FOLDER)

clean:
	rm -f *.bin
