CPP=g++
PBBSLIB_PATH=../../pbbslib/
PBBSBENCH_PATH=../../pbbsbench/
INSTALL_FOLDER=.

# Common compilation flags
# ------------------------

ifeq (, $(shell which jemalloc-config))
	JEMALLOC =
else
	JEMALLOCLD = $(shell jemalloc-config --libdir)
	JEMALLOC = -L$(JEMALLOCLD) -ljemalloc
endif

ifdef USE_HWLOC
	HWLOC_CFLAGS+=-DUSE_HWLOC
endif

INCLUDE_FILES=\
	$(wildcard *.hpp) \
	$(wildcard $(CMDLINE_PATH)/include/*.hpp) \
	$(wildcard $(MCSL_INCLUDE_PATH)/*.hpp) \
	$(wildcard $(PBBSLIB_PATH)/*.h) \
	$(wildcard $(PBBSLIB_PATH)/examples/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/common/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/breadthFirstSearch/ndBFS/*.C) \
	$(wildcard $(PBBSBENCH_PATH)/convexHull/quickHull/*.C)

INCLUDE_PREFIX=\
	-I $(CMDLINE_PATH)/include \
	-I $(MCSL_INCLUDE_PATH) \
	-I $(PBBSLIB_PATH) \
	-I $(PBBSLIB_PATH)/examples \
	-I $(PBBSBENCH_PATH) \
	-I $(PBBSBENCH_PATH)/common

WARNINGS_PREFIX=-Wno-subobject-linkage -Wno-overflow -Wno-cpp -Wno-return-type
COMMON_PREFIX=--std=c++17 -pthread -mcx16 $(WARNINGS_PREFIX) $(HWLOC_CFLAGS)
OPT_BASE_PREFIX=-O2 -m64 -march=native --save-temps -DNDEBUG
DBG_BASE_PREFIX=-O0 -g3
LINKER_PREFIX=$(HWLOC_LDFLAGS) $(JEMALLOC)

# MCSL binaries
# -------------

MCSL_PREFIX=-DMCSL $(COMMON_PREFIX)
OPT_PREFIX=$(MCSL_PREFIX) $(OPT_BASE_PREFIX) $(INCLUDE_PREFIX)
STA_PREFIX=-DMCSL_ENABLE_STATS $(OPT_PREFIX)
LOG_PREFIX=-DMCSL_ENABLE_LOGGING $(STA_PREFIX)
DBG_PREFIX=-DMCSL_ENABLE_STATS -DMCSL_ENABLE_LOGGING $(MCSL_PREFIX) $(DBG_BASE_PREFIX) $(INCLUDE_PREFIX)

%.opt: %.cpp $(INCLUDE_FILES)
	$(CPP) $(OPT_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.sta: %.cpp $(INCLUDE_FILES)
	$(CPP) $(STA_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.log: %.cpp $(INCLUDE_FILES)
	$(CPP) $(LOG_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.dbg: %.cpp $(INCLUDE_FILES)
	$(CPP) $(DBG_PREFIX) -o $@ $< $(LINKER_PREFIX)

# Cilk Plus binaries
# ------------------

CILK_BASE_PREFIX=-fcilkplus -lcilkrts -DCILK -ldl
CILK_PREFIX=$(COMMON_PREFIX) $(OPT_BASE_PREFIX) $(CILK_BASE_PREFIX) $(CILK_EXTRAS_PREFIX) $(INCLUDE_PREFIX)

%.cilk: %.cpp $(INCLUDE_FILES)
	$(CPP) $(CILK_PREFIX) -o $@ $< $(LINKER_PREFIX)

# Installation and cleanup
# ------------------------

BENCHMARKS=\
	fib \
	grep \
	samplesort \
	quickhull

all: $(BENCHMARKS:%=%.opt) $(BENCHMARKS:%=%.sta) $(BENCHMARKS:%=%.cilk)

install:
	cp *.opt *.sta *.cilk run $(INSTALL_FOLDER)

clean:
	rm -f *.o *.ii *.s *.opt *.sta *.log *.dbg *.cilk
