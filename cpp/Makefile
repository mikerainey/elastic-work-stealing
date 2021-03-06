CPP=g++
PBBSLIB_PATH=../../pbbslib/
PBBSBENCH_PATH=../../pbbsbench/
MINI_UTS_PATH=../../mini-uts/
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
	HWLOC_FLAGS+=-DMCSL_HAVE_HWLOC
endif

UTS_INCLUDE_FILES=\
	$(MINI_UTS_PATH)/uts.c \
	$(MINI_UTS_PATH)/rng/brg_sha1.c

# it's important for PBBSLIB_PATH to appear before MINI_UTS_PATH because
# both of them have files like parallel.h, utilities.h. GCC should guarantee
# that it searches for includes in left-to-right order
UTS_INCLUDE_PREFIX=\
	-I $(CMDLINE_PATH)/include \
	-I $(MCSL_INCLUDE_PATH) \
	-I $(PBBSLIB_PATH) \
	-I $(MINI_UTS_PATH)

INCLUDE_FILES=\
	$(wildcard *.hpp) \
	$(wildcard $(CMDLINE_PATH)/include/*.hpp) \
	$(wildcard $(MCSL_INCLUDE_PATH)/*.hpp) \
	$(wildcard $(PBBSLIB_PATH)/*.h) \
	$(wildcard $(PBBSLIB_PATH)/examples/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/common/*.h) \
	$(wildcard $(PBBSBENCH_PATH)/breadthFirstSearch/ndBFS/*.C) \
	$(wildcard $(PBBSBENCH_PATH)/convexHull/quickHull/*.C) \
	$(wildcard $(PBBSBENCH_PATH)/suffixArray/parallelKS/*.C)

INCLUDE_PREFIX=\
	-I $(CMDLINE_PATH)/include \
	-I $(MCSL_INCLUDE_PATH) \
	-I $(PBBSLIB_PATH) \
	-I $(PBBSLIB_PATH)/examples \
	-I $(PBBSBENCH_PATH) \
	-I $(PBBSBENCH_PATH)/common \
	-I $(PBBSBENCH_PATH)/suffixArray/parallelKS/

WARNINGS_PREFIX=-Wno-subobject-linkage -Wno-overflow -Wno-cpp -Wno-return-type
COMMON_PREFIX=--std=c++17 -pthread -mcx16 $(WARNINGS_PREFIX) $(HWLOC_FLAGS) $(HWLOC_CFLAGS) #-DUSEMALLOC
OPT_BASE_PREFIX=-O3 -m64 -march=native -DNDEBUG
DBG_BASE_PREFIX=-O0 -g3
LINKER_PREFIX=$(HWLOC_LDFLAGS) $(JEMALLOC)

# MCSL binaries
# -------------

MCSL_PREFIX=-DMCSL $(COMMON_PREFIX)
OPT_PREFIX=$(MCSL_PREFIX) $(OPT_BASE_PREFIX) $(INCLUDE_PREFIX)
OPT_ELASTICSPIN_PREFIX=-DMCSL_ELASTIC_SPINSLEEP $(OPT_PREFIX)
OPT_ELASTICOFF_PREFIX=-DMCSL_DISABLE_ELASTIC $(OPT_PREFIX)
STA_PREFIX=-DMCSL_ENABLE_STATS $(OPT_PREFIX)
STA_ELASTICSPIN_PREFIX=-DMCSL_ELASTIC_SPINSLEEP $(STA_PREFIX)
STA_ELASTICOFF_PREFIX=-DMCSL_DISABLE_ELASTIC $(STA_PREFIX)
LOG_PREFIX=-DMCSL_ENABLE_LOGGING $(STA_PREFIX)
DBG_PREFIX=-DMCSL_ENABLE_STATS -DMCSL_ENABLE_LOGGING $(MCSL_PREFIX) $(DBG_BASE_PREFIX) $(INCLUDE_PREFIX)

uts.opt: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) $(MCSL_PREFIX) $(OPT_BASE_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.opt_elasticspin: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) $(MCSL_PREFIX) $(OPT_ELASTICSPIN_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.opt_elasticoff: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) $(MCSL_PREFIX) $(OPT_ELASTICOFF_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.sta: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) -DMCSL_ENABLE_STATS $(MCSL_PREFIX) $(OPT_BASE_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.sta_elasticspin: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) -DMCSL_ENABLE_STATS $(MCSL_PREFIX) $(OPT_ELASTICSPIN_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.sta_elasticoff: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) -DMCSL_ENABLE_STATS $(MCSL_PREFIX) $(OPT_ELASTICOFF_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.log: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) -DMCSL_ENABLE_LOGGING -DMCSL_ENABLE_STATS $(MCSL_PREFIX) $(OPT_BASE_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

uts.dbg: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) -DMCSL_ENABLE_STATS -DMCSL_ENABLE_LOGGING $(MCSL_PREFIX) $(DBG_BASE_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

%.opt: %.cpp $(INCLUDE_FILES)
	$(CPP) $(OPT_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.opt_elasticspin: %.cpp $(INCLUDE_FILES)
	$(CPP) $(OPT_ELASTICSPIN_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.opt_elasticoff: %.cpp $(INCLUDE_FILES)
	$(CPP) $(OPT_ELASTICOFF_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.sta: %.cpp $(INCLUDE_FILES)
	$(CPP) $(STA_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.sta_elasticspin: %.cpp $(INCLUDE_FILES)
	$(CPP) $(STA_ELASTICSPIN_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.sta_elasticoff: %.cpp $(INCLUDE_FILES)
	$(CPP) $(STA_ELASTICOFF_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.log: %.cpp $(INCLUDE_FILES)
	$(CPP) $(LOG_PREFIX) -o $@ $< $(LINKER_PREFIX)

%.dbg: %.cpp $(INCLUDE_FILES)
	$(CPP) $(DBG_PREFIX) -o $@ $< $(LINKER_PREFIX)

# Cilk Plus binaries
# ------------------

CILK_BASE_PREFIX=-fcilkplus -lcilkrts -DCILK -ldl -Wno-format-zero-length
CILK_PREFIX=$(COMMON_PREFIX) $(OPT_BASE_PREFIX) $(CILK_BASE_PREFIX) $(CILK_EXTRAS_PREFIX) $(INCLUDE_PREFIX)

uts.cilk: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) $(COMMON_PREFIX) $(OPT_BASE_PREFIX) $(CILK_BASE_PREFIX) $(CILK_EXTRAS_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

%.cilk: %.cpp $(INCLUDE_FILES)
	$(CPP) $(CILK_PREFIX) -o $@ $< $(LINKER_PREFIX)

# Homegrown scheduler binaries
# ----------------------------

HOMEGROWN_PREFIX=$(COMMON_PREFIX) $(OPT_BASE_PREFIX) -DHOMEGROWN $(INCLUDE_PREFIX)

uts.hg: uts.cpp $(UTS_INCLUDE_FILES)
	$(CPP) $(COMMON_PREFIX) $(OPT_BASE_PREFIX) $(HOMEGROWN_PREFIX) $(UTS_INCLUDE_PREFIX) -o $@ uts.cpp $(UTS_INCLUDE_FILES) $(LINKER_PREFIX) -lm

%.hg: %.cpp $(INCLUDE_FILES)
	$(CPP) $(HOMEGROWN_PREFIX) -o $@ $< $(LINKER_PREFIX)

# Installation and cleanup
# ------------------------

BENCHMARKS=\
	fib \
	tabulate \
	reduce \
	scan \
	filter \
	grep \
	samplesort \
	quicksort \
	quickhull \
	bfs \
	suffixarray

all: \
	$(BENCHMARKS:%=%.opt) $(BENCHMARKS:%=%.opt_elasticspin) $(BENCHMARKS:%=%.opt_elasticoff) \
	$(BENCHMARKS:%=%.sta) $(BENCHMARKS:%=%.sta_elasticspin) $(BENCHMARKS:%=%.sta_elasticoff) \
	$(BENCHMARKS:%=%.cilk)

install:
	cp *.opt *.opt_elasticspin *.opt_elasticoff *.sta *.sta_elasticspin *.sta_elasticoff *.cilk run $(INSTALL_FOLDER)

clean:
	rm -f *.o *.ii *.s *.opt *.opt_elasticspin *.opt_elasticoff *.sta *.sta_elasticspin *.sta_elasticoff *.log *.dbg *.cilk *.hg vgcore.* LOG_BIN
