#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <iostream>
#include <assert.h>

#include "benchmark.hpp"
#include "uts.h"
#include "parallel.h"
#include "utilities.h"

// ==========================================================================
// These definitions are expected by uts.h

void impl_abort(int err) { exit(err); }
const char *impl_getName() { return "mini-uts parallel"; }
int impl_paramsToStr(char *strBuf, int ind) {
  ind += sprintf(strBuf+ind, "Execution strategy:  %s\n", impl_getName());
  return ind;
}
int impl_parseParam(char *param, char *value) {
  // Not using UTS command line params, return non-success
  return 1;
}
void impl_helpMessage() { printf("   none.\n"); }

// ==========================================================================

typedef struct {
  counter_t maxdepth, size, leaves;
} Result;

Result treeSearch(UTSConfig *config, int depth, Node *parent) {
  int numChildren, childType;
  counter_t parentHeight = parent->height;

  Result r;
  r.maxdepth = depth;
  r.size = 1;
  r.leaves = 0;

  numChildren = uts_numChildren(config, parent);
  childType   = uts_childType(config, parent);

  // record number of children in parent
  parent->numChildren = numChildren;

  // Recurse on the children
  if (numChildren == 0) {
    r.leaves = 1;
    return r;
  }

  long granularity = (depth > 100) ? numChildren : 1;

  parallel_for(0, numChildren, [&] (long i) {
    Node child;
    child.type = childType;
    child.height = parentHeight + 1;
    child.numChildren = -1;    // not yet determined
    for (int j = 0; j < config->computeGranularity; j++) {
      rng_spawn(parent->state.state, child.state.state, i);
    }
    Result c = treeSearch(config, depth+1, &child);

    pbbs::write_max(&r.maxdepth, c.maxdepth, std::less<int>());
    pbbs::write_add(&r.size, c.size);
    pbbs::write_add(&r.leaves, c.leaves);
  }, granularity);

  return r;
}

// ===========================================================================

typedef struct tree_params {
  size_t size;
  size_t depth;
  size_t leaves;
  int argc;
  std::vector<std::string> argv;
} TreeParams;

// (T1L) Geometric [fixed]
// Tree size = 102181082, tree depth = 13, num leaves = 81746377 (80.00%)
TreeParams T1 = {
  .size = 102181082,
  .depth = 13,
  .leaves = 81746377,
  .argc = 11,
  .argv = {"ignore", "-t", "1", "-a", "3", "-d", "13", "-b", "4", "-r", "29"}
};

// (T2L) Geometric [cyclic]
// Tree size = 96793510, tree depth = 67, num leaves = 53791152 (55.57%)
TreeParams T2 = {
  .size = 96793510,
  .depth = 67,
  .leaves = 53791152,
  .argc = 11,
  .argv = {"ignore", "-t", "1", "-a", "2", "-d", "23", "-b", "7", "-r", "220"}
};

void hack_parseParams(UTSConfig *config, TreeParams *p) {
  int n = p->argc;
  char **argv = new char*[n];
  for (int i = 0; i < n; i++) {
    std::string a = p->argv[i];
    argv[i] = new char[a.length()+1];
    a.copy(argv[i], a.length());
    argv[i][a.length()] = '\0';
  }
  uts_parseParams(config, n, argv);
  for (int i = 0; i < n; i++) delete[] argv[i];
  delete[] argv;
}

// ===========================================================================

int main() {
  UTSConfig config;
  Node root;
  double t1, t2;

  int64_t tree = deepsea::cmdline::parse_or_default_int("t", 1);

  TreeParams *treeArgs;

  if (1 == tree) {
    treeArgs = &T1;
  } else if (2 == tree) {
    treeArgs = &T2;
  } else {
    std::cerr << "Unknown input tree " << tree << std::endl;
    return 1;
  }

  Result r;

  launch([&] {
    hack_parseParams(&config, treeArgs);
    uts_printParams(&config);
    uts_initRoot(&config, &root);
    t1 = uts_wctime();
  }, [&] {
    t2 = uts_wctime();
    uts_showStats(&config, 1, 0, t2-t1, r.size, r.leaves, r.maxdepth);
    assert(r.size == treeArgs->size);
    assert(r.leaves == treeArgs->leaves);
    assert(r.maxdepth == treeArgs->depth);
  }, [&] {
    r = treeSearch(&config, 0, &root);
  });

  return 0;
}
