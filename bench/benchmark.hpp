#pragma once

#include <sys/time.h>
#include <sys/resource.h>
#include "parallel.h"

#include "cmdline.hpp"

__attribute__((constructor))
void initialize(int argc, char **argv) {
  deepsea::cmdline::set(argc, argv);
}

namespace pbbsBench {

template <class Bench>
void launch(const Bench& bench) {
  struct rusage ru_before, ru_after;
  struct timezone tzp({0,0});
  auto double_of_tv = [] (struct timeval tv) {
    return ((double) tv.tv_sec) + ((double) tv.tv_usec)/1000000.;
  };
  auto get_time = [&] {
    timeval now;
    gettimeofday(&now, &tzp);
    return double_of_tv(now);
  };
  auto st = get_time();
  getrusage (RUSAGE_SELF, &ru_before);
  bench();
  getrusage (RUSAGE_SELF, &ru_after);
  printf("exectime %.3lf\n", get_time() - st);
  printf("usertime  %.3lf\n",
	 double_of_tv(ru_after.ru_utime) -
	 double_of_tv(ru_before.ru_utime));
  printf("systime  %.3lf\n",
	 double_of_tv(ru_after.ru_stime) -
	 double_of_tv(ru_before.ru_stime));
  int num_workers = fj.num_workers();
  double stealtime = 0.0;
  for (int i = 0; i < num_workers; i++) {
    stealtime += time_in_get_job[i].e;
  }
  printf("stealtime %.3lf\n", stealtime);
}

}
