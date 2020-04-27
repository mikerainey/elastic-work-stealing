#pragma once

#include "cmdline.hpp"
#include "parallel.h"

#ifdef CILK
#include "mcsl_fjnative.hpp"
__attribute__((constructor))
void initialize(int argc, char **argv) {
  deepsea::cmdline::set(argc, argv);
  unsigned nb_proc = deepsea::cmdline::parse_or_default_int("proc", 1);
  set_num_workers(nb_proc);
}
#endif
  
template <typename Bench_pre, typename Bench_post, typename Bench_body>
void launch(const Bench_pre& bench_pre,
            const Bench_post& bench_post,
            const Bench_body& bench_body) {
#if defined(MCSL)
  mcsl::launch(bench_pre, bench_post, bench_body);
#elif defined(CILK)
  bench_pre();
#ifdef CILK_RUNTIME_WITH_STATS
  __cilkg_take_snapshot_for_stats();
#endif
  auto start_time = mcsl::clock::now();
  bench_body();
  mcsl::aprintf("exectime %.3f\n", mcsl::clock::since(start_time));
#ifdef CILK_RUNTIME_WITH_STATS
  __cilkg_dump_encore_stats_to_stderr();
#endif
  bench_post();
#endif
}
