#include <iostream>
#include <chrono>
#include <ctime>

#include "mcsl_fjnative.hpp"

int64_t fib_seq(int64_t n) {
  if (n <= 1) {
    return n;
  } else {
    return fib_seq(n-1) + fib_seq(n-2);
  }
}

int64_t cutoff;

int64_t fib_fjnative(int64_t n) {
  if (n <= cutoff) {
    return n;
  } else {
    int64_t r1, r2;
    mcsl::fork2([&] {
      r1 = fib_fjnative(n-1);
    }, [&] {
      r2 = fib_fjnative(n-2);
    });
    return r1 + r2;
  }
}

int main(int argc, char** argv) {
  int64_t n = deepsea::cmdline::parse_or_default_int("n", 30);
  int64_t dst = 0;
  cutoff = deepsea::cmdline::parse_or_default_int("cutoff", 1);
  mcsl::launch([&] { /* no initialization needed */ },
  [&] {
    assert(fib_seq(n) == dst);
    printf("result %ld\n", dst);
  }, [&] {
     dst = fib_fjnative(n);
  });
  return 0;
}
