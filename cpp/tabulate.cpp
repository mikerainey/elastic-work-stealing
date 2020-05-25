#include "benchmark.hpp"
#include "sequence.h"

using namespace std;
using namespace pbbs;

int main(int argc, char** argv) {
  using T = double;
  size_t n = max((size_t)1, (size_t)deepsea::cmdline::parse_or_default_long("n", 100000000));
  T result;
  launch([&] {
    for (int i = 0; i < 5; i++) { // warmup
      sequence<T> A(n, [] (size_t i) { return T(i); });
    }
  },
  [&] {
    cout << "result " << result << endl;
  }, [&] {
    sequence<T> A(n, [] (size_t i) { return T(i); });
    result = (T)A[n/2];
  });

  return 0;
}
