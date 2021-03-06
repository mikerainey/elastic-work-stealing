#include "benchmark.hpp"
#include "sequence.h"

using namespace std;
using namespace pbbs;

int main(int argc, char** argv) {
  using T = double;
  size_t n = max((size_t)1, (size_t)deepsea::cmdline::parse_or_default_long("n", 100000000));
  sequence<T> A;
  sequence<T> B;
  T tot;
  launch([&] {
    for (int i = 0; i < 5; i++) { // warmup
      sequence<T> C(n, [] (size_t i) { return T(i); });
    }
    A = sequence<T>(n, [] (size_t i) { return T(i % 2); });
  },
  [&] {
    cout << "result " << B[n/4] << endl;
  }, [&] {
     B = filter(A, [] (T v) { return v == 1.0; });
  });

  return 0;
}
