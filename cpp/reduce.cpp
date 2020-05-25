#include "benchmark.hpp"
#include "sequence.h"

using namespace std;
using namespace pbbs;

int main(int argc, char** argv) {
  using T = double;
  size_t n = max((size_t)1, (size_t)deepsea::cmdline::parse_or_default_long("n", 100000000));
  sequence<T> A;
  T tot;
  launch([&] {
    A = sequence<T>(n, T(1));
  },
  [&] {
    cout << "result " << tot << endl;
  }, [&] {
    tot = reduce(A, addm<T>());
  });

  return 0;
}
