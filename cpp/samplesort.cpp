#include "benchmark.hpp"
#include "sample_sort.h"
#include "sequenceIO.h"

using namespace std;
using namespace pbbs;

int main() {
  using T = double;
  auto infile = deepsea::cmdline::parse_or_default_string("infile", "doubles.txt");
  sequence<T> in;
  sequence<T> out;
  launch([&] {
    auto D = benchIO::readSequenceFromFile(infile.c_str());
    in = sequence<T>((T*)D.A, D.n);
  }, [&] {
    cout << "result " << out.size() << endl;
  }, [&] {
    out = sample_sort(in, less<T>());
  });
  return 0;
}
