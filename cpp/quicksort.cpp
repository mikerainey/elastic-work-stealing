#include "benchmark.hpp"
#include "quicksort.h"
#include "sequenceIO.h"

using namespace std;
using namespace pbbs;

int main () {
  using T = int;
  long cutsize = deepsea::cmdline::parse_or_default_int("cutsize", 1<<10);
  auto infile = deepsea::cmdline::parse_or_default_string("infile", "grep.txt");
  char* filename = (char*)infile.c_str();
  sequence<T> in;
  launch([&] {
    auto D = benchIO::readSequenceFromFile(infile.c_str());
    in = sequence<T>((T*)D.A, D.n);
  },
  [&] {
    cout << "result " << *in.begin() << endl;
  }, [&] {
    quicksort(in.begin(), in.size(), less<T>(), cutsize);
  });
  return 0;
}

