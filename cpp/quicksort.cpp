#include "benchmark.hpp"
#include "quicksort.h"
#include "sequenceIO.h"

using namespace std;
using namespace pbbs;

int main(int argc, char** argv) {
  using T = double;
  long cutsize = deepsea::cmdline::parse_or_default_int("cutsize", 1<<10);
  auto infile = deepsea::cmdline::parse_or_default_string("infile", "sequence.txt");
  auto D = benchIO::readSequenceFromFile(infile.c_str());
  sequence<T> in((T*)D.A, D.n);

  pbbsBench::launch([&] {
    quicksort(in.begin(), D.n, less<T>(), cutsize);
  });
  
  cout << "result " << *in.begin() << endl;
}
