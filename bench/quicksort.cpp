#include "benchmark.hpp"
#include "quicksort.h"
#include "sequenceIO.h"

using namespace std;
using namespace pbbs;

int main(int argc, char** argv) {
  using T = double;
  auto infile = deepsea::cmdline::parse_or_default_string("infile", "sequence.txt");
  auto D = benchIO::readSequenceFromFile(infile.c_str());
  sequence<T> in((T*)D.A, D.n);
  sequence<T> out;

  pbbsBench::launch([&] {
    out = p_quicksort(in, less<T>());
  });
  
  cout << "result " << out.size() << endl;
}
