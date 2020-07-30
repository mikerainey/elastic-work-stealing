#include <iostream>
#include <algorithm>
#include "benchmark.hpp"
#include "SA.C"
#include "IO.h"
#include "sequenceIO.h"
#include "sequence_ops.h"
#include "sequence.h"

using namespace std;
using namespace pbbs;

using uchar = unsigned char;

int main(int argc, char** argv) {
  pbbs::sequence<uchar> ss;
  pbbs::sequence<unsigned int> R;
  std::size_t n;
  launch([&] {
    auto infile = deepsea::cmdline::parse_or_default_string("infile", "chr22.dna");
    pbbs::sequence<char> S = benchIO::readStringFromFile(infile.c_str());
    n = S.size();
    ss = pbbs::sequence<uchar>(n, [&] (size_t i) {return (uchar) S[i];});
  }, [&] {

  }, [&] {
    R = suffixArray(ss);
  });

  return 0;
}
