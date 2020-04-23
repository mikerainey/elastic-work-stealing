#include <assert.h>
#include "cmdline.hpp"
#include "dataGen.h"
#include "IO.h"

using namespace std;
using namespace pbbs;

auto generateTestData(size_t nb_rows, size_t row_len, size_t nb_occurrences, std::string const& pattern)
  -> sequence<char> {
  size_t pat_len = pattern.size();
  assert(pat_len <= row_len); assert(nb_occurrences <= nb_rows);
  sequence<char> r(nb_rows * (row_len+1), [&] (size_t i) { return (dataGen::hash<int>(i) % 94)+33; });
  parallel_for(0, nb_rows, [&] (size_t i) {
    r[(i*(row_len+1)) + row_len] = '\n';
  });
  parallel_for(0, nb_occurrences, [&] (size_t i) {
    int j = dataGen::hash<int>(i) % ((row_len - pat_len)+1);
    pattern.copy(&r[(i*(row_len+1)) + j], pat_len);
  });
  return r;
}

int main(int argc, char** argv) {
  deepsea::cmdline::set(argc, argv);
  auto outfile = deepsea::cmdline::parse_string("outfile");
  size_t nb_rows = deepsea::cmdline::parse_or_default_long("nb_rows", 5);
  size_t row_len = deepsea::cmdline::parse_or_default_long("row_len", 16);
  size_t nb_occurrences = deepsea::cmdline::parse_or_default_long("nb_occurrences", 3);
  std::string pattern = deepsea::cmdline::parse_or_default_string("pattern", "xxy");
  auto testData = generateTestData(nb_rows, row_len, nb_occurrences, pattern);
  auto fileName = outfile.c_str();
  {
    ofstream file (fileName, ios::out | ios::binary);
    if (!file.is_open()) {
      std::cout << "Unable to open file: " << fileName << std::endl;
      return 1;
    }
    file.write(testData.begin(), testData.size());
    file.close();
  }
  printf("exectime 0.0\n");
}
