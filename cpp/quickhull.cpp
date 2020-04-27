#include "benchmark.hpp"
#include "convexHull/quickHull/hull.C"
#include "geometryIO.h"
#include "sequenceIO.h"

using namespace std;
using namespace pbbs;

double randDouble(uint64_t seed) {
  uint64_t resolution = 1000000;
  return (double)(hash64(seed) % resolution) / ((double)resolution);
}

point2d<double> randPt(uint64_t seed) {
  const double pi = 3.14159265358979323846;
  double r = sqrt(randDouble(2*seed));
  double theta = randDouble(2*seed+1) * 2.0 * pi;
  return point2d<double>(1.0 + r * cos(theta), 1.0 + r * sin(theta));
}

int main() {
  using point = point2d<double>;
  string infile = deepsea::cmdline::parse_or_default_string("infile", "");
  string outfile = deepsea::cmdline::parse_or_default_string("outfile", "");
  size_t n = max((size_t)1, (size_t)deepsea::cmdline::parse_or_default_long("n", 100000000));

  sequence<point> Points;

  sequence<indexT> I;
  launch([&] {
    if (infile != "") {
      Points = readPointsFromFile<point>(infile.c_str());
      n = Points.size();
    } else {
      Points = tabulate(n, [] (size_t i) { return randPt(i); });
    }
  }, [&] {
    if (outfile != "") {
      writeSequenceToFile(I, outfile.c_str());
    }
  }, [&] {
    I = hull(Points);
  });
  cout << "result " << I.size() << endl;

  return 0;
}
