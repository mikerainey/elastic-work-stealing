#include "benchmark.hpp"
#include "graphIO.h"
#include "sequence.h"
#include "graph.h"
#include "parallel.h"
#include <limits>
using namespace std;
using namespace pbbs;

// **************************************************************
//    NON DETERMINISTIC BREADTH FIRST SEARCH
// **************************************************************

template <class intV = long, class intE = intV>
std::pair<intV,size_t> BFS(intV start, graph<intV, intE> &G) {
  using vertexId = intV;
  using edgeId = intE;
  vertexId numVertices = G.numVertices();
  edgeId numEdges = G.m;
  vertexId maxIdx = std::numeric_limits<vertexId>::max();
  
  sequence<edgeId> Offsets(numVertices+1);
  sequence<bool> Visited(numVertices, false);
  sequence<vertexId> Frontier(1, start);

  Visited[start] = true;
  size_t round = 0;
  vertexId totalVisited = 0;
  
  while (Frontier.size() > 0) {
    totalVisited += Frontier.size();
    round++;

    parallel_for (0, Frontier.size(), [&] (size_t i) {
	Offsets[i] = G[Frontier[i]].degree; });
    
    // Find offsets to write the next frontier for each v in this frontier
    size_t nr = scan_inplace(Offsets.slice(0,Frontier.size()), addm<edgeId>());
    Offsets[Frontier.size()] = nr;
    sequence<vertexId> FrontierNext(nr);

    // For each vertex in the frontier try to "hook" unvisited neighbors.
    parallel_for (0, Frontier.size(), [&] (size_t i) {
	size_t k = 0;
	vertexId v = Frontier[i];
	edgeId o = Offsets[i];
	for (size_t j=0; j < G[v].degree; j++) {
	  vertexId ngh = G[v].Neighbors[j];
	  if (!Visited[ngh] && atomic_compare_and_swap(&Visited[ngh], false, true)) {
	    FrontierNext[o+j] = ngh;
	  }
	  else FrontierNext[o+j] = -1;
	}
      });

    // Filter out the empty slots (marked with -1)
    Frontier = filter(FrontierNext, [&] (vertexId x) {return x >= 0;});
  }

  return std::pair<vertexId,size_t>(totalVisited,round);
}

int main() {
  using T = long;
  auto infile = deepsea::cmdline::parse_or_default_string("infile", "graph.adj");
  auto source = (T)deepsea::cmdline::parse_or_default_long("source", 0);
  sequence<T> o0(1);
  sequence<T> e0;
  graph<T> G(o0, e0, 0);
  size_t levels, visited;
  launch([&] {
    G = benchIO::readGraphFromFile<T>((char*)infile.c_str());
    G.addDegrees(); // needed for BFS.. it'll segfault otherwise!           
  }, [&] {
    cout << "levels " << levels << endl;
    cout << "visited " << visited << endl;
  }, [&] {
    tie(visited, levels) = BFS(source, G);
  });
  return 0;
}
