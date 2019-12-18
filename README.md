# Elastic work stealing

## Manual setup

Clone this repository and a number of others, as follows:
```
$ git clone git@github.com:mikerainey/elastic-work-stealing
$ git clone git@github.com:mikerainey/pbbslib.git -b elastic-work-stealing
$ git clone git@github.com:mikerainey/pbbsbench.git
$ git clone git@github.com:deepsea-inria/cmdline.git
```

## Building benchmarks

To build `quicksort`, for example, change to the benchmark folder and call `make`.
````
$ cd elastic-work-stealing/elastic-work-stealing/bench
$ make quicksort.hg.bin
````

## Running benchmarks

For quicksort, first we will need a file containing a PBBS sequence of double precision floats. Suppose that our sequence file is named `sequence.txt`. Then, to invoke `quicksort`, we can run with that input as follows.

````
$ ./quicksort.hg.bin -infile sequence.txt
````
