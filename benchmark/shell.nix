{ pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  parlaylibSrc ? ./../../parlaylib,
  taskparts ? import ./../../nix-packages/pkgs/taskparts/default.nix {stdenv=pkgs.stdenv;cmake=pkgs.cmake;taskpartsSrc=./../../successor;hwloc=pkgs.hwloc;},
  taskparts-nonelastic ? import ./../../nix-packages/pkgs/taskparts/default.nix {stdenv=pkgs.stdenv;cmake=pkgs.cmake;taskpartsSrc=./../../successor; hwloc=null; },
  parlay-homegrown ? import ./../../nix-packages/pkgs/parlaylib/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;},
  parlay-serial ? import ./../../nix-packages/pkgs/parlaylib-serial/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;},
  parlay-taskparts ? import ./../../nix-packages/pkgs/parlaylib-taskparts/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;taskparts=taskparts;},
  parlay-taskparts-nonelastic ? import ./../../nix-packages/pkgs/parlaylib-taskparts/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;taskparts=taskparts-nonelastic;}
}:

let
  customPython = pkgs.python310.buildEnv.override {
    extraLibs = [ pkgs.python310Packages.jsonschema
                  pkgs.python310Packages.simplejson
                  pkgs.python310Packages.psutil
                  pkgs.python310Packages.py-cpuinfo
                  pkgs.python310Packages.sqlalchemy
                  pkgs.python310Packages.python-sql
                  (with import <nixpkgs> {}; pkgs.python310Packages.callPackage ../../flexibench {})
                ];
  };
in

# to use jemalloc: prefix the benchmark with
#   LD_PRELOAD=`jemalloc-config --libdir`/libjemalloc.so.`jemalloc-config --revision`

stdenv.mkDerivation rec {
  name = "elastic-benchmark";
  buildInputs = [ customPython pkgs.dsq pkgs.jq pkgs.jemalloc ];
  PARLAY_HOMEGROWN="${parlay-homegrown}/examples";
  PARLAY_SERIAL="${parlay-serial}/examples";
  PARLAY_TASKPARTS="${parlay-taskparts}/examples";
  PARLAY_TASKPARTS_NONELASTIC="${parlay-taskparts-nonelastic}/examples";
  JEMALLOC_PRELOAD_PATH="${pkgs.jemalloc}/lib/libjemalloc.so";
  INFILES_PATH="../../infiles";
}
