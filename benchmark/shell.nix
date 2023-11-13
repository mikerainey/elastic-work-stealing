{ pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.llvmPackages_14.stdenv,
  parlaylibSrc ? ./../../parlaylib,
  taskpartsSrc ? ./../../successor,
  few ? false,
  taskparts ? import ./../../nix-packages/pkgs/taskparts/hdronly.nix {stdenv=pkgs.stdenv;cmake=pkgs.cmake;taskpartsSrc=taskpartsSrc; },
  taskparts-nonelastic ? import ./../../nix-packages/pkgs/taskparts/hdronly.nix {stdenv=pkgs.stdenv;cmake=pkgs.cmake;taskpartsSrc=taskpartsSrc; disable-elastic-scheduling = true; },
  taskparts-ywra ? import ./../../nix-packages/pkgs/taskparts/hdronly.nix {stdenv=pkgs.stdenv;cmake=pkgs.cmake;taskpartsSrc=taskpartsSrc; disable-elastic-scheduling = true; use-ywra-deque=true; },
  parlay-homegrown ? import ./../../nix-packages/pkgs/parlaylib/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc; few=few;},
  parlay-serial ? import ./../../nix-packages/pkgs/parlaylib-serial/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc; few=few;},
  parlay-taskparts ? import ./../../nix-packages/pkgs/parlaylib-taskparts/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;taskparts=taskparts; few=few;},
  parlay-taskparts-nonelastic ? import ./../../nix-packages/pkgs/parlaylib-taskparts/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;taskparts=taskparts-nonelastic; few=few;},
  parlay-taskparts-ywra ? import ./../../nix-packages/pkgs/parlaylib-taskparts/default-examples.nix {stdenv=stdenv;fetchgit=pkgs.fetchgit;cmake=pkgs.cmake;parlaylibSrc=parlaylibSrc;taskparts=taskparts-ywra; few=few; }
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
  PARLAY_TASKPARTS_YWRA="${parlay-taskparts-ywra}/examples";
  PARLAY_OPENCILK="../../parlaylib-opencilk/examples";
  JEMALLOC_PRELOAD_PATH="${pkgs.jemalloc}/lib/libjemalloc.so";
  INFILES_PATH="../../infiles";
}
