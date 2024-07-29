{ pkgs ? import <nixpkgs> {},
  #  stdenv ? pkgs.llvmPackages_14.stdenv,
  stdenv ? pkgs.stdenv,
  lib ? pkgs.lib,
  cmake ? pkgs.cmake,
  fetchgit ? pkgs.fetchgit,
  flexibenchSrc ? ./../../flexibench,
  few ? false,
  taskpartsSrc ? ./../../successor,
  parlaylib-homegrown ? import ../../nix-packages/pkgs/parlaylib {inherit stdenv lib cmake fetchgit;
                                                                  parlayExamples = true; parlayInstallExamples = true;
                                                                  parlayFewExamples = few;
                                                                 },
  parlaylib-taskparts ? import ../../nix-packages/pkgs/parlaylib {inherit stdenv lib cmake fetchgit;
                                                                  parlayExamples = true; parlayInstallExamples = true;
                                                                  parlayFewExamples = few;
                                                                  taskparts = import ../../nix-packages/pkgs/taskparts {inherit stdenv lib cmake taskpartsSrc; statsEnable = true;};
                                                                 },
  parlaylib-homegrown-ne ? import ../../nix-packages/pkgs/parlaylib {inherit stdenv lib cmake fetchgit;
                                                                  parlayExamples = true; parlayInstallExamples = true;
                                                                  parlayFewExamples = few;
                                                                  parlayDisableElasticParallelism = true;
                                                                 },
  parlaylib-taskparts-ne ? import ../../nix-packages/pkgs/parlaylib {inherit stdenv lib cmake fetchgit;
                                                                  parlayExamples = true; parlayInstallExamples = true;
                                                                  parlayFewExamples = few;
                                                                  taskparts = import ../../nix-packages/pkgs/taskparts {inherit stdenv lib cmake taskpartsSrc; statsEnable = true; disableElasticParallelism = true;};
                                                                 }
}:

let
  customPython = pkgs.python310.buildEnv.override {
    extraLibs = [ pkgs.python310Packages.jsonschema
                  pkgs.python310Packages.simplejson
                  pkgs.python310Packages.psutil
                  pkgs.python310Packages.py-cpuinfo
                  (with import <nixpkgs> {}; pkgs.python310Packages.callPackage ../../nix-packages/pkgs/flexibench {flexibenchSrc = flexibenchSrc;})
                ];
  };
in

# to use jemalloc: prefix the benchmark with
#   LD_PRELOAD=`jemalloc-config --libdir`/libjemalloc.so.`jemalloc-config --revision`

stdenv.mkDerivation rec {
  name = "elastic-benchmark";
  buildInputs = [ customPython pkgs.jemalloc ];
  JEMALLOC_PRELOAD_PATH="${pkgs.jemalloc}/lib/libjemalloc.so";
  PARLAY_HOMEGROWN="${parlaylib-homegrown}/share/examples/bin";
  PARLAY_TASKPARTS="${parlaylib-taskparts}/share/examples/bin";
  PARLAY_HOMEGROWN_NONELASTIC="${parlaylib-homegrown-ne}/share/examples/bin";
  PARLAY_TASKPARTS_NONELASTIC="${parlaylib-taskparts-ne}/share/examples/bin";
  INFILES_PATH="../../infiles";
}

  ##########################################################################
  #   PARLAY_HOMEGROWN="${parlay-homegrown}/examples";                     #
  # PARLAY_SERIAL="${parlay-serial}/examples";                             #
  # PARLAY_TASKPARTS="${parlay-taskparts}/examples";                       #
  # PARLAY_TASKPARTS_NONELASTIC="${parlay-taskparts-nonelastic}/examples"; #
  # PARLAY_TASKPARTS_YWRA="${parlay-taskparts-ywra}/examples";             #
  # PARLAY_OPENCILK="../../parlaylib-opencilk/examples";                   #
  ##########################################################################
