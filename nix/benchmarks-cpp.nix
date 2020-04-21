{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  sources ? import ./local-sources.nix,
  dfltSrc ? sources.benchmarksCppSrc,
  mcsl ? import sources.mcsl {},
  pbbslib ? import sources.pbbslib {},
  pbbsbench ? import sources.pbbsbench { pbbslib = pbbslib; },
  cmdline ? import sources.cmdline {},
  cilkRtsWithStats ? import sources.cilkRtsWithStats {},
  gcc ? pkgs.gcc7,
  which ? pkgs.which,
  hwloc ? pkgs.hwloc, # use hwloc, unless this parameter equals null
  jemalloc ? pkgs.jemalloc450 # use jemalloc, unless this parameter equals null (for now, use v4.5.0, because 5.1.0 has a deadlock bug)
}:

stdenv.mkDerivation rec {
  name = "benchmarks-cpp";
  
  src = dfltSrc;
  
  buildInputs =
    [ gcc which ]
    ++ (if hwloc == null then [] else [ hwloc ])
    ++ (if jemalloc == null then [] else [ jemalloc ]);
  
  buildPhase =
    let hwlocFlgs =
          if hwloc == null then
            ""
          else
            ''USE_HWLOC=1 \
              HWLOC_CFLAGS="-I ${hwloc.dev}/include/" \
              HWLOC_LDFLAGS="-L ${hwloc.lib}/lib/ -lhwloc"
            '';
        jemallocCfg = 
          if jemalloc == null then
            ""
          else
            "export PATH=${jemalloc}/bin:$PATH";
    in
    ''
    ${jemallocCfg}
    make clean
    make \
      all \
      CMDLINE_PATH=${cmdline} \
      PBBSLIB_PATH=${pbbslib} \
      PBBSBENCH_PATH=${pbbsbench} \
      MCSL_INCLUDE_PATH=${mcsl}/include \
      CILK_EXTRAS_PREFIX="-L ${cilkRtsWithStats}/lib -I ${cilkRtsWithStats}/include -ldl -DCILK_RUNTIME_WITH_STATS" \
      CPP=${gcc}/bin/g++ \
      ${hwlocFlgs}
    '';
  
  installPhase = ''
    mkdir -p $out

    make install \
      INSTALL_FOLDER=$out

    mv $out/run $out/run-CPP

    ln -s ${pbbslib} $out/pbbslib
    ln -s ${pbbslib.test} $out/pbbslib-test
    ln -s ${pbbslib.examples} $out/pbbslib-examples
    ln -s ${pbbsbench} $out/pbbsbench
  '';
  
}   
