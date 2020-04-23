{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  makeWrapper ? pkgs.makeWrapper,
  sources ? import ./local-sources.nix,
  dfltSrc ? sources.inputDataSrc,
  pbbslib ? import sources.pbbslib {},
  pbbsbench ? import sources.pbbsbench { pbbslib = pbbslib; },
  cmdline ? import sources.cmdline {},
  gcc ? pkgs.gcc7,
  which ? pkgs.which,
  jemalloc ? pkgs.jemalloc # use jemalloc, unless this parameter equals null
}:

stdenv.mkDerivation rec {
  name = "input-data";
  
  src = dfltSrc;
  
  buildInputs =
    [ gcc which makeWrapper ]
    ++ (if jemalloc == null then [] else [ jemalloc ]);

  enableParallelBuilding = true;
  
  buildPhase =
    let jemallocCfg = 
          if jemalloc == null then
            ""
          else
            "export PATH=${jemalloc}/bin:$PATH";
    in
    ''
    ${jemallocCfg}
    make clean
    make -j $NIX_BUILD_CORES \
      all \
      CMDLINE_PATH=${cmdline} \
      PBBSLIB_PATH=${pbbslib} \
      PBBSBENCH_PATH=${pbbsbench} \
      CPP=${gcc}/bin/g++
    '';

  installPhase = ''
    mkdir -p $out

    make install \
         INSTALL_FOLDER=$out

    # Test-data generation
    wrapProgram $out/run-rMatGraph \
      --prefix PATH ":" ${pbbsbench.testData}/graphData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-parallelPathsGraph \
      --prefix PATH ":" ${pbbsbench.testData}/graphData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-phasedGraph \
      --prefix PATH ":" ${pbbsbench.testData}/graphData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-gridGraph \
      --prefix PATH ":" ${pbbsbench.testData}/graphData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-randomSeq \
      --prefix PATH ":" ${pbbsbench.testData}/sequenceData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-exptSeq \
      --prefix PATH ":" ${pbbsbench.testData}/sequenceData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-almostSortedSeq \
      --prefix PATH ":" ${pbbsbench.testData}/sequenceData \
      --prefix PATH ":" $out/
    wrapProgram $out/run-randPoints \
      --prefix PATH ":" ${pbbsbench.testData}/geometryData \
      --prefix PATH ":" $out/

    ln -s ${pbbsbench.testData} $out/pbbsbench-testData
  '';
  
}   
