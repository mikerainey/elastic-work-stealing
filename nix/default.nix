{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  makeWrapper ? pkgs.makeWrapper,
  buildDunePackage ? pkgs.ocamlPackages.buildDunePackage,
  sources ? import ./local-sources.nix,
  benchmarksCpp ? import sources.benchmarksCpp { sources = sources; },
  benchScript ? import sources.benchScript { sources = sources; }
}:

stdenv.mkDerivation rec {
  name = "benchmark";

  src = "${sources.nixSrc}/dummy";

  buildInputs = [ makeWrapper ];

  configurePhase =
  let pbenchScript = "${benchScript}/pbench";
  in
  ''
  cp ${pbenchScript} pbench
  '';

  installPhase = ''
    mkdir -p $out
    cp pbench $out/pbench
    wrapProgram $out/pbench \
      --prefix PATH ":" $out/ \
      --prefix PATH ":" ${benchmarksCpp} \
      --add-flags "-skip make"
    ln -s ${benchmarksCpp} $out/benchmarksCpp
    '';

}

