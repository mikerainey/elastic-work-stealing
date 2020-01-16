{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv
}:

stdenv.mkDerivation rec {
  name = "cpufreq-scripts";

  src = ../cpufreq;

  installPhase = ''
    mkdir -p $out
    cp *.sh $out/
  '';

}
