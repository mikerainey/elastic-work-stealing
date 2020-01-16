# Opens a nix shell after raising the frequency of the system
# cores to the maximum. The original settings are restored by
# exiting from the shell.

{
  pkgs ? import <nixpkgs> {},
  scripts ? import ./cpufreq-scripts.nix {}
}:

pkgs.stdenv.mkDerivation rec {
  name = "cpufreq-max";
  
  src = ./.;

  buildInputs = [ pkgs.cpufrequtils pkgs.hwloc ];

  shellHook = ''
    set -e
    temp_file=$(mktemp)
    ${scripts}/capture.sh $temp_file
    ${scripts}/raise.sh $temp_file
    cleanup() {
      ${scripts}/restore.sh $temp_file
      rm $temp_file
    }
    trap cleanup EXIT
  '';

}
