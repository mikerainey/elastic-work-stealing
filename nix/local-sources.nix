let pkgs = import <nixpkgs> {};
    projectSrc = ../.;
    cmdlineSrc = pkgs.fetchFromGitHub {
      owner  = "deepsea-inria";
      repo   = "cmdline";
      rev    = "c5f96b4aecb2019b5a690176195d37f7df3ed34b";
      sha256 = "1rz9bfdd5242gy3vq4n9vj2rcr5pwp0j4cjycpn3pm7rnwrrcjnh";
    };
    cilkRtsWithStatsSrc = pkgs.fetchFromGitHub {
      owner  = "deepsea-inria";
      repo   = "cilk-plus-rts-with-stats";
      rev    = "d143c31554bc9c122d168ec22ed65e7941d4c91d";
      sha256 = "123bsrqcp6kq6xz2rn4bvj2nifflfci7rd9ij82fpi2x6xvvsmsb";
    };
    pbenchSrc = pkgs.fetchFromGitHub {
      owner  = "mikerainey";
      repo   = "pbench";
      rev    = "1c90259b594b6612bc6b9973564e89c297ad17b3";
      sha256 = "1440zavl3v74hcyg49h026vghhj1rv5lhfsb5rgfzmndfynzz7z0";
    };
    pbenchOcamlSrcs = import "${pbenchSrc}/nix/local-sources.nix";
    pbbslibSrc = ../../pbbslib;
    pbbsbenchSrc = ../../pbbsbench;
in

let
  nixSrc = "${projectSrc}/nix";
in

{
  nixSrc = nixSrc;

  # External package dependencies for C++ benchmarks
  cmdline = "${cmdlineSrc}/script/default.nix";
  pbbslib = "${pbbslibSrc}/default.nix";
  pbbsbench = "${pbbsbenchSrc}/default.nix";
  cilkRtsWithStats = "${cilkRtsWithStatsSrc}/default.nix";

  # Benchmarking script
  pbenchOcamlSrcs = pbenchOcamlSrcs;
  benchOcamlSrc = "${projectSrc}/bench/";
  benchScript = "${nixSrc}/bench-script.nix";

}
