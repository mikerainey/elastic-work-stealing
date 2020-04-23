let pkgs = import <nixpkgs> {};
    projectSrc = ../.;
    cmdlineSrc = pkgs.fetchFromGitHub {
      owner  = "deepsea-inria";
      repo   = "cmdline";
      rev    = "67b01773169de11bf04253347dd1087e5863a874";
      sha256 = "1bzmxdmnp7kn6arv3cy0h4a6xk03y7wdg0mdkayqv5qsisppazmg";
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
    mcslSrc = ../../mcsl;
in

let
  nixSrc = "${projectSrc}/nix";
in

{
  # Source package dependencies
  nixSrc = nixSrc;

  # External package dependencies
  cmdline = "${cmdlineSrc}/script/default.nix";
  pbbslib = "${pbbslibSrc}/default.nix";
  pbbsbench = "${pbbsbenchSrc}/default.nix";
  mcsl = "${mcslSrc}/nix/default.nix";
  cilkRtsWithStats = "${cilkRtsWithStatsSrc}/default.nix";

  # Input data
  inputData = "${nixSrc}/input-data.nix";
  inputDataSrc = "${projectSrc}/inputs/";

  # Benchmarking script
  pbenchOcamlSrcs = pbenchOcamlSrcs;
  benchOcamlSrc = "${projectSrc}/bench/";
  benchScript = "${nixSrc}/bench-script.nix";

  # C++ Benchmarks
  benchmarksCppSrc = "${projectSrc}/cpp";
  benchmarksCpp = "${nixSrc}/benchmarks-cpp.nix";

}
