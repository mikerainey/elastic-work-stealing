{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  sources ? import ../nix/local-sources.nix,
  cmdline ? import sources.cmdline {},
  cilkRtsWithStats ? import sources.cilkRtsWithStats {},
  jemalloc ? pkgs.jemalloc450, # use jemalloc, unless this parameter equals null (for now, use v4.5.0, because 5.1.0 has a deadlock bug)
  gcc ? pkgs.gcc7,
  hwloc ? pkgs.hwloc, # use hwloc, unless this parameter equals null
  pviewSrc ? pkgs.fetchFromGitHub {
    owner  = "deepsea-inria";
    repo   = "pview";
    rev    = "78d432b80cc1ea2767e1172d56e473f484db7f51";
    sha256 = "1hd57237xrdczc6x2gxpf304iv7xmd5dlsvqdlsi2kzvkzysjaqn";
  },
  miniUTS ? import sources.miniUTS {}
}:

with pkgs; {
  qpidEnv = stdenvNoCC.mkDerivation {
    name = "cpp";
    
    buildInputs = [
        gcc
        jemalloc
        which
        cilkRtsWithStats
    ] ++ (if hwloc == null then [] else [ hwloc ]);
    
    shellHook =
      let hwlocFlgs =
            if hwloc == null then
              ""
            else
              ''export USE_HWLOC=1
                export HWLOC_CFLAGS="-I ${hwloc.dev}/include/"
                export HWLOC_LDFLAGS="-L ${hwloc.lib}/lib/ -lhwloc"
              '';
      in
      let pview = import "${pviewSrc}/default.nix" {}; in
      ''
      export CMDLINE_PATH="${cmdline}"
      export MCSL_INCLUDE_PATH="../../mcsl/include/"
      export CILK_EXTRAS_PREFIX="-L ${cilkRtsWithStats}/lib -I ${cilkRtsWithStats}/include -ldl -DCILK_RUNTIME_WITH_STATS"
      export MINI_UTS_PATH="${miniUTS}"xo
      ${hwlocFlgs}
      export PATH=${gcc}/bin/:${pview}/bin:$PATH
      '';
  };
}
