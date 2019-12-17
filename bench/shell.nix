{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  sources ? import ../nix/local-sources.nix,
  cilkRtsWithStats ? import sources.cilkRtsWithStats {},
  hwloc ? pkgs.hwloc # use hwloc, unless this parameter equals null
}:

with pkgs; {
  qpidEnv = stdenvNoCC.mkDerivation {
    name = "cpp";
    
    buildInputs = [
        gcc7
        gdb
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
      ''
      export CILK_EXTRAS_PREFIX="-L ${cilkRtsWithStats}/lib -I ${cilkRtsWithStats}/include -ldl -DCILK_RUNTIME_WITH_STATS"
      ${hwlocFlgs}
      '';
  };
}
