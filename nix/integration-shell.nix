{ cmake, ncurses, jq, miller, z3, stdenv, lib, nix-gitignore, mach-nix, mkShell }: k:
mkShell {
  packages = [
    k
    ncurses # TODO: .../lib/kframework/setenv: line 31: tput: command not found
    jq
    miller # processing test statistics
    z3
    (mach-nix.mkPython {
      requirements = ''
        jsonrpcclient
        poetry
      '';
    })
    cmake # for evm-semantics
  ];

  shellHook = ''
    export NIX=1 PS1=nix-develop:$PS1
  '';
}
