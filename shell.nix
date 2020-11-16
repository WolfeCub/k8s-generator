with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    gmp
    haskellPackages.haskell-language-server
  ];
}

