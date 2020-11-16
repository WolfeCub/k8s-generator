with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    gmp
  ];
}

