{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      mpq = pkgs: pkgs.haskellPackages.callPackage ./mpq.nix { };
    in {
      packages = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in { default = mpq pkgs; });

      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            nativeBuildInputs = [ pkgs.cabal-install pkgs.zlib ];

          };
        });
    };
}
