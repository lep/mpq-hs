{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
          ps.bytestring
          ps.binary
          ps.containers
          ps.zlib
          ps.optparse-applicative
        ]);

        mpq = pkgs.stdenv.mkDerivation {
          name = "mpq";
          src = self;
          buildPhase = "${ghcPackages}/bin/ghc -O Main.hs -o mpq";

          installPhase = "install -Dt $out/bin mpq";
        };

      in {
        packages.mpq = mpq;
        defaultPackage = mpq;

        devShell = pkgs.mkShell {
          buildInputs = [
            ghcPackages
            pkgs.hecate # seems like a reasonable hexviewer
            pkgs.hlint
          ];
        };
      });
}

