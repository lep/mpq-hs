{
    inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs";
	flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
	flake-utils.lib.eachDefaultSystem (system:
	    let pkgs = import nixpkgs { inherit system; };
		packageName = "mpq";


		ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
		    ps.bytestring
		    ps.binary
		    ps.containers
		    ps.zlib
		]);

	    in rec {
		devShell = pkgs.mkShell {
		    buildInputs = [
			ghcPackages
			pkgs.hecate # seems like a reasonable hexviewer
			pkgs.hlint
		    ];
		};
	    });
}


