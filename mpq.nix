{ mkDerivation, array, base, binary, bytestring, containers
, filepath, lib, mtl, optparse-applicative, transformers, zlib
}:
mkDerivation {
  pname = "mpq";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base binary bytestring containers filepath mtl
    optparse-applicative transformers zlib
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "mpq";
}
