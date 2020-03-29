{ mkDerivation, attoparsec, base, base58-bytestring
, base64-bytestring, bytestring, cryptonite, hex, hpack, io-streams
, memory, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "hs-multihash";
  version = "0.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base58-bytestring base64-bytestring bytestring
    cryptonite hex io-streams memory
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring io-streams optparse-applicative
  ];
  prePatch = "hpack";
  homepage = "https://github.com/MatrixAI/hs-multihash#readme";
  description = "Multihash library and CLI executable";
  license = stdenv.lib.licenses.bsd3;
}
