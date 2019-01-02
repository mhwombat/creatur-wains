{ mkDerivation, base, bytestring, cereal, containers, creatur
, deepseq, directory, filepath, lens, MonadRandom, mtl
, numeric-tools, QuickCheck, random, som, stdenv, test-framework
, test-framework-quickcheck2, unix
}:
mkDerivation {
  pname = "creatur-wains";
  version = "9.5.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers creatur deepseq directory
    filepath lens MonadRandom mtl random som unix
  ];
  testHaskellDepends = [
    base cereal containers creatur deepseq lens MonadRandom mtl
    numeric-tools QuickCheck random som test-framework
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/mhwombat/creatur-wains#readme";
  description = "Artificial agent specialised for for pattern discovery";
  license = stdenv.lib.licenses.bsd3;
}
