{ mkDerivation, base, bytestring, hashable, stdenv, text }:
mkDerivation {
  pname = "comparable-key";
  version = "0.0.1";
  src = import ../../nix/cabal-sdist.nix ./.;
  buildDepends = [ base bytestring hashable text ];
  homepage = "https://github.com/circuithub/comparable-key";
  description = "Eq, Ord and Hashable for the key part of a data type";
  license = stdenv.lib.licenses.mit;
}
