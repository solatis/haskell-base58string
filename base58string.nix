{ mkDerivation, aeson, base, binary, bytestring, hspec, stdenv
, text
}:
mkDerivation {
  pname = "base58string";
  version = "0.10.0";
  src = ./.;
  buildDepends = [ aeson base binary bytestring text ];
  testDepends = [ base binary bytestring hspec text ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Fast and safe representation of a Base-58 string";
  license = stdenv.lib.licenses.mit;
}
