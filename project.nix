{ mkDerivation, base, strict, stdenv }:
mkDerivation {
  pname = "thc-slides";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base strict ];
  license = stdenv.lib.licenses.unfree;
}
