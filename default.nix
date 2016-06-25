{ mkDerivation, ansi-terminal, base, process, stdenv, stm, text }:
mkDerivation {
  pname = "minibar";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ ansi-terminal base process stm text ];
  license = stdenv.lib.licenses.mit;
}
