{ mkDerivation, base, containers, data-default, lens, stdenv, text
}:
mkDerivation {
  pname = "jmdict-ast";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers data-default lens text ];
  description = "AST for JMDict";
  license = stdenv.lib.licenses.mit;
}
