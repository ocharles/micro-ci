{ mkDerivation, aeson, base, dhall, directory, filepath, github
, process, servant-github-webhook, servant-server, stdenv, stm
, text, warp
}:
mkDerivation {
  pname = "micro-ci";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base dhall directory filepath github process
    servant-github-webhook servant-server stm text warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
