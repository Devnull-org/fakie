{ mkDerivation, aeson, async, base, bytestring, case-insensitive
, casing, containers, directory, errors, exceptions, filepath
, gi-gtk, haskell-gi-base, hedgehog, hspec, http-client
, http-client-tls, http-conduit, http-types, lens, lens-aeson
, microlens, monad-logger, mtl, network-uri, optparse-applicative
, pretty, pretty-simple, raw-strings-qq, safe-exceptions, stdenv
, template-haskell, text, time, transformers, unordered-containers
, uri-encode, vector, wai, wai-app-static, wai-cors, wai-extra
, wai-transformers, wai-util, warp, yaml
}:
mkDerivation {
  pname = "fakie";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring case-insensitive casing containers
    directory errors filepath gi-gtk haskell-gi-base http-client
    http-client-tls http-conduit http-types lens lens-aeson
    monad-logger mtl network-uri optparse-applicative pretty
    pretty-simple safe-exceptions template-haskell text time
    transformers unordered-containers uri-encode vector wai
    wai-app-static wai-cors wai-extra wai-transformers wai-util warp
    yaml
  ];
  executableHaskellDepends = [
    aeson async base bytestring case-insensitive casing containers
    directory errors filepath http-client http-client-tls http-conduit
    http-types lens lens-aeson monad-logger mtl network-uri
    optparse-applicative pretty pretty-simple safe-exceptions
    template-haskell text time transformers unordered-containers
    uri-encode vector wai wai-app-static wai-cors wai-extra
    wai-transformers wai-util warp yaml
  ];
  testHaskellDepends = [
    aeson async base bytestring case-insensitive casing containers
    directory errors exceptions filepath hedgehog hspec http-client
    http-client-tls http-conduit http-types lens lens-aeson microlens
    monad-logger mtl network-uri optparse-applicative pretty
    pretty-simple raw-strings-qq safe-exceptions template-haskell text
    time transformers unordered-containers uri-encode vector wai
    wai-app-static wai-cors wai-extra wai-transformers wai-util warp
    yaml
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
