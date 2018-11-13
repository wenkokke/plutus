{ stdenv, pkgs }:

with pkgs;

let
  yarnDeps = import ./yarn.nix { inherit fetchurl linkFarm; };
  offlineCache = yarnDeps.offline_cache;
  patchShebangs = dir: ''
    node=`type -p node`
    coffee=`type -p coffee || true`
    find -L ${dir} -type f -print0 | xargs -0 grep -Il . | \
    xargs sed --follow-symlinks -i \
        -e 's@#!/usr/bin/env node@#!'"$node"'@' \
        -e 's@#!/usr/bin/env coffee@#!'"$coffee"'@' \
        -e 's@#!/.*/node@#!'"$node"'@' \
        -e 's@#!/.*/coffee@#!'"$coffee"'@' || true
  '';

in {
  plutus-playground-client = stdenv.mkDerivation {
    src = ./.;
  
    name = "plutus-playground-client";
  
    buildInputs = [ nodejs yarn git cacert purescript ];
  
    configurePhase = ''
      export HOME="$NIX_BUILD_TOP"
  
      yarn config --offline set yarn-offline-mirror ${offlineCache}
      yarn config --offline set yarn-offline-mirror-pruning true
  
      # yarn install --offline --frozen-lockfile --ignore-engines --ignore-scripts
      yarn install --offline
      ${patchShebangs "node_modules/.bin/"}
    '';
  
    bowerComponents = pkgs.buildBowerComponents {
      name = "my-web-app";
      generated = ./bower-packages.nix;
      src = ./.;
    };

    buildPhase = ''
      cp --reflink=auto --no-preserve=mode -R $bowerComponents/bower_components .
      yarn --offline
      yarn run --offline bower install
      yarn run --offline webpack
      ls
    '';
  
    doCheck = false;
  
    checkPhase = ''
      yarn test --coverage --ci --no-color
    '';
  
    installPhase = ''
      mv dist $out
    '';
  };
}
