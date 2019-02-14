{ system ? builtins.currentSystem
, config ? {}
, localPackages ? import ./. { inherit config system; }
, pkgs ? localPackages.pkgs
, plutusSrc ? ./.
}:

let
  localLib = import ./lib.nix { inherit config system; };
  pkgs = import (localLib.iohkNix.fetchNixpkgs ./nixpkgs-bazel-src.json) {};
  # These are tools that will be used by bazel
  haskellPackages = pkgs.haskellPackages;
  ghc = pkgs.haskell.packages.ghc844.ghc;
  happy = haskellPackages.happy;
  alex = haskellPackages.alex;
  hlint = haskellPackages.hlint;
  stylishHaskell = haskellPackages.stylish-haskell;
  nodejs = pkgs.nodejs;
  yarn = pkgs.yarn;
  purescript = if pkgs.stdenv.isDarwin
    then pkgs.writeTextFile {name = "purescript"; text = ""; destination = "/bin/purs"; }
    else (import (localLib.iohkNix.fetchNixpkgs ./plutus-playground/plutus-playground-client/nixpkgs-src.json) {}).purescript;
  glibcLocales = if pkgs.stdenv.isDarwin
    then pkgs.writeTextFile {name = "glibcLocales"; text = ""; destination = "/lib/locale/locale-archive"; }
    else pkgs.glibcLocales;
  mkBazelScript = {name, script}: pkgs.stdenv.mkDerivation {
          name = name;
          unpackPhase = "true";
          buildInputs = [];
          buildPhase = "";
          installPhase = ''
            mkdir -p $out/bin
            cp ${script} $out/bin/run.sh
          '';
        };
  hlintScript = mkBazelScript { name = "hlintScript";
                                script = import ./test-scripts/hlint-script.nix {inherit pkgs haskellPackages;};
                                };
  stylishHaskellScript = mkBazelScript { name = "stylishHaskellScript";
                                         script = import ./test-scripts/stylish-haskell-script.nix {inherit pkgs haskellPackages;};
                                         };
  shellcheckScript = mkBazelScript { name = "shellcheckScript";
                                     script = import ./test-scripts/shellcheck-script.nix {inherit pkgs;};
                                     };
  /* with pkgs; */
  /* with darwin.apple_sdk.frameworks; */
  # XXX On Darwin, workaround
  # https://github.com/NixOS/nixpkgs/issues/42059. See also
  # https://github.com/NixOS/nixpkgs/pull/41589.
  cc = with pkgs; with darwin.apple_sdk.frameworks; runCommand "cc-wrapper-bazel" {
      buildInputs = [ stdenv.cc makeWrapper libiconv ];
    }
    ''
      mkdir -p $out/bin
      # Copy the content of stdenv.cc
      for i in ${stdenv.cc}/bin/*
      do
        ln -sf $i $out/bin
      done
      # Override clang
      rm $out/bin/clang
      makeWrapper ${stdenv.cc}/bin/clang $out/bin/clang \
        --add-flags "-isystem ${llvmPackages.libcxx}/include/c++/v1 \
                     -F${CoreFoundation}/Library/Frameworks \
                     -F${CoreServices}/Library/Frameworks \
                     -F${Security}/Library/Frameworks \
                     -F${Foundation}/Library/Frameworks \
                     -L${libcxx}/lib \
                     -L${libiconv}/lib \
                     -L${darwin.libobjc}/lib"
   '';
  patchedStdenv = with pkgs; if stdenv.isDarwin then overrideCC stdenv cc else stdenv;
in
patchedStdenv.mkDerivation rec {
  name = "plutus-all";

  # XXX: hack for macosX, this flag disables bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be caught earlier
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  src = plutusSrc;

  buildInputs = [
    ghc
    pkgs.git
    pkgs.cacert
    pkgs.libcxx
    pkgs.unzip
    pkgs.perl
    pkgs.file
    pkgs.bazel
    pkgs.libiconv
  ];

  setupTools = ''
    # link the tools bazel will import to predictable locations
    mkdir -p tools
    ln -nfs ${ghc} ./tools/ghc
    ln -nfs ${happy} ./tools/happy
    ln -nfs ${alex} ./tools/alex
    ln -nfs ${hlintScript} ./tools/hlint
    ln -nfs ${stylishHaskellScript} ./tools/stylish-haskell
    ln -nfs ${shellcheckScript} ./tools/shellcheck
    ln -nfs ${purescript} ./tools/purescript
    ln -nfs ${glibcLocales} ./tools/glibc-locales
    mkdir -p yarn-nix/bin
    ln -nfs ${nodejs} ./node-nix
    ln -nfs ${yarn}/bin/yarn ./yarn-nix/bin/yarn.js
  '';

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"

    # Add nix config flags to .bazelrc.
    BAZELRC_LOCAL="$HOME/.bazelrc"
    if [ ! -e "$BAZELRC_LOCAL" ]
    then
      ARCH=""
      if [ "$(uname)" == "Darwin" ]; then
        ARCH="darwin"
      elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
        ARCH="linux"
      fi
    fi

    if [ "$ARCH" == "linux" ]
    then
      (
        echo "common --host_platform=@io_tweag_rules_purescript//purescript/platforms:linux_x86_64_nixpkgs"
        echo "common --platforms=@io_tweag_rules_purescript//purescript/platforms:linux_x86_64_nixpkgs"
      ) >> $BAZELRC_LOCAL
    fi

    (
      echo "common --remote_http_cache=http://34.243.81.23:80"
      echo "common --verbose_failures"
      echo "test --test_output=errors"
      echo "test --test_verbose_timeout_warnings"
      echo "test --test_env=PATH"
      echo "test --test_env=BUILD_WORKSPACE_DIRECTORY"
    ) >> $BAZELRC_LOCAL

    export BUILD_WORKSPACE_DIRECTORY=$PWD

    ${setupTools}
  '';

  shellHook = ''
    ${setupTools}

    # source bazel bash completion
    source ${pkgs.bazel}/share/bash-completion/completions/bazel
  '';

  buildPhase = "bazel test //...";

  installPhase = ''
    mkdir -p $out/bin
    unzip bazel-bin/plutus-playground/plutus-playground-client/dist.zip -d $out/plutus-playground-client/
    cp bazel-bin/plutus-playground/plutus-playground-server/plutus-playground-server-app $out/bin/
  '';
}
