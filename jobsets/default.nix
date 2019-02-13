{ nixpkgs ? <nixpkgs>
, plutusPrsJSON ? ./simple-pr-dummy.json
, declInput ? { rev = "bazel-nix"; }
}:
let
    plutusPrs = builtins.fromJSON (builtins.readFile plutusPrsJSON );

    plutusURI = "git@github.com:input-output-hk/plutus.git";
    mkFetchGithub = value: {
      inherit value;
      type = "git";
      emailresponsible = false;
    };
    nixpkgs-src = builtins.fromJSON (builtins.readFile ../nixpkgs-bazel-src.json);
    pkgs        = import nixpkgs {}; ## Strictly for 'lib'/'runCommand'

    defaultSettings = extraInputs: {
      enabled = 1;
      hidden = false;
      keepnr = 3;
      schedulingshares = 500;
      checkinterval = 90;
      inputs = {
        nixpkgs   = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
        jobsetSrc = mkFetchGithub "${declInput.uri} ${declInput.rev}";
      } // extraInputs;
      enableemail = false;
      emailoverride = "";
    };

    mkGitSrc = { repo, branch ? "refs/heads/master"}: {
      type = "git";
      value = repo + " " + branch + " leaveDotGit";
      emailresponsible = false;
    };

    mkJob = { name, description, nixexprinput ? "jobsetSrc", nixexprpath, extraInputs ? {} }: {
      inherit name;
      value = (defaultSettings extraInputs) // {
        inherit description nixexprinput nixexprpath;
      };
    };

    mkPlutus = {
      name,
      description,
      plutusBranch ? "refs/heads/master"
    }:
      mkJob {
        inherit name description;
        nixexprpath = "jobsets/release.nix";
        extraInputs = {
          plutusSrc = mkFetchGithub "${plutusURI} ${plutusBranch}";
        };
      };

    plutusJobsetDefinition = pkgs.lib.listToAttrs (
      [
        (mkPlutus {
          name = "plutus";
          description = "Plutus";
          plutusBranch = "bazel-nix-hydra";
        })
      ]
      ++
      (pkgs.lib.mapAttrsToList
        (
          num:
          info: mkPlutus {
            name = "plutus-PR-${num}";
            description = info.title;
            plutusBranch = info.head.sha;
          }
        )
        plutusPrs
      )
    );

    jobsetDefinition = plutusJobsetDefinition;
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF

    tee $out <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF
  '';
}
