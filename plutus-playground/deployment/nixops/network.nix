let
  pkgs = import <nixpkgs> {};
  machines = (pkgs.lib.importJSON ./machines.json);
  mkInstance = node:
  {
      deployment.targetHost = node.ip;
      deployment.hasFastConnection = true;
  };
  playgroundA = mkInstance machines.playgroundA;
in
  { inherit playgroundA;
    network.description = "Plutus Playground";
    network.enableRollback = true;
  }
