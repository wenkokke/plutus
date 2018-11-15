let
  hostPkgs = import <nixpkgs> {};
  playground = import ./playground.nix;
  machines = (hostPkgs.lib.importJSON ./machines.json);
  overlays = import ./overlays.nix;
  stdOverlays = [ overlays.journalbeat ];
  playground = import ../../../. {};
  options = { inherit stdOverlays machines defaultMachine hydraPlayground; };
  defaultMachine = (import ./default-machine.nix) options;
  playgroundA = playground.mkInstance options machines.playgroundA;
in
  { inherit playgroundA; }
