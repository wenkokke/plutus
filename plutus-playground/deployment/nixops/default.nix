let
  hostPkgs = import <nixpkgs> {};
  playgroundMachine = import ./playground.nix;
  machines = (hostPkgs.lib.importJSON ./machines.json);
  overlays = import ./overlays.nix;
  stdOverlays = [ overlays.journalbeat ];
  playground = import ../../../. {};
  options = { inherit stdOverlays machines defaultMachine playground; };
  defaultMachine = (import ./default-machine.nix) options;
  playgroundA = playgroundMachine.mkInstance options machines.playgroundA;
in
  { inherit playgroundA; }
