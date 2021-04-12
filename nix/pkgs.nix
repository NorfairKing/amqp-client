{ pkgsf ? import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "35b3a1f43a9621a88e906a839f99d8252500152b"; # Keep this up-to-date with the LTS in stack.yaml
  })
, extraOverlays ? [ ]
}:
let
  sources = import ./sources.nix;
  pkgs = pkgsf { };
  pre-commit-hooks = import sources.pre-commit-hooks;
  validity-overlay = import (sources.validity + "/nix/overlay.nix");
  yamlparse-applicative-overlay = import (sources.yamlparse-applicative + "/nix/overlay.nix");
  safe-coloured-text-overlay = import (sources.safe-coloured-text + "/nix/overlay.nix");
  sydtest-overlay = import (sources.sydtest + "/nix/overlay.nix");
  amqpPkgs =
    pkgsf {
      overlays =
        [
          validity-overlay
          yamlparse-applicative-overlay
          safe-coloured-text-overlay
          sydtest-overlay
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (import ./overlay.nix)
        ] ++ extraOverlays;
      config.allowUnfree = true;
    };
in
amqpPkgs
