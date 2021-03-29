let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { };
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "amqp-shell";
  buildInputs = with pkgs; [
    coreutils
    zlib
    (import sources.niv { }).niv
    rabbitmq-server
  ];
  shellHook = ''
    ${pre-commit.check.shellHook}
  '';
}
