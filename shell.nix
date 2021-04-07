let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { };
  pre-commit = import ./nix/pre-commit.nix { inherit pkgs; };
in
pkgs.haskell.lib.buildStackProject {
  name = "amqp-shell";
  buildInputs = with pkgs; [
    coreutils
    zlib
    (import sources.niv { }).niv
    rabbitmq-server
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp # Otherwise we can get into trouble with tmpfs getting full
    ${pre-commit.check.shellHook}
  '';
}
