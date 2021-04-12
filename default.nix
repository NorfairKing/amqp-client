let
  pkgs = import ./nix/pkgs.nix {
    extraOverlays = [
      (final: previous: {
        amqp-client-tests = true;
      })
    ];
  };
in
pkgs.amqpRelease
