let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  name = "amqp-shell";
  buildInputs = with pkgs; [
    (import sources.niv { inherit pkgs; }).niv
  ];
}
