let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.mkShell {
  name = "amqp-shell";
  buildInputs = with pkgs; [
    (import sources.niv { inherit pkgs; }).niv
  ];
  shellHook = ''
    ${pre-commit.check.shellHook}
  '';
}
