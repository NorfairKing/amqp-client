let
  sources = import ./sources.nix;
  nix-pre-commit-hooks = import sources.pre-commit-hooks;
in
{ pkgs ? import sources.nixpkgs { }
}:
let
  amqp-generator-wrapped = pkgs.writeShellScript "amqp-generator-wrapped" ''
    PATH="${nix-pre-commit-hooks.ormolu}/bin/:$PATH"
    exec ${pkgs.haskellPackages.amqp-generator}/bin/amqp-generator $@
  '';
in
{
  tools = with nix-pre-commit-hooks; [
    hlint
    nixpkgs-fmt
    ormolu
  ];
  check = nix-pre-commit-hooks.run {
    src = ../.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
      generate-amqp = {
        enable = true; # Turn this to False while working on the generator.
        name = "generate amqp client code from spec";
        entry = "${amqp-generator-wrapped} spec/amqp0-9-1.extended.xml";
        types = [ "text" ];
        pass_filenames = false;
      };
    };
  };
}
