let
  sources = import ./sources.nix;
  nix-pre-commit-hooks = import sources.pre-commit-hooks;

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
        enable = true;
        name = "generate amqp client code from spec";
        entry = "amqp-generator spec/amqp0-9-1.extended.xml";
        types = [ "text" ];
        pass_filenames = false;
      };
    };
  };
}
