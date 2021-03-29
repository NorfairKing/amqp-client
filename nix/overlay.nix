final: previous:
with final.haskell.lib;

{
  amqpPackages =
    let
      amqpPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
              )
            )
            (final.haskellPackages.autoexporter)
        );
      amqpPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (amqpPkg name);
      amqpPkgWithOwnComp = name: amqpPkgWithComp name name;

    in
    {
      "amqp-generator" = amqpPkg "amqp-generator";
      "amqp-serialisation" = amqpPkg "amqp-serialisation";
      "amqp-client" = overrideCabal (amqpPkg "amqp-client") (old: {
        testDepends = (old.testDepends or [ ]) ++ [ final.rabbitmq-server ];
        doCheck = false; # Something goes wrong with testing; the tests hang?
      });
    };

  amqpRelease =
    final.symlinkJoin {
      name = "amqp-release";
      paths = final.lib.attrValues final.amqpPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                final.amqpPackages // { }
            );
      }
    );
}
