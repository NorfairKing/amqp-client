# Haskell's AMQP Client

This is a client library for the AMQP protocol.

The specification is available for reference in this repository in the `spec` directory.

## Hacking

1. Activate the pre-commit hooks

```
nix-shell --command ""
```

2. Start your feedback loop

```
stack test --file-watch
```
