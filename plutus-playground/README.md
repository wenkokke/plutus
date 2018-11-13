# Building

## Server

``` sh
cd plutus-playground-server
stack build
stack exec -- plutus-playground-server psgenerator ../plutus-playground-client/src/
stack exec -- plutus-playground-server webserver -p 8080 ../plutus-playground-client/dist/
```

## Client

``` sh
cd plutus-playground-client
yarn
yarn run bower install
yarn run webpack
```

## Viewing

Now navigate to the webserver on http://localhost:8080

# TroubleShooting

On a Mac, you may see this when compiling a contract:

```
GhcException "unable to load package `integer-gmp-1.0.2.0'"
```

This is due to a [GHC
bug](https://ghc.haskell.org/trac/ghc/ticket/15105). Sadly this isn't
slated to be fixed until GHC 8.8 at the earliest, but there's a simple
workaround (listed in that ticket):

``` sh
find ~/.stack -name HSinteger-gmp-1.0.2.0.o -ok rm {} \;
```
