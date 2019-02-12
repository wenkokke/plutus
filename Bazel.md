# Plutus Bazel

This branch demonstrates building the Plutus project using bazel + nix rather than stack + cabal + nix

## Getting Started

```
nix-shell shell-bazel.nix

# for OSX and 'normal' Linux distributions
bazel test --test_env BUILD_WORKSPACE_DIRECTORY=$(pwd) --remote_http_cache=http://34.243.81.23:80 //...

# or if you are using NixOS you will need to tell bazel
bazel test --test_env BUILD_WORKSPACE_DIRECTORY=$(pwd) --remote_http_cache=http://34.243.81.23:80 \
  --platforms=@io_tweag_rules_purescript//purescript/platforms:linux_x86_64_nixpkgs \
  --host_platform=@io_tweag_rules_purescript//purescript/platforms:linux_x86_64_nixpkgs \
  //...
```

## Advantages

* bazel uses content addressable storage so cache hit rate should be much higher making switching branches faster and improving CI times
* the tools used by developers are the same as those used by CI; bazel wrapped in a small amount of nix to provide the tool chain for building the project.
* the user facing part of the build tooling is simpler, there should be no need to understand the nix code as it doesn't do much and if you do want to understand it this should be much easier. What's required for bazel is pretty simple, similar in complexity to cabal. There is a new piece of complexity in the `WORKSPACE` file however it is mostly boilerplate and simple to understand
* bazel has been designed with cross-compilation in mind so this should be simpler than a nix + cabal solution
* the overall project structure is more "standard" than the current nix + stack + cabal setup, a combination that doesn't seem to have any agreed upon structure and tooling yet
* there should be no need to use a forked nixpkgs
* bazel is backed by Google, rules_haskell and rules_purescript backed by Tweag and both are developing very quickly, with a lot of resources being put into making developers lives better. Nix is also developing quickly however it is a much more general solution with bigger, slower changes coming. In particular, features for using nix with Haskell are not being developed by many people at the moment and not in a very diverse way (it seems most developments are coming from IOHK)

## Disadvantages

* Not as flexible as the current setup, developers will be forced to use bazel and nothing else (an advantage from other perspectives)
* Sandboxed nix builds are not possible in practice because that would mean giving up the remote cache
* Fewer expertise at IOHK, there would be a period where IOHK would be reliant on Tweag for adding features
* Although ghcid and dante work well with bazel, they are not as feature rich as intero at the moment. We expect to overtake intero in terms of features some time this year

## Still to do

As this is a proof of concept, not everything is complete, most notably:

* haskell packages are still being resolved using nix + cabal. I am currently working on removing this but I haven't finished yet
* once the above work is done, all the nix, stack and cabal files can be removed. We will then only require a simple default.nix and shell.nix
* the current tooling for the purescript project still provides a better developer experience (webpage hot reloading, ide server etc), there is a roadmap to fix this but there is probably quite a bit of work required
