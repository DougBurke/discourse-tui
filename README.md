This is a terminal viewer for [discourse](https://www.discourse.org/) inspired by a [similar tool for reddit](https://github.com/michael-lazar/rtv). This
is a fork of [the original](https://builds.sr.ht/~jackwines/discourse-tui).

Note that the demo is out of date but gives an idea of what's going
on:

![demo](demo.svg)

Usage: discourse-tui baseUrl
Example: discourse-tui http://discourse.haskell.org

to compile:
[make sure you have nix installed](https://nixos.org/nix/download.html)
```
nix-build release.nix
```

As of 22/3/2018, `cabal new-install` also works.
