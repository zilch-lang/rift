Rift is an experimental project manager (dependency & build tool) for the Zilch programming language.

It features configuration written in [Dhall](https://dhall-lang.org/) as well as complete project isolation
(meaning that your project will have its own package set separated from other projects, along the lines of what [stack](https://docs.haskellstack.org/en/stable/README/) does).

:warning: This piece of software is still pretty much a work in progress (not much has been done).
Therefore, using it may yield bugs, or even lack functionalities.

## Installing

Currently written in Haskell, you will need [stack](https://docs.haskellstack.org/en/stable/README/) to build and install
the `rift` executable in your environment.

## Contributing

Rift uses the package set located at [zilch-lang/pkgs](https://github.com/zilch-lang/pkgs) which is written in Dhall.

The whole codebase is written in Haskell, but should be pretty accessible.
If you feel like working on an issue, drop a comment there, open a draft PR and start working! And most importantly, enjoy.
You can also open issues if you feel like this tool does not work as expected!

