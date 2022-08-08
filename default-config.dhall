let VersionRange = { package : Text, range : Text }

let Component =
      let K = < Executable | Library >

      in  { Kind = K
          , Type =
              { {- |
                  The name of the component.
                -}
                name : Text
              , version : Text
              , dependencies : List VersionRange
              , {- |
                  A list of directories containing Zilch source files.

                  When the component is for an executable, at least one of the files
                  should be a main module.
                -}
                source-dirs : List Text
              , kind : K
              , {- |
                  Additional flags to give to the Zilch compiler.
                -}
                gzc-flags : List Text
              }
          , default =
            { dependencies = [] : List VersionRange
            , gzc-flags = [] : List Text
            }
          }

let Dependency =
      https://raw.githubusercontent.com/zilch-lang/rift/lib/source.dhall
        sha256:8fc3205ee05bdee3f8e4f553c96f898c14f7e9cb1a2150345cd7316694c4f8cb

let LTS =
      https://raw.githubusercontent.com/zilch-lang/rift/lib/lts.dhall
        sha256:11b148af43c98e53e30a373023774adaf4d292eec7933d9f214bf68714bcb141

let
    {- |
      A project is a set of local components, each versioned separately.
    -}
    Project =
      List Component.Type

let {- |
      The configuration is a set of options which are useful when locally building packages.

      * The @extra-deps@ option is taken in account when fetching a package from the package set,
        as it can contain useful information for missing packages.
      * The @lts@ is also checked against the current one, and is accepted only if it is a past one
        (if it satisfies the natural order of versions, where @unstable > *@).
    -}
    Configuration =
      { Type =
          { {- |
              The LTS version to use for the package set when locally compiling.
            -}
            lts : LTS.Type
          , {- |
              Additional dependencies which are not part of the package set.
            -}
            extra-deps : List Dependency
          }
      , default.extra-deps = [] : List Dependency
      }

in  { Project, Configuration, VersionRange, Component, Dependency, LTS }
