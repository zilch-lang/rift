let LTS =
      https://raw.githubusercontent.com/zilch-lang/rift/00bd366210b51b8bc70c52cbc79f270d0f2f9c85/lib/lts.dhall
        sha256:11b148af43c98e53e30a373023774adaf4d292eec7933d9f214bf68714bcb141

let Version =
      https://raw.githubusercontent.com/zilch-lang/rift/00bd366210b51b8bc70c52cbc79f270d0f2f9c85/lib/version.dhall
        sha256:54a70816c7185df69cd1d1a36410cc57a496e78d68422bc9d4332b6801d24275

let ExtraDependency =
      https://raw.githubusercontent.com/zilch-lang/rift/00bd366210b51b8bc70c52cbc79f270d0f2f9c85/lib/extra-package.dhall
        sha256:0cee6fd112dd9b821b12b7110f311d9b22d1a3bf74d324e883f39629ea252a4e

let Source =
  		https://raw.githubusercontent.com/zilch-lang/rift/00bd366210b51b8bc70c52cbc79f270d0f2f9c85/lib/source.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

let Component =
  		https://raw.githubusercontent.com/zilch-lang/rift/00bd366210b51b8bc70c52cbc79f270d0f2f9c85/lib/component.dhall
        sha256:bd4c557eb36312ea58510a87bc75a206e871f141c249e52df36d848e916f96a2

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
            extra-deps : List ExtraDependency.Type
          }
      , default.extra-deps = [] : List ExtraDependency.Type
      }

in  { Project
    , Configuration
    , Version
    , Component
    , ExtraDependency
    , LTS
    , Source
    }
