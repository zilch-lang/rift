let LTS =
      https://raw.githubusercontent.com/zilch-lang/rift/f1155219b65393dcfec37cf57fa1b905d031c567/lib/lts.dhall
        sha256:11b148af43c98e53e30a373023774adaf4d292eec7933d9f214bf68714bcb141

let Version =
      https://raw.githubusercontent.com/zilch-lang/rift/f1155219b65393dcfec37cf57fa1b905d031c567/lib/version.dhall
        sha256:596564e58f0959e1cccfa1bb154948adb195d9220d381f0742f4058c9d083b58

let ExtraDependency =
      https://raw.githubusercontent.com/zilch-lang/rift/f1155219b65393dcfec37cf57fa1b905d031c567/lib/extra-package.dhall
        sha256:9cb1f6360b61c93ab2d6c680de9a447732e019f1531690b32008cadfe90cd781

let Source =
  		https://raw.githubusercontent.com/zulch-lang/rift/f1155219b65393dcfec37cf57fa1b905d031c567/lib/extra-package.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

let PackageDependency =
      { {- | The name of the package in the package set. -}
        package : Text
      , {- | A predicate which must hold when importing this package. -}
        version : Version.Type → Bool
      }

let Component =
      let K = < Executable | Library >

      in  { Kind = K
          , Type =
              { {- |
                  The name of the component.
                -}
                name : Text
              , version : Version.Type
              , dependencies : List PackageDependency
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
            { dependencies = [] : List PackageDependency
            , gzc-flags = [] : List Text
            }
          }

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
    , PackageDependency
    , Component
    , ExtraDependency
    , LTS
    , Source
    }
