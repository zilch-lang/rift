let Version =
      ./version.dhall
        sha256:54a70816c7185df69cd1d1a36410cc57a496e78d68422bc9d4332b6801d24275

let PackageDependency =
      { {- | The name of the package in the package set. -}
        package : Text
      , {- | A predicate which must hold when importing this package. -}
        version : Version.Type → Bool
      }

let K = < Executable | Library >

in  { Kind = K
    , Type =
        { version : Version.Type
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
      { dependencies = [] : List PackageDependency, gzc-flags = [] : List Text }
    }
