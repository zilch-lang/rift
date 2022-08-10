let Version =
      ./version.dhall
        sha256:596564e58f0959e1cccfa1bb154948adb195d9220d381f0742f4058c9d083b58

let PackageDependency =
      { {- | The name of the package in the package set. -}
        package : Text
      , {- | A predicate which must hold when importing this package. -}
        version : Version.Type → Bool
      }

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
      { dependencies = [] : List PackageDependency, gzc-flags = [] : List Text }
    }
