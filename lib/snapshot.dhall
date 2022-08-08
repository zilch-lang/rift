let Package =
      ./package.dhall
        sha256:3942e69734391e8177ccaf1a1c551b94a594cbe22fd792e7e578bc24c2f392fb

let LTS =
      ./lts.dhall
        sha256:11b148af43c98e53e30a373023774adaf4d292eec7933d9f214bf68714bcb141

in  { Type =
        { -- | The name of the snapshot
          name : LTS.Type
        , -- | The version of the Zilch compiler the package set is associated with
          gzc-version : Text
        , -- | The list of packages to include in that snapshot
          --
          --   NOTE: Please hash all the packages (the .dhall files) using @dhall hash@ and reference them
          --         using @./some-file.dhall sha256:...@.
          package-set : List Package.Type
        }
    , default.package-set = [] : List Package.Type
    }
