let Package =
      ./package.dhall
        sha256:462174c7abda6c221ffc4ae8c6e893f437bd0b2cbddc6abb4886abf3bf17f978

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
