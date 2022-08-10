let Package =
      ./package.dhall
        sha256:884bca97ee2b7869042897c175a24d65a2dbbb80957b4546c33924dd36d82fd6

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
