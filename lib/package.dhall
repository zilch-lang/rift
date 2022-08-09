let ExtraPackage =
      ./extra-package.dhall
        sha256:f28295368a791a3e59a71a6c6794205b764726a268499fc2eace1af9eb82d7c1

in  { Type =
          ExtraPackage.Type
        ⩓ { -- | A list of maintainers
            maintainers : List Text
          , -- | Is the package currently broken?
            broken : Bool
          , -- | Is the package deprecated?
            deprecated : Bool
          }
    , default =
      { component = None Text
      , maintainers = [] : List Text
      , broken = False
      , deprecated = False
      }
    }
