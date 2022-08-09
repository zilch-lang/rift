let ExtraPackage =
      ./extra-package.dhall
        sha256:9cb1f6360b61c93ab2d6c680de9a447732e019f1531690b32008cadfe90cd781

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
