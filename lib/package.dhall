let ExtraPackage =
      ./extra-package.dhall
        sha256:0cee6fd112dd9b821b12b7110f311d9b22d1a3bf74d324e883f39629ea252a4e

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
      { maintainers = [] : List Text, broken = False, deprecated = False }
    }
