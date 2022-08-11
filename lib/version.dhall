{- |
  A record representing a version according to semantic versioning.
-}
let Ty =
      { {- |
          The major version, incremented only when a major breaking change happens.
        -}
        major : Natural
      , {- |
          The minor version, incremented when a minor breaking change happens.
        -}
        minor : Natural
      , {- |
          The bugfix version.
        -}
        bug : Natural
      }

let Natural/equal =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Natural/equal.dhall
        sha256:7f108edfa35ddc7cebafb24dc073478e93a802e13b5bc3fd22f4768c9b066e60

let Natural/lessThan =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Natural/lessThan.dhall
        sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c

let Bool/or =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Bool/or.dhall
        sha256:5c50738e84e1c4fed8343ebd57608500e1b61ac1f502aa52d6d6edb5c20b99e4

let Bool/not =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Bool/not.dhall
        sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4

let equal =
      λ(v1 : Ty) →
      λ(v2 : Ty) →
            Natural/equal v1.major v2.major
        &&  Natural/equal v1.minor v2.minor
        &&  Natural/equal v1.bug v2.bug

let notEqual = λ(v1 : Ty) → λ(v2 : Ty) → Bool/not (equal v1 v2)

let lessThan =
      λ(v1 : Ty) →
      λ(v2 : Ty) →
        Bool/or
          [ Natural/lessThan v1.major v2.major
          ,     Natural/equal v1.major v2.major
            &&  Natural/lessThan v1.minor v2.minor
          ,     Natural/equal v1.major v2.major
            &&  Natural/equal v1.minor v2.minor
            &&  Natural/lessThan v1.bug v2.bug
          ]

let lessThanEqual = λ(v1 : Ty) → λ(v2 : Ty) → lessThan v1 v2 || equal v1 v2

let greaterThan = λ(v1 : Ty) → λ(v2 : Ty) → Bool/not (lessThanEqual v1 v2)

let greaterThanEqual = λ(v1 : Ty) → λ(v2 : Ty) → Bool/not (lessThan v1 v2)

let show =
      λ(v : Ty) →
        "${Natural/show v.major}.${Natural/show v.minor}.${Natural/show v.bug}"

let {- | A builder to create a version in a more readable way than writing a record. -}
    v =
      λ(major : Natural) →
      λ(minor : Natural) →
      λ(bug : Natural) →
        { major, minor, bug } : Ty

in  { Type = Ty
    , show
    , equal
    , `==` = equal
    , lessThan
    , `<` = lessThan
    , lessThanEqual
    , `<=` = lessThanEqual
    , greaterThan
    , `>` = greaterThan
    , greaterThanEqual
    , `>=` = greaterThanEqual
    , notEqual
    , `!=` = notEqual
    , v
    }
