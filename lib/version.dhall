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

let Natural/lessThan =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Natural/lessThan.dhall

let Bool/or =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Bool/or.dhall

let Bool/not =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Bool/not.dhall

let equal =
      λ(v1 : Ty) →
      λ(v2 : Ty) →
            Natural/equal v1.major v2.major
        &&  Natural/equal v1.minor v2.minor
        &&  Natural/equal v1.bug v2.bug

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

let version =
      λ(major : Natural) →
      λ(minor : Natural) →
      λ(bug : Natural) →
        { major, minor, bug } : Ty

in  { Type = Ty
    , show
    , equal
    , lessThan
    , lessThanEqual
    , greaterThan
    , greaterThanEqual
    , version
    }
