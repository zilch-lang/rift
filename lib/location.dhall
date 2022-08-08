{- |
  This is basically a hack, but corresponds to the type of an import which is not resolved.
  This way, we will be able to use Dhall's import mecanism (or we should be able to)
  to retrieve archives, paths, etc.
  It's a shame that Dhall does not expose @Location@ to the users.

  For example, writing

  > https://github.com/zilch-lang/stdlib/archive/master.tar.gz as Location

  yields the value

  > < Environment : Text | Local : Text | Missing | Remote : Text >.Remote
  >   "https://github.com/zilch-lang/stdlib/archive/master.tar.gz"



  A simple way to check that this type is correct is to input the following Dhall expression:

  > https://github.com/zilch-lang/stdlib/archive/master.tar.gz as Location : ./location.dhall

  If it succeeds, it should return the same value as above.

  -------------

  We will need to change this type whenever Dhall changes it, but this is not
  such a problem.
-}
< Environment : Text | Local : Text | Missing | Remote : Text >
