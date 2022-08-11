module Rift.Internal.Exceptions where

import Control.Exception (Exception)

data RiftException

instance Show RiftException where
  show _ = undefined

instance Exception RiftException
