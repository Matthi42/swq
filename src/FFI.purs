module FFI where

import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Effect (Effect)

foreign import storageGet :: forall a. String -> Effect (Nullable a)
foreign import storageSet :: forall a. String -> a -> Effect Unit
