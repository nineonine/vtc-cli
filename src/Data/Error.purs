module Data.Error where

import Data.Either (Either)
import Prelude (bind, ($), pure)
import Data.Argonaut
import Data.Show (class Show)
import Data.Generic (class Generic, gShow)

newtype VTCError = VTCError
    { error   :: String
    , message :: String
    }

derive instance genericVTCError :: Generic VTCError

instance showVTCError :: Show VTCError where
  show = gShow

instance decodeVTCError :: DecodeJson VTCError where
  decodeJson json = do
    obj     <- decodeJson json
    error   <- obj .?  "error"
    message <- obj .? "message"
    pure $ VTCError { error, message }

parseError :: Json -> Either String VTCError
parseError = decodeJson

getMessage :: VTCError -> String
getMessage (VTCError {error, message} ) = message
