module Data.QuoteParams
    ( initialQuoteParams
    , QuoteParams(..)

    ) where

import Data.Tuple (Tuple(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Show (class Show)
import Network.HTTP.Affjax.Request (class Requestable)
import Prelude (bind, pure, ($), show)
import Unsafe.Coerce (unsafeCoerce)


newtype QuoteParams = QuoteParams
  { visitorsType :: String
  , numOfPpl     :: String
  , age          :: Int
  , coverage     :: Int
  , duration     :: Int
  , province     :: Maybe String
  , preEx        :: Boolean
  }

derive instance genericQuoteParams :: Generic QuoteParams

instance showQuoteParams :: Show QuoteParams where
  show = gShow

initialQuoteParams :: QuoteParams
initialQuoteParams =  QuoteParams {
    visitorsType: "SuperVisa"
    , numOfPpl: "Single"
    , age: 60
    , coverage: 100000
    , duration: 365
    , province: Nothing
    , preEx: false
}

instance decodeJsonQuoteParams :: DecodeJson QuoteParams where
  decodeJson json = do
    obj <- decodeJson json
    visitorsType <- obj .? "visitors_type"
    numOfPpl <- obj .? "num_of_ppl"
    age <- obj .? "age"
    coverage <- obj .? "coverage"
    duration <- obj .? "duration"
    province <- obj .? "province"
    preEx <- obj .? "pre_ex"
    pure $ QuoteParams { visitorsType, numOfPpl, age, coverage, duration, province, preEx }


instance encodeJsonQuoteParams :: EncodeJson QuoteParams where
  encodeJson (QuoteParams qp)
    = "visitors_type" := qp.visitorsType
    ~> "num_of_ppl"   := qp.numOfPpl
    ~> "age"          := qp.age
    ~> "coverage"     := qp.coverage
    ~> "duration"     := qp.duration
    ~> "province"     := qp.province
    ~> "pre_ex"       := qp.preEx
    ~> jsonEmptyObject

instance requestableQuoteParams :: Requestable QuoteParams where
  toRequest json = Tuple (Just applicationJSON) (unsafeCoerce $ show json)
