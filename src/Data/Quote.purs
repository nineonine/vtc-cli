module Data.Quote where

import Data.Argonaut (fromArray, class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, (.?), (:=), (~>))
import Data.Either (Either)
import Data.Traversable (traverse)
import Prelude (bind, pure, ($), (<$>), (>>=))

newtype Quote = Quote
  { pec         :: Boolean
  , cid         :: Int
  , companyName :: String
  , planName    :: String
  , logoUrl     :: String
  , description :: String
  , rate        :: Number
  , deductible  :: Int
  , price       :: Number
  }

instance decodeJsonQuote :: DecodeJson Quote where
  decodeJson json = do
    obj <- decodeJson json
    pec <- obj .? "pec"
    cid <- obj .? "cid"
    companyName <- obj .? "company_name"
    planName  <- obj .? "plan_name"
    logoUrl <- obj .? "logo_url"
    description <- obj .? "description"
    rate  <- obj .? "rate"
    deductible <- obj .? "deductible"
    price <- obj .? "price"
    pure $ Quote { pec, cid, companyName, planName, logoUrl,
                   description, rate ,deductible, price }

instance encodeJsonQuote :: EncodeJson Quote where
  encodeJson (Quote quote)
    = "pec" := quote.pec
   ~> "cid" := quote.cid
   ~> "company_name" := quote.companyName
   ~> "plan_name" := quote.planName
   ~> "logo_url" := quote.logoUrl
   ~> "description" := quote.description
   ~> "rate" := quote.rate
   ~> "deductible" := quote.deductible
   ~> "price" := quote.price

type QuoteArray = Array Quote


decodeQuoteArray :: Json -> Either String QuoteArray
decodeQuoteArray json = decodeJson json >>= traverse decodeJson

encodeJsonArray :: QuoteArray -> Json
encodeJsonArray qa = fromArray $ encodeJson <$> qa
