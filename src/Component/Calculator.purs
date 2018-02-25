module Component.Calculator
    ( calcUi
    , Query(..)
    , VTCEffects
    , initialState
    ) where

import Data.Error
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HEF
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Network.HTTP.Affjax as AX
import Component.Footer (genFooter)
import Component.Header (genHeader)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Free (fromEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (encodeJson)
import Data.Array (concat)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (any)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Quote (Quote(..), QuoteArray, decodeQuoteArray)
import Data.QuoteParams (QuoteParams(..), initialQuoteParams)
import Data.Tuple (Tuple(..))
import Halogen.HTML.Core (className)
import Halogen.Query (gets)
import Halogen.Themes.Bootstrap3 (btn, btnBlock, btnLg, checkboxInline, colLg12, colMd12, colMd2, colMd3, colMd7, colSm12, colSm2, colSm3, colSm7, colXs2, colXs3, colXs7, containerFluid, formControl, row, textCenter)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (type (~>), bind, const, flip, map, pure, show, ($), ($>), (*>), (<<<), (==))

-- component state
type State =
    { busy           :: Boolean
    , secondAge      :: Boolean
    , provinceSelect :: Boolean
    , disabledButton :: Boolean
    , quoteParams    :: QuoteParams
    , quotes         :: QuoteArray
    , errorMessage   :: Maybe String
    }

-- | DSL for querying component
data Query a
    = Debug a
    | HandleNumOfVisitorsInput String a
    | HandleVisaTypeInput String a
    | SetProvince String a
    | SetAge Int a
    | SetCoverage Int a
    | SetDays Int a
    | SetPreEx Boolean a
    | RequestQuotes a



-- | The effects used within component
type VTCEffects eff = (ajax :: AX.AJAX, console :: CONSOLE | eff)

toInt :: String -> Int
toInt s = case fromString s of
    Just i -> i
    _      -> 0

-- | initial component state
initialState :: State
initialState =
             { busy: false
             , quoteParams: initialQuoteParams
             , quotes: []
             , secondAge: false
             , provinceSelect: false
             , disabledButton: false
             , errorMessage: Nothing
             }

err :: Maybe String -> String
err mbs = case mbs of
  Nothing -> "all good"
  Just msg -> msg


provincesArray :: Array String
provincesArray = [ "BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU" ]

numberOfVisitors :: Array String
numberOfVisitors = [ "Single", "Couple", "Family" ]

visaTypes :: Array (Tuple String String)
visaTypes = [ Tuple "SuperVisa" "Super Visa", Tuple "NewImmigrants" "New Immigrants"
            , Tuple "TouristVisitor" "Tourist/Visitor", Tuple "IECWorkingHoliday" "IEC / Working Holiday"
            , Tuple "ReturningCanadians" "Returning Canadian", Tuple "ForeignWorkers" "Foreign Worker"
            ]

inputForm :: State -> H.ComponentHTML Query
inputForm st =
    HH.div [ HP.classes [ className "ui-15" ] ] [
      HH.div [ HP.classes [ className "ui-content"] ] [
        HH.div [ HP.classes [ className "container-fluid"] ] [
          HH.div [ HP.classes [ row ] ] [
            HH.div [ HP.classes [ colLg12, colMd12, colSm12, className "ui-padd"] ] [
              HH.div [ HP.classes [ className "ui-form"] ] [
                HH.h2 [ HP.classes [textCenter] ] [ HH.text "Get Quotes" ]
              , HH.form_ [

                  HH.text (err st.errorMessage) ,

                  HH.div [ HP.classes [ className "ui-input"] ] [
                    HH.label_ [ HH.text "Select Visa type"]
                    , HH.select [ HP.classes [ formControl ], HEF.onValueChange (HE.input HandleVisaTypeInput) ] $
                      map (\(Tuple v s) -> HH.option [ HP.value v ] [ HH.text s ]) visaTypes
                  ]

                  , HH.div [ HP.classes [ className "ui-input"] ] $ concat [
                      if st.provinceSelect then [
                        HH.label_ [ HH.text "Select Province" ]
                      , HH.select [ HP.classes [ formControl ], HEF.onValueChange (HE.input SetProvince) ] $
                         map (\abbr -> HH.option [ HP.value abbr ] [ HH.text abbr ]) provincesArray
                      ] else []
                    ]

                  , HH.div [ HP.classes [ className "ui-input"] ] [
                      HH.label_ [ HH.text "Number of visitors" ]
                    , HH.select [ HP.classes [ formControl ], HEF.onValueChange (HE.input HandleNumOfVisitorsInput) ] $
                        map (\v -> HH.option [ HP.value v ] [ HH.text v ]) numberOfVisitors
                  ]
                  , HH.div [ HP.classes [ className "ui-input"], HEF.onValueChange (\i -> HE.input SetAge (toInt i) ) ] [
                    HH.input [ HP.type_ "text", HP.placeholder "Enter your age", HP.classes [formControl] ]
                  , HH.label [ HP.classes [ className "ui-icon" ] ] [ HH.i [HP.class_ (className "fa fa-user")] [] ]
                  ]

                  , HH.div [ HP.classes [ className "ui-input"] ]
                      if st.secondAge then [
                        HH.input [ HP.type_ "text", HP.placeholder "Enter second age", HP.classes [formControl] ]
                      , HH.label [ HP.classes [ className "ui-icon" ] ] [ HH.i [HP.class_ (className "fa fa-user")] [] ]
                     ] else []

                  , HH.div [ HP.classes [ className "ui-input"], HEF.onValueChange (\i -> HE.input SetCoverage (toInt i) ) ] [
                    HH.input [ HP.type_ "text", HP.placeholder "Enter coverage ammount", HP.classes [formControl] ]
                  , HH.label [ HP.classes [ className "ui-icon" ] ] [ HH.i [HP.class_ (className "fa fa-usd")] [] ]
                  ]

                  , HH.div [ HP.classes [ className "ui-input"], HEF.onValueChange (\i -> HE.input SetDays (toInt i) ) ] [
                    HH.input [ HP.type_ "text", HP.placeholder "Enter number of days", HP.classes [formControl] ]
                  , HH.label [ HP.classes [ className "ui-icon" ] ] [ HH.i [HP.class_ (className "fa fa-calendar")] [] ]
                  ]

                  , HH.div [ HP.classes [ className "ui-input"], HEF.onChecked ( HE.input SetPreEx) ] [
                    HH.label [ HP.classes [ checkboxInline ] ] [
                      HH.input [ HP.type_ "checkbox" ]
                    , HH.text "Pre Existing Conditions"
                    ]
                  ]
                  , HH.input [ HP.type_ "submit", HP.value "Get Quotes" , HP.id_ "quoteButton" , HP.classes [btn, btnLg, btnBlock],
                               HE.onClick (\_ -> EH.preventDefault $> Just (HQ.action RequestQuotes))]
                ]
              ]
            ]
          ]
        ]
      ]
    ]

quoteResultPanel :: State -> H.ComponentHTML Query
quoteResultPanel st =
    HH.div [ HE.onClick (HE.input_ Debug) , HP.class_ (className "ui-11") ] [
      HH.div [ HP.classes [containerFluid, className "ui-padd"] ] [
        HH.div [ HP.class_ row ] [
          HH.div [ HP.id_ "quote-panel" ] [
            HH.div [ HP.classes [ className "ui-item-one", className "ui-padd"] ] [
              HH.a [ HP.href "#" ] [
                HH.i [ HP.classes [ className "fa fa-envelope-o"], HP.id_ "quote-panel-email-icon" ] []
              , HH.span [ HP.id_ "quote-panel-email-label" ] [ HH.text "Email Quotes" ]
              ]
              , HH.div [ HP.id_ "quote-panel-deductible-select" ] [
                HH.select [ HP.classes [ formControl ] ] [
                  HH.option_ [ HH.text "0" ]
                , HH.option_ [ HH.text "500" ]
                , HH.option_ [ HH.text "10000" ]
                ]
              ]
              , HH.span [ HP.id_ "quote-panel-deductible-label" ]  [ HH.text "Select Deductible" ]
            ]
          ]
        ]
      ]
    ]

quoteResultList :: State -> H.ComponentHTML Query
quoteResultList st =
    HH.div [ HP.classes [ className "ui-206"] ] $
      flip map st.quotes $ \(Quote q) ->
          HH.div [ HP.classes [ className "ui-outer"] ] [
            HH.div [ HP.classes [ containerFluid ] ] [
              HH.div [ HP.classes [ row ] ] [
                HH.div [ HP.classes [ colMd3, colSm3, colXs3, className "ui-padd" ] ] [
                  HH.div [ HP.classes [ className "ui-logo"] ] [
                    HH.a [ HP.href "#"] [
                      HH.img [ HP.classes [ className "img-responsive"], HP.src q.logoUrl , HP.alt "" ]
                    ]
                  ]
                ]
                , HH.div [ HP.classes [ colMd7, colSm7, colXs7, className "col-pad" ] ] [
                  HH.div [ HP.classes [ className "ui-content"] ] [
                    HH.h4_ [ HH.a [ HP.href "#"] [ HH.text q.companyName ] ]
                  , HH.p_ [ HH.text "Plan Details" ]
                  ]
                ]
                , HH.div [ HP.classes [ colMd2, colSm2, colXs2, className "col-pad" ] ] [
                  HH.h5 [ HP.classes [ textCenter] ] [ HH.text (show q.price) ]
                , HH.div [ HP.classes [className "ui-btn"] ] [
                    HH.a [ HP.href "#" , HP.classes [btn, className "btn-green", className "ui-green"], HP.target "_blank" ] [ HH.text "Buy now" ]
                  ]
                ]
              ]
            ]
          ]

    -- ]

-- | rendering function
render :: State -> H.ComponentHTML Query
render st =
  HH.div_ [
    -- HEADER START --
    genHeader
    -- HEADER END --
    ,
    -- CALCULATOR FORM START --
    inputForm st
    -- CALCULATOR FORM END --
    ,
    -- QUOTE RESULTS PANEL START --
    quoteResultPanel st
    -- QUOTE RESULTS PANEL END --
    ,
    -- QUOTE RESULTS START --
    quoteResultList st
    -- QUOTE RESULTS END --
    ,
    -- FOOTER START --
    genFooter
    -- FOOTER END --
  ]

-- | evaluating component interaction DSL
eval :: forall eff. Query ~> H.ComponentDSL State Query (Aff (VTCEffects eff))
eval = case _ of
    Debug next -> do
      qp <- gets _.quoteParams
      fromEff $ log (show qp)
      pure next
    HandleNumOfVisitorsInput numOfp next -> do
        H.modify (setNumOfPpl numOfp)
        case numOfp of
            "Single" -> H.modify (_{secondAge = false }) *> pure next
            _        -> H.modify (_{secondAge = true }) *> pure next
    HandleVisaTypeInput visaType next -> do
        -- update visa type in quote params
        H.modify (setVisaType visaType )
        -- show/hide province input
        if any ((==) visaType) ["NewImmigrants", "ForeignWorkers", "ReturningCanadians"]
          then H.modify (_{ provinceSelect = true }) *> pure next
          else H.modify (_{ provinceSelect = false }) *> pure next
    SetProvince p next -> do
        -- update province in quote params
        H.modify (setProvince p)
        pure next
    SetAge age next -> do
      H.modify (setAge age)
      pure next
    SetCoverage cov next -> do
      H.modify (setCoverage cov)
      pure next
    SetDays days next -> do
      H.modify (setDays days)
      pure next
    SetPreEx val next -> do
      H.modify (setPreEx val)
      pure next
    RequestQuotes next -> do
      params <- gets _.quoteParams
      H.modify (_{busy = true})
      fromEff $ log (show $ encodeJson params)
      result <-  H.fromAff $ attempt $ AX.post "http://localhost:8000/visitors/quotes" (encodeJson params)
      case result of
          Left e -> do
            -- could not request.smth is wrong with the server
            H.modify (_{busy = false})
            fromEff $ log "REQUEST FAILED" *> log (show e)
            pure next
          Right resp -> do
            case resp.status of
              StatusCode 400 -> do
                  H.modify ( _{busy = false, errorMessage = Just <<< either (const "couldn't parse error") getMessage $ parseError resp.response } )
                  pure next
              _  -> case decodeQuoteArray resp.response of
                  Left errMsg -> (fromEff $ log errMsg) *> H.modify (_{busy = false, errorMessage = Just "couldn't parse quotes array"}) *> pure next
                  Right quotesArray -> H.modify (_{busy = false, quotes = quotesArray}) *> pure next


setVisaType :: String -> State -> State
setVisaType v st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {visitorsType = v}

setNumOfPpl :: String -> State -> State
setNumOfPpl num st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {numOfPpl = num}

setProvince :: String -> State -> State
setProvince pr st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {province = Just pr}

setAge :: Int -> State -> State
setAge age st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {age = age}

setCoverage :: Int -> State -> State
setCoverage cov st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {coverage = cov}

setDays :: Int -> State -> State
setDays days st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {duration = days}

setPreEx :: Boolean -> State -> State
setPreEx val st@{ quoteParams: QuoteParams ps} = st { quoteParams = newQp }
    where newQp = QuoteParams $ ps {preEx = val}

-- | component itself
calcUi :: forall eff. H.Component State Query (Aff (VTCEffects eff))
calcUi = H.component { render, eval }
