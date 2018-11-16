module Action where

import Bootstrap (alertInfo_, bgInfo, btn, btnDanger, btnInfo, btnPrimary, btnSecondary, btnSmall, btnSuccess, card, cardBody_, col_, pullRight, row_, textWhite)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Halogen (HTML)
import Halogen.HTML (ClassName(ClassName), br_, button, div, div_, h3_, hr_, input, small_, text)
import Halogen.HTML.Events (input_, onClick, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(InputText, InputNumber), class_, classes, type_, value)
import Halogen.Query as HQ
import Icons (Icon(..), icon)
import Network.RemoteData (RemoteData(..))
import Prelude (map, pure, show, zero, ($), (<$>), (<<<), (==))
import Servant.PureScript.Affjax (AjaxError)
import Types (Action, FormEvent(..), Query(EvaluateActions, PopulateAction, RemoveAction), SimpleArgument(Unknowable, SimpleObject, SimpleString, SimpleInt), Blockchain)
import Wallet (walletIdPane)

actionsPane :: forall p. Array Action -> RemoteData AjaxError Blockchain -> HTML p Query
actionsPane actions evaluationResult =
  div [ class_ $ ClassName "actions" ]
    [ h3_ [ text "Actions" ]
    , if Array.length actions == zero
      then
        alertInfo_ [ text "Select some actions to run against the blockchain. First choose a wallet from the list on the left, then click one the actions below it to add it to the stack." ]
      else
        div_
          [ div_
             (
               intercalate
                 [ icon LongArrowDown ]
                 (mapWithIndex (\index -> pure <<< actionPane index) actions)
             )
          , br_
          , evaluateActionsPane evaluationResult
          , div_ [ small_ [ text "Run this set of actions against a simulated blockchain." ] ]
          ]
    ]

actionPane :: forall p. Int -> Action -> HTML p Query
actionPane index action =
  div [ class_ $ ClassName "action" ]
    [ div [ classes [ card, textWhite, bgInfo ] ]
      [ cardBody_
        [ button
            [ classes [ btn, btnInfo, pullRight ]
            , onClick $ input_ $ RemoveAction index
            ]
            [ icon Close ]
        , div_ [ walletIdPane action.mockWallet.wallet ]
        , div_ [ text $ unwrap $ _.functionName $ unwrap $ action.functionSchema ]
       , hr_
        , div_
          (intercalate
             [ hr_ ]
             (Array.mapWithIndex
                (\i action -> pure $ (PopulateAction index i) <$> (actionArgumentForm action))
                (_.argumentSchema $ unwrap $ action.functionSchema))
          )
        ]
      ]
    ]


actionArgumentForm :: forall p. SimpleArgument -> HTML p FormEvent
actionArgumentForm (SimpleInt n) =
  div_ [ input
           [ type_ InputNumber
           , value $ maybe "" show n
           , onValueChange $ map (HQ.action <<< SetIntField) <<< Int.fromString
           ]
       ]
actionArgumentForm (SimpleString s) =
  div_ [ input
           [ type_ InputText
           , value $ fromMaybe "" s
           , onValueChange $ HE.input SetStringField
           ]
       ]
actionArgumentForm (SimpleObject subFields) =
  div_ (mapWithIndex (\i field -> map (SetSubField i) (subForm field)) subFields)
  where
    subForm (name /\ arg) =
      (row_ [ col_ [ text name ]
           , col_ [ actionArgumentForm arg ]
           ])
actionArgumentForm Unknowable =
  div_ [ text "UNKNOWN TODO"
       ]

evaluateActionsPane :: forall p. RemoteData AjaxError Blockchain -> HTML p Query
evaluateActionsPane evaluationResult =
  button
    [ classes [ btn, btnClass, btnSmall ]
    , onClick $ input_ EvaluateActions
    ]
    [ btnText ]
  where
    btnClass = case evaluationResult of
                 Success _ -> btnSuccess
                 Failure _ -> btnDanger
                 Loading -> btnSecondary
                 NotAsked -> btnPrimary
    btnText = case evaluationResult of
                 Loading -> icon Spinner
                 _ -> text "Evaluate"
