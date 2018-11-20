module Action where

import Bootstrap (alertInfo_, bgInfo, btn, btnInfo, btnPrimary, btnSmall, card, cardBody_, cardFooter_, col_, col4_, pullRight, row_, textWhite)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Generic (gShow, toSpine)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Halogen (HTML)
import Halogen.HTML (ClassName(ClassName), br_, button, div, div_, h3_, hr_, input, small_, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (InputType(..), class_, classes, placeholder, type_)
import Icons (Icon(..), icon)
import Playground.API (SimpleArgumentSchema(..))
import Prelude (pure, show, zero, ($), (<$>), (<<<), (==))
import Types (Action, Query(..))
import Wallet (walletIdPane)

actionsPane :: forall p. Array Action -> HTML p Query
actionsPane actions =
  row_ [
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
          , evaluateActionsPane
          , div_ [ small_ [ text "Run this set of actions against a simulated blockchain." ] ]
          ]
    ]
  ]

actionPane :: forall p. Int -> Action -> HTML p Query
actionPane index action =
  col4_
    [ div [ class_ $ ClassName "action" ]
      [ div [ classes [ card, textWhite, bgInfo ] ]
        [ cardBody_
          [ button
              [ classes [ btn, btnInfo, pullRight ]
              , onClick $ input_ $ RemoveAction index
              ]
              [ icon Close ]
          , div_ [ walletIdPane action.walletId ]
          , div_ [ text $ unwrap $ _.functionName $ unwrap $ action.functionSchema ]
        , hr_
          , div_
            (intercalate [ hr_ ] (pure <<< actionArgumentForm <$> (_.argumentSchema $ unwrap $ action.functionSchema)))
          ]
        ]
      ]
    ]

actionArgumentForm :: forall p. SimpleArgumentSchema -> HTML p Query
actionArgumentForm SimpleIntArgument =
  div_ [ input
           [ type_ InputNumber
           , placeholder "Int"
           ]
       ]
actionArgumentForm SimpleStringArgument =
  div_ [ input
           [ type_ InputText
           , placeholder "String"
           ]
       ]
actionArgumentForm (SimpleObjectArgument subFields) =
  div_ (sub <$> subFields)
  where
    sub (name /\ arg) =
      row_ [ col_ [ text name ]
           , col_ [ actionArgumentForm arg ]
           ]
actionArgumentForm (UnknownArgument _) =
  div_ [ text "UNKNOWN TODO"
       ]

evaluateActionsPane :: forall p. HTML p Query
evaluateActionsPane =
  button
    [ classes [ btn, btnPrimary, btnSmall ]
    , onClick $ input_ EvaluateActions
    ]
    [ text "Evaluate" ]
