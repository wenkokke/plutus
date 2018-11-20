module Wallet where
import Bootstrap (btn, btnGroup_, btnSecondary, btnPrimary, btnSmall, card, cardBody_, cardFooter_, cardTitle_, card_, col2_, col4_, pullRight, row_)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Newtype (unwrap)
import Halogen (HTML)
import Halogen.HTML (ClassName(ClassName), button, div, div_, h3_, span, strong_, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, classes)
import Icons (Icon(..), icon)
import Playground.API (FunctionSchema, SimpleArgumentSchema)
import Prelude (show, ($), (<$>))
import Types (DummyWallet, Query(..), WalletId(..))

walletsPane ::
  forall p.
  Array (FunctionSchema SimpleArgumentSchema)
  -> Array DummyWallet
  -> HTML p Query
walletsPane schemas wallets =
  div_
    [ h3_ [ text "Wallets" ]
    , row_ (Array.snoc (mapWithIndex (walletPane schemas) wallets) addWalletPane)
    ]

walletPane ::
  forall p.
  Array (FunctionSchema SimpleArgumentSchema)
  -> Int
  -> DummyWallet
  -> HTML p Query
walletPane schemas index wallet =
  col4_
    [ div
        [class_ $ ClassName "wallet"]
        [ card_
            [ cardBody_
                [ 
                  div [class_ $ ClassName "badgePrimary"] [text "1"],
                  button
                    [ classes [ btn, pullRight ]
                    , onClick $ input_ $ RemoveWallet index
                    ]
                    [ icon Close ]
                , cardTitle_ [walletIdPane wallet . walletId]
                , div_ [text $ show wallet . balance, icon Bitcoin]
                ]
            , cardFooter_
                [ btnGroup_
                    (actionButton wallet . walletId <$> schemas)
                ]
            ]
        ]
    ]

addWalletPane :: forall p. HTML p Query
addWalletPane =
  col4_
    [ div
        [ class_ $ ClassName "add-wallet" ]
        [ div [ class_ card
              , onClick $ input_ AddWallet
              ]
            [ cardBody_
                [ 
                  icon Plus,
                  div [] [text "Add Wallet"]
                  ]
            ]
        ]
    ]

actionButton ::
  forall p.
  WalletId
  -> FunctionSchema SimpleArgumentSchema
  -> HTML p Query
actionButton walletId functionSchema =
  button
    [ classes [ btn, btnSecondary, btnSmall]
    , onClick $ input_ $ AddAction { functionSchema, walletId }
    ]
    [ text $ unwrap $ _.functionName $ unwrap functionSchema ]

walletIdPane :: forall p i. WalletId -> HTML p i
walletIdPane (WalletId walletId) =
  span [ class_ $ ClassName "wallet-id" ]
    [ icon CreditCard
    , text " "
    , strong_ [ text walletId ]
    ]
