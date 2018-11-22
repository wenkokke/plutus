module Wallet where
import Bootstrap (btn, btnSecondary, btnSmall, card, cardBody_, cardTitle_, card_, col4_, pullRight, row_, btnGroupVertical, btnBlock)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Newtype (unwrap)
import Halogen (HTML)
import Halogen.HTML (ClassName(ClassName), button, div, div_, h3_, span, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, classes)
import Icons (Icon(..), icon)
import Playground.API (FunctionSchema, SimpleArgumentSchema)
import Prelude (show, ($), (<$>))
import Types (MockWallet, Query(AddAction, AddWallet, RemoveWallet), toValueLevel)
import Wallet.Emulator.Types (Wallet)

walletsPane ::
  forall p.
  Array (FunctionSchema SimpleArgumentSchema)
  -> Array MockWallet
  -> HTML p Query
walletsPane schemas mockWallets =
  div_
    [ h3_ [ text "Wallets" ]
    , row_ (Array.snoc (mapWithIndex (walletPane schemas) mockWallets) addWalletPane)
    ]

walletPane ::
  forall p.
  Array (FunctionSchema SimpleArgumentSchema)
  -> Int
  -> MockWallet
  -> HTML p Query
walletPane schemas index mockWallet =
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
                , cardTitle_ [walletIdPane mockWallet.wallet ]
                , div_
                    [ text $ show mockWallet . balance
                    , text " ADA"
                    ]
                , div
                    [ classes [ btnGroupVertical, btnBlock ] ]
                    (actionButton mockWallet <$> schemas)
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
  MockWallet
  -> FunctionSchema SimpleArgumentSchema
  -> HTML p Query
actionButton mockWallet functionSchema =
  button
    [ classes [ btn, btnSecondary, btnSmall]
    , onClick $ input_ $ AddAction { functionSchema: toValueLevel functionSchema
                                   , mockWallet
                                   }
    ] []

walletIdPane :: forall p i. Wallet -> HTML p i
walletIdPane wallet =
  span [ class_ $ ClassName "wallet-id" ]
    [ text "Wallet #"
    , text $ show $ (unwrap wallet).getWallet
    ]
