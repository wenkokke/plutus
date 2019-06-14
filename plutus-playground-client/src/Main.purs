module Main where

import Prelude

import AjaxUtils (ajaxSettings)
import Control.Coroutine (Consumer, Process, connect, consumer, runProcess)
import Control.Monad.Reader.Trans (runReaderT)
import Data.GraphQL (buildClientSchema, buildSchema)
import Data.Maybe (Maybe(Nothing))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (hoist)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import LocalStorage (RawStorageEvent)
import LocalStorage as LocalStorage
import MainFrame (mainFrame)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  liftEffect $ buildSchema schema >>= traceM
  liftEffect $ buildClientSchema schema >>= traceM
  driver <- runUI (hoist (flip runReaderT ajaxSettings) mainFrame) unit body
  forkAff $ runProcess watchLocalStorageProcess

watchLocalStorageProcess :: Process Aff Unit
watchLocalStorageProcess = connect LocalStorage.listen watchLocalStorage

watchLocalStorage :: forall r. Consumer RawStorageEvent Aff r
watchLocalStorage = consumer \event ->
  do log $ "Got Local Storage Event: " <> show event
     pure Nothing

onLoad :: Unit
onLoad = unsafePerformEffect main

schema :: String
schema = """
{"__schema":{"queryType":{"name":"Query"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"OBJECT","name":"Query","description":"","fields":[{"name":"greeting","description":null,"args":[{"name":"userName","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null},{"name":"userAge","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Int","ofType":null}},"defaultValue":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"contract","description":null,"args":[{"name":"contractArgsSource","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"SourceCode","ofType":null}},"defaultValue":null},{"name":"contractArgsSize","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Int","ofType":null}},"defaultValue":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"CompilationResult","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"KnownCurrency","description":"","fields":[{"name":"hash","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"friendlyName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"knownTokens","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"TokenName","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"CompilationResult","description":"","fields":[{"name":"functionSchema","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"SchemaText","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"knownCurrencies","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"KnownCurrency","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__Directive","description":"","fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"locations","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"__DirectiveLocation","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"args","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__EnumValue","description":"","fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"isDeprecated","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"deprecationReason","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__InputValue","description":"","fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"_type","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"defaultValue","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__Field","description":"","fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"args","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"_type","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"isDeprecated","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"deprecationReason","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__Type","description":"","fields":[{"name":"kind","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"__TypeKind","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"name","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"fields","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":null}],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Field","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"ofType","description":null,"args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"interfaces","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"possibleTypes","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"enumValues","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":null}],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__EnumValue","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"inputFields","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"OBJECT","name":"__Schema","description":"","fields":[{"name":"types","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"queryType","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"mutationType","description":null,"args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"subscriptionType","description":null,"args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"directives","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Directive","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":[],"interfaces":[],"enumValues":[],"possibleTypes":[]},{"kind":"SCALAR","name":"TokenName","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"SCALAR","name":"SchemaText","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"SCALAR","name":"SourceCode","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"SCALAR","name":"Int","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"ENUM","name":"__DirectiveLocation","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"QUERY","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"MUTATION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"SUBSCRIPTION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"FIELD","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"FRAGMENT_DEFINITION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"FRAGMENT_SPREAD","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INLINE_FRAGMENT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"SCHEMA","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"SCALAR","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"OBJECT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"FIELD_DEFINITION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ARGUMENT_DEFINITION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INTERFACE","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"UNION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ENUM","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ENUM_VALUE","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INPUT_OBJECT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INPUT_FIELD_DEFINITION","description":null,"isDeprecated":false,"deprecationReason":null}],"possibleTypes":null},{"kind":"SCALAR","name":"Boolean","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"SCALAR","name":"String","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[],"possibleTypes":null},{"kind":"ENUM","name":"__TypeKind","description":"","fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"SCALAR","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"OBJECT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INTERFACE","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"UNION","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ENUM","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"INPUT_OBJECT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"LIST","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"NON_NULL","description":null,"isDeprecated":false,"deprecationReason":null}],"possibleTypes":null}],"directives":[]}}
"""
