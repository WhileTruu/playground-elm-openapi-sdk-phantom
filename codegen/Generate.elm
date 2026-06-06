module Generate exposing (main)

import Elm
import Elm.Arg
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import OpenApi exposing (OpenApi)
import Elm.Case
import Json.Decode as JD
import Set exposing (Set)
import OpenApi.Path exposing (Path)
import OpenApi.Operation exposing (Operation)
import OpenApi.RequestBody exposing (RequestBody)
import OpenApi.Reference exposing (ReferenceOr, Reference)
import OpenApi.Response exposing (Response)
import OpenApi.MediaType exposing (MediaType)
import OpenApi.Components
import OpenApi.Schema exposing (Schema)
import Dict
import Gen.Http
import Gen.Debug
import Gen.Result
import Gen.Platform.Cmd
import Gen.Json.Decode
import Gen.Json.Encode
import Json.Schema.Definitions as JSD


main : Program JD.Value () ()
main =
  Generate.fromJson OpenApi.decode <| \spec ->
    [ Elm.fileWith [ "Sdk" ]
        { docs = "# Sdk"
        , aliases =
            [ (["Sdk", "Json", "Encode"], "Encode")
            , (["Sdk", "Json", "Decode"], "Decode")
            ]
        } <|
        [ Elm.expose <| Elm.customTypeWith "Body" ["supported"]
            [Elm.variantWith "Body" [Gen.Json.Encode.annotation_.value]]
        , Elm.expose <| Elm.declaration "jsonBody" <| Elm.fn
            ( Elm.Arg.varWith "value"
                (Type.namedWith ["Sdk", "Json", "Encode"] "Value" [Type.var "supported"])
            )
            ( \value ->
                Elm.apply (Elm.val "Body")
                  [ Elm.apply
                      (Elm.value
                         { importFrom = ["Sdk", "Json", "Encode"]
                         , name = "toRaw"
                         , annotation = Nothing
                         }
                      )
                      [value]
                  ]
                  |> Elm.withType (Type.namedWith [] "Body" [Type.var "supported"])
            )
        , Elm.expose <| Elm.customTypeWith "Expect" ["supported", "msg"]
            [ Elm.variantWith "Expect"
                [ Type.function [Gen.Http.annotation_.error] (Type.var "msg")
                , Gen.Json.Decode.annotation_.decoder (Type.var "msg")
                ]
            ]
        , Elm.expose <| Elm.declaration "expectJson" <| Elm.fn2
            ( Elm.Arg.varWith "onResponse"
                ( Type.function
                    [Type.result Gen.Http.annotation_.error (Type.var "a")]
                    (Type.var "msg")
                )
            )
            ( Elm.Arg.varWith "decoder"
                ( Type.namedWith ["Sdk", "Json", "Decode"] "Decoder"
                    [Type.var "supported", Type.var "a"]
                )
            )
            ( \onResponse decoder ->
                Elm.apply (Elm.val "Expect")
                  [ Elm.fn (Elm.Arg.var "a")
                      (\a -> Elm.apply onResponse [Gen.Result.make_.err a])
                  , Gen.Json.Decode.map
                      (\a -> Elm.apply onResponse [Gen.Result.make_.ok a])
                      ( Elm.apply
                          (Elm.value
                             { importFrom = ["Sdk", "Json", "Decode"]
                             , name = "toRaw"
                             , annotation = Nothing
                             }
                          )
                          [decoder]
                      )
                  ]
                  |> Elm.withType
                       ( Type.namedWith [] "Expect"
                           [Type.var "supported", Type.var "msg"]
                       )
            )
        , Elm.aliasWith "Required" ["a"]
            (Type.extensible "a" [("required", Type.named ["Sdk","Key"] "Supported")])
        ]
          ++ ( List.map Elm.expose <|
                 Dict.foldl
                   (\k v acc -> declarationsFromPath spec k v ++ acc) []
                   (OpenApi.paths spec)
             )
          ++ ( case OpenApi.components spec of
                 Just components ->
                   List.map Elm.expose <| Dict.foldl
                     ( \k v acc ->
                         Elm.alias k (createRequestBodyComponentAnnotation v)
                           :: acc
                     )
                     []
                     (OpenApi.Components.requestBodies components)

                 Nothing ->
                   []
             )

          ++ ( case OpenApi.components spec of
                 Just components ->
                   List.map Elm.expose <| Dict.foldl
                     ( \k v acc ->
                         Elm.alias k (createResponseComponentAnnotation v) :: acc
                     )
                     []
                     (OpenApi.Components.responses components)

                 Nothing ->
                   []
             )
          ++ ( case OpenApi.components spec of
                 Just components ->
                   List.map Elm.expose <| Dict.foldl
                     (\k v acc -> Elm.alias k (schemaToAnnotation v) :: acc)
                     []
                     (OpenApi.Components.schemas components)

                 Nothing ->
                   []
             )
    , let
        keys = Set.union schemaKeys responseKeys

        responseKeys =
          case OpenApi.components spec of
            Just components ->
              Dict.foldl
                ( \k v acc ->
                    OpenApi.Reference.toConcrete v
                      |> Maybe.andThen
                           (\response ->
                              Dict.get "application/json" (OpenApi.Response.content response)
                                |> Maybe.andThen (OpenApi.MediaType.schema)
                                |> Maybe.map (schemaToKeys)
                                |> Maybe.map (Set.union acc)
                           )
                      |> Maybe.withDefault acc
                )
                Set.empty
                (OpenApi.Components.responses components)
            Nothing -> Set.empty

        schemaKeys =
          case OpenApi.components spec of
            Just components ->
              Dict.foldl (\_ a acc -> Set.union acc (schemaToKeys a))
                Set.empty
                (OpenApi.Components.schemas components)
            Nothing -> Set.empty
      in
      Elm.fileWith [ "Sdk", "Key" ]
        { docs = "# Sdk keys"
        , aliases = []
        } <|
        [ Elm.expose <| Elm.customType "Supported" [Elm.variant "Supported"]
        , Elm.expose <| Elm.customTypeWith "Key" [ "provides", "supported" ]
            [Elm.variantWith "Key" [ Type.string ]]
        , Elm.expose <| Elm.declaration "toValue"
            (Elm.fn
              ( Elm.Arg.varWith "key"
                 (Type.namedWith [] "Key" [ Type.var "provides", Type.var "supported"]))
              (\a -> Elm.unwrap [] "Key" a |> Elm.withType Type.string)
            )
        ]
        ++ ( case OpenApi.components spec of
               Just components ->
                 Set.toList keys
                   |> List.map (\key ->
                        Elm.expose <| Elm.declaration key
                          ( Elm.apply (Elm.value { importFrom = [], name = "Key", annotation = Nothing })
                              [Elm.string key]
                              |> Elm.withType
                                   ( Type.namedWith [] "Key" <|
                                       [ Type.extensible "fields" [ ( key, Type.var "a" ) ]
                                       , Type.var "a"
                                       ]
                                   )
                          )

                      )

               Nothing ->
                 []
           )
    ]


declarationsFromPath : OpenApi -> String -> Path -> List Elm.Declaration
declarationsFromPath openApi key path =
  List.filterMap identity
    [ Maybe.map
        (\a -> declarationsFromOperation openApi key path "post" a)
        (OpenApi.Path.post path)
    , Maybe.map
        (\a -> declarationsFromOperation openApi key path "get" a)
        (OpenApi.Path.get path)
    , Maybe.map
        (\a -> declarationsFromOperation openApi key path "put" a)
        (OpenApi.Path.put path)
    , Maybe.map
        (\a -> declarationsFromOperation openApi key path "delete" a)
        (OpenApi.Path.delete path)
    ]


declarationsFromOperation : OpenApi -> String -> Path -> String -> Operation -> Elm.Declaration
declarationsFromOperation openApi key path method operation =
  let
    name = OpenApi.Operation.operationId operation |> Maybe.withDefault key
    bodyType_ = Type.unit
  in
    Elm.declaration name <| Elm.fn
      ( Elm.Arg.varWith "args" <| Type.record <| List.filterMap identity
          [ Maybe.map
              ( \body ->
                  ("body", Type.namedWith [] "Body" [createRequestBodyAnnotation body])
              )
              (OpenApi.Operation.requestBody operation)
          , Just
              ( "expect"
              , Type.namedWith [] "Expect"
                  [ Dict.get "200" (OpenApi.Operation.responses operation)
                      |> maybeOrElse (Dict.get "201" (OpenApi.Operation.responses operation))
                      |> Maybe.map (\res -> createSuccessResponseAnnotation res)
                      |> Maybe.withDefault (Type.var "supported")
                  , Type.var "msg"
                  ]
              )
          ]
      )
      ( \args ->
          Gen.Http.request
            { method = method
            , headers = []
            , url = key
            , body =
                case OpenApi.Operation.requestBody operation of
                  Just _ -> Gen.Http.jsonBody (Elm.unwrap [] "Body" (Elm.get "body" args))
                  Nothing -> Gen.Http.emptyBody
            , expect =
                Elm.Case.custom (Elm.get "expect" args)
                  (Type.namedWith [] "Expect" [Type.var "msg"])
                  [ Elm.Case.branch
                      (Elm.Arg.customType "Expect" Tuple.pair
                          |> Elm.Arg.item (Elm.Arg.var "fromErr")
                          |> Elm.Arg.item (Elm.Arg.var "decoder")
                      )
                      (\(fromErr, decoder) ->
                          Gen.Http.expectJson
                            ( \a ->
                                Gen.Result.caseOf_.result a
                                  { ok = identity
                                  , err = \err -> Elm.apply fromErr [err]
                                  }
                            )
                            decoder
                      )
                  ]
            , timeout = Elm.nothing
            , tracker = Elm.nothing
            }
            |> Elm.withType (Gen.Platform.Cmd.annotation_.cmd (Type.var "msg"))
      )


maybeOrElse : Maybe a -> Maybe a -> Maybe a
maybeOrElse ma mb =
  case mb of
    Nothing -> ma
    Just _ -> mb


createRequestBodyAnnotation : ReferenceOr RequestBody -> Type.Annotation
createRequestBodyAnnotation body =
  OpenApi.Reference.toReference body
    |> Maybe.map (\a -> Type.named [] (formatReferenceName (OpenApi.Reference.ref a)))
    |> Maybe.withDefault Type.unit


formatReferenceName : String -> String
formatReferenceName name =
  name
    |> String.replace "#/components/requestBodies/" ""
    |> String.replace "#/components/responses/" ""
    |> String.replace "#/components/schemas/" ""


createSuccessResponseAnnotation : ReferenceOr Response -> Type.Annotation
createSuccessResponseAnnotation response =
  OpenApi.Reference.toReference response
    |> Maybe.map (\a -> Type.named [] (formatReferenceName (OpenApi.Reference.ref a)))
    |> Maybe.withDefault Type.unit


createRequestBodyComponentAnnotation : ReferenceOr RequestBody -> Type.Annotation
createRequestBodyComponentAnnotation refOrBody =
  OpenApi.Reference.toConcrete refOrBody
    |> Maybe.andThen
         (\requestBody ->
             Dict.get "application/json" (OpenApi.RequestBody.content requestBody)
               |> Maybe.andThen (OpenApi.MediaType.schema)
               |> Maybe.map (schemaToAnnotation)
         )
    |> Maybe.withDefault Type.unit


createResponseComponentAnnotation : ReferenceOr Response -> Type.Annotation
createResponseComponentAnnotation refOrRes =
  OpenApi.Reference.toConcrete refOrRes
    |> Maybe.andThen
         (\response ->
             Dict.get "application/json" (OpenApi.Response.content response)
               |> Maybe.andThen (OpenApi.MediaType.schema)
               |> Maybe.map (schemaToAnnotation)
         )
    |> Maybe.withDefault Type.unit


schemaToAnnotation : Schema -> Type.Annotation
schemaToAnnotation schema =
  jsonSchemaToAnnotation <| OpenApi.Schema.get schema


jsonSchemaToAnnotation : JSD.Schema -> Type.Annotation
jsonSchemaToAnnotation schema =
  let supportedType = Type.named ["Sdk","Key"] "Supported" in
  case schema of
    JSD.BooleanSchema bool -> Type.named [] "Never"

    JSD.ObjectSchema subSchema ->
      case subSchema.type_ of
        JSD.AnyType ->
          Maybe.withDefault (Type.var "any") <|
            Maybe.map (\a -> Type.named [] (formatReferenceName a)) subSchema.ref

        JSD.SingleType singleType ->
          case singleType of
            JSD.IntegerType ->
              Type.record [ ("int", supportedType ) ]

            JSD.NumberType ->
              Type.record [ ("num", supportedType) ]

            JSD.StringType ->
              Type.record <| List.filterMap identity
                [ case subSchema.format of
                    Just "password" -> Just ("password", supportedType)
                    Just "date-time" -> Just ("dateTime", supportedType)
                    _ -> Nothing
                , Just ("str", supportedType)
                ]

            JSD.BooleanType ->
              Type.record [ ("bool", supportedType) ]

            JSD.ArrayType ->
              Type.record
                [ ( "array"
                  , case subSchema.items of
                      JSD.NoItems -> Type.named [] "Never"
                      JSD.ItemDefinition a -> jsonSchemaToAnnotation a
                      JSD.ArrayOfItems _ -> Type.named [] "Never"
                  )
                ]

            JSD.ObjectType ->
              Type.record
                [ ( "obj"
                  , Type.record <| Maybe.withDefault [] <| Maybe.map
                      ( \(JSD.Schemata a) ->
                          List.map
                            ( \(k, v) ->
                                let
                                  required =
                                    subSchema.required
                                      |> Maybe.map (List.member k)
                                      |> Maybe.withDefault False
                                in
                                  ( k
                                  , if required then
                                      Type.namedWith [] "Required"
                                        [jsonSchemaToAnnotation v]
                                    else
                                      jsonSchemaToAnnotation v
                                  )
                            )
                            a
                      )
                      subSchema.properties
                  )
                ]

            JSD.NullType ->
              Type.record [ ("null", supportedType) ]

        JSD.NullableType singleType -> Type.named [] "Never"
        JSD.UnionType singleTypeList -> Type.named [] "Never"


schemaToKeys : Schema -> Set String
schemaToKeys schema =
  jsonSchemaToKeys Set.empty <| OpenApi.Schema.get schema


jsonSchemaToKeys : Set String -> JSD.Schema -> Set String
jsonSchemaToKeys found schema =
  case schema of
    JSD.BooleanSchema bool -> found
    JSD.ObjectSchema subSchema ->
      case subSchema.type_ of
        JSD.AnyType -> found
        JSD.SingleType singleType ->
          case singleType of
            JSD.IntegerType -> found
            JSD.NumberType -> found
            JSD.StringType -> found
            JSD.BooleanType -> found

            JSD.ArrayType ->
              case subSchema.items of
                JSD.NoItems -> found
                JSD.ItemDefinition a -> jsonSchemaToKeys found a
                JSD.ArrayOfItems _ -> found

            JSD.ObjectType ->
              Maybe.withDefault found <|
                Maybe.map
                  ( \(JSD.Schemata a) ->
                      List.foldl
                        (\(k, v) acc -> jsonSchemaToKeys (Set.insert k acc) v)
                        found
                        a
                  )
                  subSchema.properties

            JSD.NullType ->
              found

        JSD.NullableType singleType -> found
        JSD.UnionType singleTypeList -> found