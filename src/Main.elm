module Main exposing (..)

import Sdk.Json.Encode as Encode
import Sdk.Json.Decode as Decode
import Sdk
import Sdk.Key as Sdk
import Html
import Http


type alias Article =
  { slug : String
  , title : String
  , description : String
  , body : String
  , tagList : List String
  , createdAt : String
  , updatedAt : String
  , favorited : Bool
  , favoritesCount : Int
  , author : Profile
  }


articleDecoder : Decode.Decoder Sdk.Article Article
articleDecoder =
  Decode.required Sdk.slug Decode.string <| \slug ->
  Decode.required Sdk.title Decode.string <| \title ->
  Decode.required Sdk.description Decode.string <| \description ->
  Decode.required Sdk.body Decode.string <| \body ->
  Decode.required Sdk.tagList (Decode.list Decode.string) <| \tagList ->
  Decode.required Sdk.createdAt Decode.string <| \createdAt ->
  Decode.required Sdk.updatedAt Decode.string <| \updatedAt ->
  Decode.required Sdk.favorited Decode.bool <| \favorited ->
  Decode.required Sdk.favoritesCount Decode.int <| \favoritesCount->
  Decode.required Sdk.author profileDecoder <| \profile->
  Decode.succeed
    { slug = slug
    , title = title
    , description = description
    , body = body
    , tagList = tagList
    , createdAt = createdAt
    , updatedAt = updatedAt
    , favorited = favorited
    , favoritesCount = favoritesCount
    , author = profile
    }


type alias Profile =
  { username : String
  , bio : String
  , image : String
  , following : Bool
  }


profileDecoder : Decode.Decoder Sdk.Profile Profile
profileDecoder =
  Decode.required Sdk.username Decode.string <| \username ->
  Decode.optional Sdk.bio Decode.string "" <| \bio ->
  Decode.required Sdk.image Decode.string <| \image ->
  Decode.required Sdk.following Decode.bool <| \following ->
  Decode.succeed
    { username = username
    , bio = bio
    , image = image
    , following = following
    }


createArticle : Cmd Msg
createArticle =
  let
    input : Encode.Value Sdk.NewArticleRequest
    input =
      Encode.object [
        Encode.pair Sdk.article <| Encode.object
          [ Encode.pair Sdk.title <| Encode.string articleTitle
          , Encode.pair Sdk.description <| Encode.string articleDescription
          , Encode.pair Sdk.body <| Encode.string articleBody
          , Encode.pair Sdk.tagList <| Encode.list Encode.string articleTags
          ]
      ]

    decoder : Decode.Decoder Sdk.SingleArticleResponse Article
    decoder =
      Decode.field Sdk.article articleDecoder
  in
    Sdk.createArticle
      { body = Sdk.jsonBody input
      , expect = Sdk.expectJson ArticleCreateReceived decoder
      }


type Msg =
    ArticleCreateReceived (Result Http.Error Article)


articleTitle : String
articleTitle =
  "Honestly, Vibe Coding Wins"


articleDescription : String
articleDescription =
  "Software development is not just code now; it is vibes, AI, and shipping things before thinking too much."


articleBody : String
articleBody =
  "Honestly, devs who do not vibe with LLMs are not just slow; they are being left behind while prompt people build the future better, faster, harder, stronger."


articleTags : List String
articleTags =
  [ "vibecoding", "ai", "shipit", "leftbehind" ]


main =
    Html.text ""
