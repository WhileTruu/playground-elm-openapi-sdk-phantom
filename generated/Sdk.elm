module Sdk exposing
    ( Body, jsonBody, Expect, expectJson, login, createUser
    , getCurrentUser, updateCurrentUser, getTags, followUserByUsername, unfollowUserByUsername, getProfileByUsername, createArticleFavorite
    , deleteArticleFavorite, deleteArticleComment, createArticleComment, getArticleComments, getArticle, updateArticle, deleteArticle
    , getArticlesFeed, createArticle, getArticles, UpdateUserRequest, UpdateArticleRequest, NewUserRequest, NewCommentRequest
    , NewArticleRequest, LoginUserRequest, UserResponse, Unauthorized, TagsResponse, SingleCommentResponse, SingleArticleResponse
    , ProfileResponse, MultipleCommentsResponse, MultipleArticlesResponse, GenericError, EmptyOkResponse, User, UpdateUser
    , UpdateArticle, Profile, NewUser, NewComment, NewArticle, LoginUser, GenericErrorModel
    , Comment, Article
    )

{-|
# Sdk

@docs Body, jsonBody, Expect, expectJson, login, createUser
@docs getCurrentUser, updateCurrentUser, getTags, followUserByUsername, unfollowUserByUsername, getProfileByUsername
@docs createArticleFavorite, deleteArticleFavorite, deleteArticleComment, createArticleComment, getArticleComments, getArticle
@docs updateArticle, deleteArticle, getArticlesFeed, createArticle, getArticles, UpdateUserRequest
@docs UpdateArticleRequest, NewUserRequest, NewCommentRequest, NewArticleRequest, LoginUserRequest, UserResponse
@docs Unauthorized, TagsResponse, SingleCommentResponse, SingleArticleResponse, ProfileResponse, MultipleCommentsResponse
@docs MultipleArticlesResponse, GenericError, EmptyOkResponse, User, UpdateUser, UpdateArticle
@docs Profile, NewUser, NewComment, NewArticle, LoginUser, GenericErrorModel
@docs Comment, Article
-}


import Http
import Json.Decode
import Json.Encode
import Sdk.Json.Decode as Decode
import Sdk.Json.Encode as Encode
import Sdk.Key


type Body supported
    = Body Json.Encode.Value


jsonBody : Encode.Value supported -> Body supported
jsonBody value =
    Body (Encode.toRaw value)


type Expect supported msg
    = Expect (Http.Error -> msg) (Json.Decode.Decoder msg)


expectJson :
    (Result Http.Error a -> msg)
    -> Decode.Decoder supported a
    -> Expect supported msg
expectJson onResponse decoder =
    Expect
        (\a -> onResponse (Result.Err a))
        (Json.Decode.map
             (\mapUnpack -> onResponse (Result.Ok mapUnpack))
             (Decode.toRaw decoder)
        )


type alias Required a =
    { a | required : Sdk.Key.Supported }


login :
    { body : Body LoginUserRequest, expect : Expect UserResponse msg }
    -> Cmd.Cmd msg
login args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/users/login"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


createUser :
    { body : Body NewUserRequest, expect : Expect UserResponse msg }
    -> Cmd.Cmd msg
createUser args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/users"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getCurrentUser : { expect : Expect UserResponse msg } -> Cmd.Cmd msg
getCurrentUser args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/user"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateCurrentUser :
    { body : Body UpdateUserRequest, expect : Expect UserResponse msg }
    -> Cmd.Cmd msg
updateCurrentUser args =
    Http.request
        { method = "put"
        , headers = []
        , url = "/user"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getTags : { expect : Expect TagsResponse msg } -> Cmd.Cmd msg
getTags args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/tags"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


followUserByUsername : { expect : Expect ProfileResponse msg } -> Cmd.Cmd msg
followUserByUsername args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/profiles/{username}/follow"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


unfollowUserByUsername : { expect : Expect ProfileResponse msg } -> Cmd.Cmd msg
unfollowUserByUsername args =
    Http.request
        { method = "delete"
        , headers = []
        , url = "/profiles/{username}/follow"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getProfileByUsername : { expect : Expect ProfileResponse msg } -> Cmd.Cmd msg
getProfileByUsername args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/profiles/{username}"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


createArticleFavorite :
    { expect : Expect SingleArticleResponse msg } -> Cmd.Cmd msg
createArticleFavorite args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/articles/{slug}/favorite"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteArticleFavorite :
    { expect : Expect SingleArticleResponse msg } -> Cmd.Cmd msg
deleteArticleFavorite args =
    Http.request
        { method = "delete"
        , headers = []
        , url = "/articles/{slug}/favorite"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteArticleComment : { expect : Expect EmptyOkResponse msg } -> Cmd.Cmd msg
deleteArticleComment args =
    Http.request
        { method = "delete"
        , headers = []
        , url = "/articles/{slug}/comments/{id}"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


createArticleComment :
    { body : Body NewCommentRequest, expect : Expect SingleCommentResponse msg }
    -> Cmd.Cmd msg
createArticleComment args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/articles/{slug}/comments"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getArticleComments :
    { expect : Expect MultipleCommentsResponse msg } -> Cmd.Cmd msg
getArticleComments args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/articles/{slug}/comments"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getArticle : { expect : Expect SingleArticleResponse msg } -> Cmd.Cmd msg
getArticle args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/articles/{slug}"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateArticle :
    { body : Body UpdateArticleRequest
    , expect : Expect SingleArticleResponse msg
    }
    -> Cmd.Cmd msg
updateArticle args =
    Http.request
        { method = "put"
        , headers = []
        , url = "/articles/{slug}"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteArticle : { expect : Expect EmptyOkResponse msg } -> Cmd.Cmd msg
deleteArticle args =
    Http.request
        { method = "delete"
        , headers = []
        , url = "/articles/{slug}"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getArticlesFeed :
    { expect : Expect MultipleArticlesResponse msg } -> Cmd.Cmd msg
getArticlesFeed args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/articles/feed"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


createArticle :
    { body : Body NewArticleRequest, expect : Expect SingleArticleResponse msg }
    -> Cmd.Cmd msg
createArticle args =
    Http.request
        { method = "post"
        , headers = []
        , url = "/articles"
        , body = Http.jsonBody ((\(Body val) -> val) args.body)
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getArticles : { expect : Expect MultipleArticlesResponse msg } -> Cmd.Cmd msg
getArticles args =
    Http.request
        { method = "get"
        , headers = []
        , url = "/articles"
        , body = Http.emptyBody
        , expect =
            case args.expect of
                Expect fromErr decoder ->
                    Http.expectJson
                        (\expectJsonUnpack ->
                             case expectJsonUnpack of
                                 Result.Ok value ->
                                     value

                                 Result.Err error ->
                                     fromErr error
                        )
                        decoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias UpdateUserRequest =
    { obj : { user : Required UpdateUser } }


type alias UpdateArticleRequest =
    { obj : { article : Required UpdateArticle } }


type alias NewUserRequest =
    { obj : { user : Required NewUser } }


type alias NewCommentRequest =
    { obj : { comment : Required NewComment } }


type alias NewArticleRequest =
    { obj : { article : Required NewArticle } }


type alias LoginUserRequest =
    { obj : { user : Required LoginUser } }


type alias UserResponse =
    { obj : { user : Required User } }


type alias Unauthorized =
    ()


type alias TagsResponse =
    { obj : { tags : Required { array : { str : Sdk.Key.Supported } } } }


type alias SingleCommentResponse =
    { obj : { comment : Required Comment } }


type alias SingleArticleResponse =
    { obj : { article : Required Article } }


type alias ProfileResponse =
    { obj : { profile : Required Profile } }


type alias MultipleCommentsResponse =
    { obj : { comments : Required { array : Comment } } }


type alias MultipleArticlesResponse =
    { obj :
        { articles : Required { array : Article }
        , articlesCount : Required { int : Sdk.Key.Supported }
        }
    }


type alias GenericError =
    GenericErrorModel


type alias EmptyOkResponse =
    ()


type alias User =
    { obj :
        { email : Required { str : Sdk.Key.Supported }
        , token : Required { str : Sdk.Key.Supported }
        , username : Required { str : Sdk.Key.Supported }
        , bio : { str : Sdk.Key.Supported }
        , image : Required { str : Sdk.Key.Supported }
        }
    }


type alias UpdateUser =
    { obj :
        { email : { str : Sdk.Key.Supported }
        , password : { str : Sdk.Key.Supported }
        , username : { str : Sdk.Key.Supported }
        , bio : { str : Sdk.Key.Supported }
        , image : { str : Sdk.Key.Supported }
        }
    }


type alias UpdateArticle =
    { obj :
        { title : { str : Sdk.Key.Supported }
        , description : { str : Sdk.Key.Supported }
        , body : { str : Sdk.Key.Supported }
        }
    }


type alias Profile =
    { obj :
        { username : Required { str : Sdk.Key.Supported }
        , bio : { str : Sdk.Key.Supported }
        , image : Required { str : Sdk.Key.Supported }
        , following : Required { bool : Sdk.Key.Supported }
        }
    }


type alias NewUser =
    { obj :
        { username : Required { str : Sdk.Key.Supported }
        , email : Required { str : Sdk.Key.Supported }
        , password :
            Required { password : Sdk.Key.Supported, str : Sdk.Key.Supported }
        }
    }


type alias NewComment =
    { obj : { body : Required { str : Sdk.Key.Supported } } }


type alias NewArticle =
    { obj :
        { title : Required { str : Sdk.Key.Supported }
        , description : Required { str : Sdk.Key.Supported }
        , body : Required { str : Sdk.Key.Supported }
        , tagList : { array : { str : Sdk.Key.Supported } }
        }
    }


type alias LoginUser =
    { obj :
        { email : Required { str : Sdk.Key.Supported }
        , password :
            Required { password : Sdk.Key.Supported, str : Sdk.Key.Supported }
        }
    }


type alias GenericErrorModel =
    { obj :
        { errors :
            Required { obj :
                { body : Required { array : { str : Sdk.Key.Supported } } }
            }
        }
    }


type alias Comment =
    { obj :
        { id : Required { int : Sdk.Key.Supported }
        , createdAt :
            Required { dateTime : Sdk.Key.Supported, str : Sdk.Key.Supported }
        , updatedAt :
            Required { dateTime : Sdk.Key.Supported, str : Sdk.Key.Supported }
        , body : Required { str : Sdk.Key.Supported }
        , author : Required Profile
        }
    }


type alias Article =
    { obj :
        { slug : Required { str : Sdk.Key.Supported }
        , title : Required { str : Sdk.Key.Supported }
        , description : Required { str : Sdk.Key.Supported }
        , body : Required { str : Sdk.Key.Supported }
        , tagList : Required { array : { str : Sdk.Key.Supported } }
        , createdAt :
            Required { dateTime : Sdk.Key.Supported, str : Sdk.Key.Supported }
        , updatedAt :
            Required { dateTime : Sdk.Key.Supported, str : Sdk.Key.Supported }
        , favorited : Required { bool : Sdk.Key.Supported }
        , favoritesCount : Required { int : Sdk.Key.Supported }
        , author : Required Profile
        }
    }