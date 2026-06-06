module Sdk.Key exposing
    ( Supported, Key, toValue, article, articles, articlesCount
    , author, bio, body, comment, comments, createdAt, description
    , email, errors, favorited, favoritesCount, following, id, image
    , password, profile, slug, tagList, tags, title, token
    , updatedAt, user, username
    )

{-|
# Sdk keys

@docs Supported, Key, toValue, article, articles, articlesCount
@docs author, bio, body, comment, comments, createdAt
@docs description, email, errors, favorited, favoritesCount, following
@docs id, image, password, profile, slug, tagList
@docs tags, title, token, updatedAt, user, username
-}



type Supported
    = Supported


type Key provides supported
    = Key String


toValue : Key provides supported -> String
toValue key =
    (\(Key val) -> val) key


article : Key { fields | article : a } a
article =
    Key "article"


articles : Key { fields | articles : a } a
articles =
    Key "articles"


articlesCount : Key { fields | articlesCount : a } a
articlesCount =
    Key "articlesCount"


author : Key { fields | author : a } a
author =
    Key "author"


bio : Key { fields | bio : a } a
bio =
    Key "bio"


body : Key { fields | body : a } a
body =
    Key "body"


comment : Key { fields | comment : a } a
comment =
    Key "comment"


comments : Key { fields | comments : a } a
comments =
    Key "comments"


createdAt : Key { fields | createdAt : a } a
createdAt =
    Key "createdAt"


description : Key { fields | description : a } a
description =
    Key "description"


email : Key { fields | email : a } a
email =
    Key "email"


errors : Key { fields | errors : a } a
errors =
    Key "errors"


favorited : Key { fields | favorited : a } a
favorited =
    Key "favorited"


favoritesCount : Key { fields | favoritesCount : a } a
favoritesCount =
    Key "favoritesCount"


following : Key { fields | following : a } a
following =
    Key "following"


id : Key { fields | id : a } a
id =
    Key "id"


image : Key { fields | image : a } a
image =
    Key "image"


password : Key { fields | password : a } a
password =
    Key "password"


profile : Key { fields | profile : a } a
profile =
    Key "profile"


slug : Key { fields | slug : a } a
slug =
    Key "slug"


tagList : Key { fields | tagList : a } a
tagList =
    Key "tagList"


tags : Key { fields | tags : a } a
tags =
    Key "tags"


title : Key { fields | title : a } a
title =
    Key "title"


token : Key { fields | token : a } a
token =
    Key "token"


updatedAt : Key { fields | updatedAt : a } a
updatedAt =
    Key "updatedAt"


user : Key { fields | user : a } a
user =
    Key "user"


username : Key { fields | username : a } a
username =
    Key "username"