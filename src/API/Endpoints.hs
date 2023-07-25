{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API.Endpoints where
  
import API.Handlers
import Prelude ()
import Prelude.Compat
import Servant
import Types


type MovieGetAll        =  BasicAuth "user-realm" UserForAuth :>  Get '[JSON] [Movie]
type MovieGetByName     =  BasicAuth "user-realm" UserForAuth :> Capture "name" String :> Get '[JSON] [Movie]
type MoviePost          =  BasicAuth "user-realm" UserForAuth :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
type MovieUpdate        =  BasicAuth "user-realm" UserForAuth :> "update" :> ReqBody '[JSON] Movie :> Post '[JSON] ()
type MovieDelete        =  BasicAuth "user-realm" UserForAuth :> "delete" :>Capture "name" String :> Delete '[JSON] ()
type MovieApi           = MovieGetAll  :<|> MovieGetByName :<|> MoviePost :<|> MovieUpdate :<|> MovieDelete

----User

type UserRegister       =  "register" :> ReqBody '[JSON] User :> Post '[JSON] User
type UserLogin          =  "login"    :> QueryParam "email" String :> QueryParam "password" String :> Get '[JSON] String
type AddFavouriteMovie  =  BasicAuth "user-realm" UserForAuth :>  Capture "movieName" String :> Post '[JSON] ()
type UserApi            = UserRegister :<|> UserLogin :<|> AddFavouriteMovie


movieServer :: Server MovieApi
movieServer = getAllMovies :<|> getMovieByName :<|> postMovie :<|> updateMovieByName :<|> deleteMovieByName


userServer :: Server UserApi
userServer = registerUser :<|> loginUser :<|> addFavMovie


type CombinedAPI = ("user" :> UserApi) :<|> ("movie" :> MovieApi)

server :: Server CombinedAPI
server = userServer :<|> movieServer

basicAuthApi :: Proxy CombinedAPI
basicAuthApi = Proxy