{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API.Handlers where
  
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Servant
import qualified Database.Functions as DbF
import qualified Database.Type as DbT
import Database.Esqueleto (Entity(entityVal))
import Types



getAllMovies :: UserForAuth -> Handler [Movie]
getAllMovies _ = do
                map (\ b -> Movie {mname = DbT.movieMname $ entityVal b,rating = DbT.movieRating $ entityVal b,genre = DbT.movieGenre $ entityVal b}) <$> liftIO DbF.getEveryMovie

getMovieByName :: UserForAuth -> String -> Handler [Movie]
getMovieByName _ movieName = do
                            x <- liftIO $ DbF.getOneMovieByName movieName
                            return $ map (\ b -> Movie {mname = DbT.movieMname $ entityVal b,rating = DbT.movieRating $ entityVal b,genre = DbT.movieGenre $ entityVal b}) x

postMovie:: UserForAuth -> Movie -> Handler Movie
postMovie _ movie = do
                    x <-  liftIO $ DbF.addMovie movie
                    case x of
                        Just _ -> return movie
                        Nothing -> throwError err400

updateMovieByName :: UserForAuth -> Movie -> Handler ()
updateMovieByName  _ movie = do
                            liftIO $ DbF.editMovie movie

deleteMovieByName :: UserForAuth -> String ->  Handler ()
deleteMovieByName _ movieName = do
                                liftIO $ DbF.removeMovie movieName



registerUser :: User -> Handler User
registerUser user = do
                    x <- liftIO $ DbF.addUser user
                    case x of
                      Just _ -> return user
                      Nothing -> throwError err400
loginUser :: Maybe String -> Maybe String ->  Handler String
loginUser email password = do
    case email of
        Just emailx -> case password of
                        Just passwordx -> do
                            x <- liftIO $ DbF.getUserViaMail emailx
                            case x of
                                Just user -> if DbT.userPassword (entityVal user) == passwordx then return $  DbT.userEmail (entityVal user) ++ ":" ++ DbT.userPassword (entityVal user) else throwError err401
                                Nothing -> throwError err400
                        Nothing -> throwError err400
        Nothing -> throwError err400

addFavMovie :: UserForAuth -> String -> Handler ()
addFavMovie user movie = do
    k <- liftIO $ DbF.checkMovieExists movie
    if k
    then  do
        y <- liftIO $ DbF.getUserViaMail (emailAuth user)
        case y of
            Just userdb -> liftIO $ DbF.insertFavMovie (((entityVal userdb){DbT.userFavouriteMovie = movie : DbT.userFavouriteMovie (entityVal userdb) }))
            Nothing   -> throwError err401
    else throwError err400