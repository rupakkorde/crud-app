{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}

module Database.Queries where

import Database.Type
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Reader
import qualified Database.Esqueleto as Esq
import qualified Types as T

fetchAllMovies :: SqlPersistM [Entity Movie]
fetchAllMovies = Esq.select $
                   Esq.from $ \movie -> do
                    return movie


fetchOneByName :: String -> SqlPersistM [Entity Movie]
fetchOneByName movieName =  Esq.select $
                             Esq.from $ \movie -> do
                              Esq.where_ (movie Esq.^. MovieMname Esq.==. Esq.val movieName )
                              Esq.limit 1
                              return movie

insertMovie :: MonadIO m => T.Movie ->ReaderT SqlBackend m (Maybe ())
insertMovie m = do
      insertUnique_ $ Movie (T.mname m) (T.genre m) (T.rating m)


updateMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryWrite backend) => T.Movie -> ReaderT backend m ()
updateMovie movie = updateWhere [MovieMname ==. T.mname movie] [MovieRating =. T.rating movie]

deleteMovie :: (MonadIO m, BackendCompatible SqlBackend backend,
 PersistQueryWrite backend, PersistUniqueWrite backend) =>String -> ReaderT backend m ()
deleteMovie movieName =  Esq.delete $
                         Esq.from $ \p -> do
                         Esq.where_ (p Esq.^. MovieMname Esq.==. Esq.val movieName)

insertUser :: (BaseBackend backend ~ SqlBackend, MonadIO m,PersistUniqueWrite backend) =>T.User -> ReaderT backend m (Maybe ())
insertUser user = insertUnique_ $ User (T.name user) (T.age user) (T.email user) (T.registration_date user) (T.password user) (T.favouriteMovie user)

updateUserFavMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryWrite backend) => User -> ReaderT backend m ()
updateUserFavMovie  user = updateWhere [UserEmail ==. userEmail user] [UserFavouriteMovie =. userFavouriteMovie user]


getUserByEmail :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryRead backend) =>String -> ReaderT backend m (Maybe (Entity User))
getUserByEmail email = selectFirst [UserEmail ==. email] []

checkIfMovieExists :: String -> SqlPersistM Bool
checkIfMovieExists movie = Esq.existsBy $ MoviePrimaryKey movie