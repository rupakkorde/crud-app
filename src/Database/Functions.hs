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

module Database.Functions where

import Database.Type
import Database.Queries

import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger
import qualified Types as T
import Data.ByteString(ByteString)


connectionStr :: ByteString
connectionStr = "host=postgres dbname=postgres user=postgres password=postgres port=5432"


getEveryMovie :: IO [Entity Movie]
getEveryMovie = do
             runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
              liftIO (runSqlPersistM fetchAllMovies b))


getOneMovieByName ::  String -> IO [Entity Movie]
getOneMovieByName movieName = do
                  runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                       liftIO (runSqlPersistM (fetchOneByName movieName) b))

addMovie ::  T.Movie -> IO (Maybe ())
addMovie movie = do
                    runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (insertMovie movie) b)

editMovie :: T.Movie -> IO ()
editMovie  movie = do
                    runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (updateMovie movie) b)

removeMovie :: String -> IO ()
removeMovie name = do
                   runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (deleteMovie name) b)

getUserViaMail :: String -> IO (Maybe (Entity User))
getUserViaMail email = do
                     runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                       runSqlConn (getUserByEmail email) b)

addUser :: T.User -> IO (Maybe ())
addUser user = do
                    runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (insertUser user) b)

insertFavMovie :: User -> IO()
insertFavMovie user =  do
                    runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (updateUserFavMovie user) b)

migrationScript :: IO ()
migrationScript = do
                  runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
                        runSqlConn (runMigration migrateAll) b)

checkMovieExists :: String -> IO Bool
checkMovieExists  movieName = do
             runNoLoggingT $ withPostgresqlConn connectionStr (\b -> do
              liftIO (runSqlPersistM (checkIfMovieExists movieName) b))