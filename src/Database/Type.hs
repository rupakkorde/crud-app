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

module Database.Type where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time (Day (ModifiedJulianDay))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Movie
      mname String 
      genre String
      rating Double
      Primary mname
      deriving Show

User
      name  String
      age   Int
      email String
      registration_date Day
      password String
      favouriteMovie [String]
      Primary email
      deriving Show
|]