{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Import
import Yesod
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Yesod.Static

import Database.Persist.Postgresql

--connStr = "dbname=d5rq97o0klocmf host=ec2-23-21-165-183.compute-1.amazonaws.com user=nrqrmhgbllkdpm password=sCbudbyKiex_yyIQd_sfZkeGo3 port=5432"
connStr = "dbname=d91vkvcmggiog2 host=ec2-23-21-165-183.compute-1.amazonaws.com user=kcuwgjwyyokquu password=rlyDhJqRqQPKGxy7g2rp-WWfuU port=5432"
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)
       
       
       