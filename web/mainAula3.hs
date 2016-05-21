{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina
-- 15, 16, 17, 21 e 22
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clientes json
   nome Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/cadastro UserR GET POST
/cadastro/check/#ClientesId CheckR GET
/cadastro/update/#ClientesId UpdateR PUT
/cadastro/delete/#ClientesId DeleteR DELETE
--/cadastro/#Text BuscaR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------
{-
getBuscaR :: Clientes -> Handler ()
getBuscaR nome = do
    cliente <- runDB $ get404 nome
    sendResponse $ toJSON cliente
-}
getVendaCliR :: ClientesId -> Handler ()    
getVendaCliR pid = do
    clis <- runDB $ (rawSql (pack $ "SELECT ??, ?? FROM produtoz  \ 
        \ INNER JOIN clientes_produtos ON produtoz.id=clientes_produtos.id \ 
        \ INNER JOIN clientes ON  clientes.id=clientes_produtos.id \
        \ and clientes.id = " ++ show pid) []) :: Handler [(Entity Produtoz,Entity Clientes)]
    sendResponse (object [pack "data" .= fmap (toJSON . fst) clis])

deleteDeleteR :: ClientesId -> Handler ()
deleteDeleteR pid = do
    runDB $ delete pid
    sendResponse (object [pack "resp" .= pack "DELETE"])
    
postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

getUserR :: Handler ()
getUserR = do
    allclientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allclientes])

getCheckR :: ClientesId -> Handler ()
getCheckR pid = do
    cli <- runDB $ get404 pid
    sendResponse $ toJSON cli

putUpdateR :: ClientesId -> Handler ()
putUpdateR pid = do
    cli <- requireJsonBody :: Handler Clientes
    runDB $ update pid [ClientesNome =. clientesNome cli]
    sendResponse (object [pack "resp" .= pack "UPDATED"])

connStr = "dbname=d6529hkmav9lbj host=ec2-23-21-165-183.compute-1.amazonaws.com user=tasgzhlszohdqq password=wIvg8DoJ9-USazCBgU2kt8DYX9"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


