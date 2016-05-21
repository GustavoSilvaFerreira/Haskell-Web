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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clientes json
   nome Text
   deriving Show
   
Produtos json
    nome Text
    valor Double
    deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/cadastro UserR GET POST
/produto ProdR GET POST
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

getUserR :: Handler Html
getUserR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <form>
        Nome: <input type="text" id="usuario">
    <button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{UserR}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#usuario").val()}),
                 success: function(data) {
                     alert(data.resp);
                     $("#usuario").val("");
                 }
            })
         });
     }
  |]

postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])
    -- Linha 60: Le o json {nome:"Teste"} e converte para
    -- Cliente "Teste". O comando runDB $ insert (Clientes "Teste")
    -- {resp:"CREATED"}
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Ola Mundo
|] 

getProdR :: Handler Html
getProdR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <form>
        Nome: <input type="text" id="nameProd">
        Valor: <input type="text" id="valorProd">
    <button #btnProd> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btnProd").click(function(){
             $.ajax({
                contentType: "application/json",
                url: "@{ProdR}",
                type: "POST",
                data: JSON.stringify({"nome":$("#nameProd").val(),"valor":parseFloat($("#valorProd").val())}),
                success: function(data) {
                    alert(data.resp);
                    $("#nameProd").val("");
                    $("#valorProd").val("");
                }
            })
         });
     }
  |]

postProdR :: Handler ()
postProdR = do
    produto <- requireJsonBody :: Handler Produtos
    runDB $ insert produto
    sendResponse (object [pack "resp" .= pack "CREATED"])
    
connStr = "dbname=d6529hkmav9lbj host=ec2-23-21-165-183.compute-1.amazonaws.com user=tasgzhlszohdqq password=wIvg8DoJ9-USazCBgU2kt8DYX9 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


