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
Produtox json
   nome Text
   valor Double
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/produto/cadastro ProdutoR GET POST
/produto/checar/#ProdutoxId ChecarProdR GET
/erro ErroR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------

-- Sempre que preciso um form, sera ncessario
-- funcoes deste tipo
formProd :: Form Produtoz
formProd = renderDivs $ Produtox <$>
           areq textField "Nome: " Nothing <*>
           areq doubleField "Valor: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProd
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{ProdutoR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

postProdutoR :: Handler Html
postProdutoR = do
           ((result, _), _) <- runFormPost formProd
           case result of 
               FormSuccess prod -> runDB $ insert prod >>= \piid -> redirect (ChecarProdR piid)
               _ -> redirect ErroR
           
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getChecarProdR :: ProdutozId -> Handler Html
getChecarProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> #{produtozNome produto}  
        <p><b> #{show $ produtozValor produto}
    |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]

connStr = "dbname=d6529hkmav9lbj host=ec2-23-21-165-183.compute-1.amazonaws.com user=tasgzhlszohdqq password=wIvg8DoJ9-USazCBgU2kt8DYX9"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
