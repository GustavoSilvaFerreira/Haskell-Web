{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET POST
/link LinkR GET
|]
-- Yesod eh um typeclass Importante: toda pagina deve ser instancia de Yesod
--O tipo helloWord representa sua pagina, pois eh uma instancia de Yesod
instance Yesod HelloWorld
--Site começa aqui
-- HomeR GET
-- defaultLayout : mostra na tela o html/css/js
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Ola Mundo
    <button onclick="ola()"> Teste
|] >> toWidget [cassius|
    h1
        color:red;
|] >> toWidgetHead [julius|
    function ola(){
        alert("Ola js do haskell");
    }
|]

postHomeR :: Handler Html
postHomeR = defaultLayout $ [whamlet| 
    <p> Ola do POST 
|] 

getLinkR :: Handler Html
getLinkR = defaultLayout $ [whamlet| 
    <a href=@{HomeR}>Pagina inicial  
|] 


main :: IO ()
main = warp 8080 HelloWorld


{-
Executar:
    stack exec -- nome da pasta
    
Parar aplicação:
    no terminal ctl + c
    
para css pode ser lucius (com chaves{}) ou cassius (sem chaves)
-}

