module Aula13 where
import Control.Applicative

data Produtos = Produtos {produtosNome :: String,
                            produtosValor :: Double} deriving Show
                            

-- APLICATIVE FUNCTOR

-- Entrar na pasta form e no bash digitar: stack ghci

-- (2*) <$> Just 10
-- fmap (2*) (Just 10)
-- (\x -> x+9) <$> Just 11
-- :t (Just (\x -> x ++ "mundo"))

-- let f = UmaCoisa (\x -> x+1)
-- f <*> (UmaCoisa 5)

{-
<*> funcao com funtor e parametro com funtor
<$> funcao sem funtor e parametro com funtor

soma <$> Just 5 <*> Just 3
Usa-se o <$> pq o soma esta sem funtor
(soma <$> Just 5) <*> Just 3
Just ((\x y -> x+y) 5) = 
Just (\y -> 5+y)

*Aula13> Produtos <$> (Just "Algo") <*> (Just 10)
Just (Produtos {produtosNome = "Algo", produtosValor = 10.0})

areq = para campos obrigatorios
aopt = para campos não obrigatorios
-}