{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- carregar mensagensJSON
carregarMensagens :: FilePath -> IO (Map Text [Text])
carregarMensagens path = do
    conteudo <- B.readFile path
    case eitherDecode conteudo of
        Right mensagens -> return mensagens
        Left err -> do
            putStrLn $ "Erro ao carregar JSON: " ++ err
            return Map.empty


calculaSigno :: Int -> Int -> Text
calculaSigno dia mes
    | dia < 1 || dia > 31 || mes < 1 || mes > 12 = "Data inválida"
    | (mes == 3 && dia >= 21) || (mes == 4 && dia <= 19) = "Áries"
    | (mes == 4 && dia >= 20) || (mes == 5 && dia <= 20) = "Touro"
    | (mes == 5 && dia >= 21) || (mes == 6 && dia <= 20) = "Gêmeos"
    | (mes == 6 && dia >= 21) || (mes == 7 && dia <= 22) = "Câncer"
    | (mes == 7 && dia >= 23) || (mes == 8 && dia <= 22) = "Leão"
    | (mes == 8 && dia >= 23) || (mes == 9 && dia <= 22) = "Virgem"
    | (mes == 9 && dia >= 23) || (mes == 10 && dia <= 22) = "Libra"
    | (mes == 10 && dia >= 23) || (mes == 11 && dia <= 21) = "Escorpião"
    | (mes == 11 && dia >= 22) || (mes == 12 && dia <= 21) = "Sagitário"
    | (mes == 12 && dia >= 22) || (mes == 1 && dia <= 19) = "Capricórnio"
    | (mes == 1 && dia >= 20) || (mes == 2 && dia <= 18) = "Aquário"
    | (mes == 2 && dia >= 19) || (mes == 3 && dia <= 20) = "Peixes"
    | otherwise = "Data inválida"

-- MSG
mensagemAleatoria :: Map Text [Text] -> Text -> IO Text
mensagemAleatoria mensagens signo = do
    let mensagensSigno = Map.lookup signo mensagens
    case mensagensSigno of
        Just ms -> do
            index <- randomRIO (0, length ms - 1)
            return $ ms !! index
        Nothing -> return "Nenhuma mensagem disponível para este signo."

-- respostaJSON
formatarRespostaJson :: Text -> Text -> B.ByteString
formatarRespostaJson signo mensagem = 
    encode $ object ["signo" .= signo, "mensagem" .= mensagem]

main :: IO ()
main = do
    mensagens <- carregarMensagens "mensagens.json"  -- Carrega as mensagens do JSON
    scotty 3000 $ do
        middleware logStdoutDev
        get "/signo/:dia/:mes" $ do
            dia <- param "dia" :: ActionM Int
            mes <- param "mes" :: ActionM Int
            
            -- VE SIGNO E MES 
            let signo = calculaSigno dia mes
            mensagem <- liftIO $ mensagemAleatoria mensagens signo
            setHeader "Content-Type" "application/json"
            raw $ formatarRespostaJson signo mensagem  -- Resposta bruta em ByteString
