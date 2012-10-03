{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell,
 MultiParamTypeClasses #-}

module Main where

import Yesod
import Data.List
import Control.Concurrent.Chan (Chan, dupChan, writeChan, newChan)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Concurrent.MVar
import System.Environment (getArgs)

data OneZero = OneZero {
  score   :: MVar Int,
  channel :: Chan ServerEvent
}

instance Yesod OneZero

main :: IO ()
main = do
  score   <- newMVar 1
  channel <- newChan
  args    <- getArgs
  warpDebug (getPort args) (OneZero score channel)

getPort :: [String] -> Int
getPort args = extractPort $ "-p" `elemIndex` args
  where extractPort (Just index) = read $ args !! (index+1)
        extractPort (Nothing) = 3100

mkYesod "OneZero" [parseRoutes|
/      HomeR    GET
/send  SendR    POST
/recv  ReceiveR GET
|]

instance RenderMessage OneZero FormMessage

postSendR :: Handler ()
postSendR = do
  onezero <- getYesod
  body <- runInputGet $ ireq textField "vote"

  liftIO $ modifyMVar (score onezero) $ if body == "no"
    then (\score -> return ( max (score-1) 0 , score))
    else (\score -> return ( score+1         , score))

  score <- liftIO $ readMVar $ score onezero

  let msg = (show score)
  let event = ServerEvent Nothing Nothing
    in liftIO $ writeChan (channel onezero) $ event $ return $ fromString msg

getReceiveR :: Handler ()
getReceiveR = do
  onezero    <- getYesod
  channel  <- liftIO $ dupChan $ channel onezero
  request  <- waiRequest
  response <- lift $ eventSourceApp channel request
  sendWaiResponse response

getHomeR :: Handler RepHtml
getHomeR = do
  onezero <- getYesod
  score <- liftIO $ readMVar $ score onezero
  hamletToRepHtml [hamlet|
    $doctype 5
      <html>
        <head>
          <title> OneZero - the binary voting system
      <body>
        <h1 id=score> #{show score}
        <div id=buttons>
          <input id=yes type=submit value=Yes onClick=window.onezero.voteYes()>
          <input id=no  type=submit value=No  onClick=window.onezero.voteNo()>
        <script>
          var send = function(vote) {
            var xhr = new XMLHttpRequest();
            var params = "?vote=" + encodeURI(vote);
            xhr.open("POST", "@{SendR}" + params);
            xhr.send(null);
          };

          window.onezero = {
            voteYes: function(){
              document.getElementById("yes").disabled = true;
              document.getElementById("no").disabled  = false;
              send('yes');
            },
            voteNo:  function(){
              document.getElementById("yes").disabled = false;
              document.getElementById("no").disabled  = true;
              send('no');
            }
          };

          document.getElementById("no").disabled  = true;

          var output = document.getElementById("score");
          var src    = new EventSource("@{ReceiveR}");
          src.onmessage = function(message) {
            output.innerHTML = message.data;
          }
        <style>
          body { background: rgb(253,246,227); } h1 {color: rgb(38,139,210); text-shadow: rgba(0,0,0,0.3) 0 0 10px; font-size: 96px;text-align: center;}#buttons { text-align: center;}#buttons input {font-size: 30px;  border-radius: 30px; border: 0px; padding: 5px 15px; background:rgb(52,171,132); box-shadow: rgba(12,31,21,0.9) 0px 1px 3px 1px; color: #333} #buttons input[disabled] {box-shadow: rgba(20,10,10,0.5) 0px 2px 5px 1px inset; background:rgb(42,161,132); color: #FFF}
    |]
