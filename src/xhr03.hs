{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Meteo.Swiss
import           Data.Word
import           Control.Monad (when)

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Weather Data display data or status-code"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  evSend <- button "Send"
  -- Build and send the request
  let evCode = tagPromptlyDyn (value dd) evSend
  evRsp <- performRequestAsync $ fmap buildReq evCode

  -- Display the status
  dynWord <- holdDyn 200 (_xhrResponse_status <$> evRsp)
  dynStatus <- holdDyn "" (_xhrResponse_statusText <$> evRsp)
  let displayStatusCond  =
                (\word status -> when (word  /= 200) $ displayStatus word status)
                  <$>   
                  dynWord 
                  <*>
                  dynStatus
  _ <- dyn displayStatusCond
  -- Display the result
  dynRsp <- holdDyn (Just "")  (_xhrResponse_responseText <$> evRsp)
  let displayRspCond  =
                (\word rsp -> when (word  == 200) $ displayRsp word rsp)
                  <$>   
                  dynWord 
                  <*>
                  dynRsp
  _ <- dyn displayRspCond
  
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" (urlDataStat code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

result :: Maybe T.Text -> T.Text
result (Just t) = t
result Nothing = "Response is Nothing"

word2Txt :: Word -> T.Text 
word2Txt = T.pack . show . word2Int
  where 
    word2Int :: Word -> Int
    word2Int = fromIntegral

displayStatus :: MonadWidget t m => Word -> T.Text ->  m ()
displayStatus code status = do
  el "p"  blank
  text "Statuscode: "
  text $ word2Txt code <> " " <> status

displayRsp :: MonadWidget t m => Word -> Maybe T.Text -> m ()
displayRsp word rsp = do
  el "p" blank
  text "Response Text:"
  el "br" blank
  text $ result rsp