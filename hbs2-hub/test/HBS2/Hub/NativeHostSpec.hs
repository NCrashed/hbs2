-- | The native messaging host (PEP-24): its framing, its manifests, its flags.
module HBS2.Hub.NativeHostSpec (spec) where

import HBS2.Hub.NativeHost
import HBS2.Hub.CLI.NativeHost
import HBS2.Hub.CLI.Argv (argvAtom)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Config.Suckless (C)
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

-- Write frames to a file and read them back, as the browser and the host do.
roundTrip :: [LBS.ByteString] -> IO [Maybe BS.ByteString]
roundTrip msgs = withSystemTempFile "frames" $ \path h -> do
  hSetBinaryMode h True
  mapM_ (LBS.hPut h . encodeFrame) msgs
  hClose h
  withBinaryFile path ReadMode $ \r -> mapM (const (readFrame r)) (msgs <> ["eof"])

spec :: Spec
spec = describe "PEP-24 native host" $ do

  it "reads back every frame it writes, then the end" $ do
    got <- roundTrip ["{\"id\":1}", "", LBS.replicate 70000 0x61]
    got `shouldBe` [ Just "{\"id\":1}", Just "", Just (BS.replicate 70000 0x61), Nothing ]

  it "writes the length little-endian, as the browsers read it" $
    LBS.take 4 (encodeFrame (LBS.replicate 258 0)) `shouldBe` LBS.pack [2, 1, 0, 0]

  it "treats a frame cut short as the end, not as a message" $
    withSystemTempFile "short" $ \path h -> do
      hSetBinaryMode h True
      LBS.hPut h (LBS.take 6 (encodeFrame "{\"id\":1}"))
      hClose h
      r <- withBinaryFile path ReadMode readFrame
      r `shouldBe` Nothing

  it "lets exactly one extension start it, in each browser's own words" $ do
    let decode = Aeson.decode @Aeson.Object
        Just ff = decode (manifestFor Firefox "/x/native-host" "hbs2-hub@hbs2.app")
        Just ch = decode (manifestFor Chrome "/x/native-host" "abcdefghijklmnopabcdefghijklmnop")
    KM.lookup "name" ff `shouldBe` Just (Aeson.String "app.hbs2.hub")
    KM.lookup "allowed_extensions" ff `shouldBe` Just (Aeson.toJSON ["hbs2-hub@hbs2.app" :: String])
    KM.member "allowed_origins" ff `shouldBe` False
    KM.lookup "allowed_origins" ch
      `shouldBe` Just (Aeson.toJSON ["chrome-extension://abcdefghijklmnopabcdefghijklmnop/" :: String])
    KM.lookup "path" ch `shouldBe` Just (Aeson.String "/x/native-host")

  it "serves by default and installs only when asked" $ do
    let parse = nativeArgs @C . fmap argvAtom
    parse [] `shouldBe` Just (Serve False)
    parse ["--read-only"] `shouldBe` Just (Serve True)
    parse ["--install"] `shouldBe` Just (Install False chromeExtensionId firefoxExtensionId)
    parse ["--install", "--chrome-id", "abc"] `shouldBe` Just (Install False "abc" firefoxExtensionId)
    parse ["--uninstall"] `shouldBe` Just Uninstall
    parse ["--install", "--uninstall"] `shouldBe` Nothing
    -- An id without --install is a mistake, not a server.
    parse ["--chrome-id", "abc"] `shouldBe` Nothing
