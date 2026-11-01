{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ServerUri
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pathUri" $ do
    it "prefixes a plain path with the file:// scheme" $
      pathUri "/tmp/main.tynix" `shouldBe` "file:///tmp/main.tynix"

    it "percent-encodes spaces" $
      pathUri "/tmp/with space/main.tynix"
        `shouldBe` "file:///tmp/with%20space/main.tynix"

    it "percent-encodes non-ASCII identifiers in path segments" $
      pathUri "/tmp/\x578b.tynix" `shouldBe` "file:///tmp/%E5%9E%8B.tynix"

  describe "uriPath" $ do
    it "drops the file:// scheme and returns the underlying path" $
      uriPath "file:///tmp/main.tynix" `shouldBe` "/tmp/main.tynix"

    it "round-trips through percent decoding" $
      uriPath "file:///tmp/with%20space/main.tynix"
        `shouldBe` "/tmp/with space/main.tynix"

    it "tolerates a localhost authority" $
      uriPath "file://localhost/tmp/main.tynix" `shouldBe` "/tmp/main.tynix"

    it "leaves a non-URI path untouched" $
      uriPath "/tmp/main.tynix" `shouldBe` "/tmp/main.tynix"

  describe "percentEncode / percentDecode" $ do
    it "round-trips spaces" $
      percentDecode (percentEncode "with space") `shouldBe` "with space"

    it "round-trips unicode" $
      percentDecode (percentEncode "型情報") `shouldBe` "型情報"

    it "leaves unreserved characters untouched" $
      percentEncode "abc-_.~/0" `shouldBe` "abc-_.~/0"
