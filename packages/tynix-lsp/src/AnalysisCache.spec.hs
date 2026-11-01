{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AnalysisCache
import Data.Either (isLeft)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "AnalysisCache" $ do
  it "starts empty" $
    analysisCacheSize emptyAnalysisCache `shouldBe` 0

  it "stores and retrieves analysis results" $ do
    let key = ("/tmp/main.tynix", "1")
        cache = insertAnalysisCache key (Left "stub") emptyAnalysisCache
    lookupAnalysisCache key cache `shouldSatisfy` maybe False isLeft
    analysisCacheSize cache `shouldBe` 1

  it "treats identical content under different paths as different keys" $ do
    let content = "1"
        cache =
          insertAnalysisCache ("/a.tynix", content) (Left "alpha")
            . insertAnalysisCache ("/b.tynix", content) (Left "beta")
            $ emptyAnalysisCache
    analysisCacheSize cache `shouldBe` 2
    lookupAnalysisCache ("/a.tynix", content) cache `shouldBe` Just (Left "alpha")
    lookupAnalysisCache ("/b.tynix", content) cache `shouldBe` Just (Left "beta")

  it "replaces the entry when the same key is inserted twice" $ do
    let key = ("/tmp/main.tynix", "1")
        cache =
          insertAnalysisCache key (Left "second")
            . insertAnalysisCache key (Left "first")
            $ emptyAnalysisCache
    analysisCacheSize cache `shouldBe` 1
    lookupAnalysisCache key cache `shouldBe` Just (Left "second")

  it "evicts the oldest entries past the capacity bound" $ do
    let capacity = analysisCacheCapacity
        overshoot = capacity + 10
        keys = [("/file" <> show n <> ".tynix", "stub") | n <- [1 .. overshoot]]
        cache =
          foldl
            (\acc (n, key) -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            (zip [1 :: Int ..] keys)
    analysisCacheSize cache `shouldBe` capacity
    -- The first 10 inserts should have been evicted.
    lookupAnalysisCache (head keys) cache `shouldBe` Nothing
    -- The most recent inserts should still be present.
    lookupAnalysisCache (last keys) cache `shouldSatisfy` maybe False isLeft

  it "touches cached entries so eviction follows least-recently-used order" $ do
    let capacity = analysisCacheCapacity
        keys = [("/file" <> show n <> ".tynix", "stub") | n <- [1 .. capacity]]
        fullCache =
          foldl
            (\acc (n, key) -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            (zip [1 :: Int ..] keys)
        (hit, touchedCache) = accessAnalysisCache (head keys) fullCache
        evictedCache = insertAnalysisCache ("/extra.tynix", "stub") (Left "extra") touchedCache
    hit `shouldBe` Just (Left "1")
    analysisCacheSize evictedCache `shouldBe` capacity
    lookupAnalysisCache (head keys) evictedCache `shouldBe` Just (Left "1")
    lookupAnalysisCache (keys !! 1) evictedCache `shouldBe` Nothing

  it "evicts exactly one entry when an insert pushes it over" $ do
    let capacity = analysisCacheCapacity
        keys = [("/file" <> show n <> ".tynix", "stub") | n <- [1 .. capacity]]
        full =
          foldl
            (\acc (n, key) -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            (zip [1 :: Int ..] keys)
        overflowed = insertAnalysisCache ("/extra.tynix", "stub") (Left "extra") full
    analysisCacheSize overflowed `shouldBe` capacity
    -- Only the least recently used key is gone; everything else survives.
    lookupAnalysisCache (head keys) overflowed `shouldBe` Nothing
    [lookupAnalysisCache key overflowed | key <- drop 1 keys]
      `shouldBe` [Just (Left (show n)) | n <- [2 .. capacity]]
    lookupAnalysisCache ("/extra.tynix", "stub") overflowed
      `shouldBe` Just (Left "extra")

  it "does not grow when the same key is rewritten repeatedly" $ do
    let key = ("/tmp/main.tynix", "1")
        cache =
          foldl
            (\acc n -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            [1 :: Int .. 1000]
    analysisCacheSize cache `shouldBe` 1
    lookupAnalysisCache key cache `shouldBe` Just (Left "1000")

  it "keeps every entry that fits, in any insertion order" $ do
    let keys = [("/file" <> show n <> ".tynix", "stub") | n <- [1 .. analysisCacheCapacity]]
        cache =
          foldl
            (\acc (n, key) -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            (zip [1 :: Int ..] (reverse keys))
    analysisCacheSize cache `shouldBe` analysisCacheCapacity
    all (\key -> maybe False isLeft (lookupAnalysisCache key cache)) keys
      `shouldBe` True

  it "survives a burst far larger than its capacity" $ do
    let total = analysisCacheCapacity * 4
        keys = [("/file" <> show n <> ".tynix", "stub") | n <- [1 .. total]]
        cache =
          foldl
            (\acc (n, key) -> insertAnalysisCache key (Left (show n)) acc)
            emptyAnalysisCache
            (zip [1 :: Int ..] keys)
    analysisCacheSize cache `shouldBe` analysisCacheCapacity
    -- The surviving window is the most recent `capacity` inserts.
    lookupAnalysisCache (keys !! (total - analysisCacheCapacity)) cache
      `shouldBe` Just (Left (show (total - analysisCacheCapacity + 1)))
    lookupAnalysisCache (keys !! (total - analysisCacheCapacity - 1)) cache
      `shouldBe` Nothing

  it "leaves the cache alone when a lookup misses" $ do
    let key = ("/tmp/main.tynix", "1")
        cache = insertAnalysisCache key (Left "stub") emptyAnalysisCache
        (hit, untouched) = accessAnalysisCache ("/tmp/other.tynix", "1") cache
    hit `shouldBe` Nothing
    analysisCacheSize untouched `shouldBe` 1
    lookupAnalysisCache key untouched `shouldBe` Just (Left "stub")
