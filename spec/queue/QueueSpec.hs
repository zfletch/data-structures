module QueueSpec (main, spec) where

import Test.Hspec
import Queue

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "null" $ do
    it "is null when queue is empty" $ do
      Queue.null Queue.mkQueue

    it "is not null when queue is empty" $ do
      not $ Queue.null $ Queue.snoc 1 Queue.mkQueue

  describe "head" $ do
    it "gets the head" $ do
      let queue = Queue.snoc 2 $ Queue.snoc 1 Queue.mkQueue
      Queue.head queue == Just 1

    it "returns nothing for an empty queue" $ do
      let queue = Queue.mkQueue :: Queue.Queue Int
      Queue.head queue == Nothing

  describe "tail" $ do
    it "gets the tail" $ do
      let queue = Queue.snoc 3 $ Queue.tail $ Queue.snoc 2 $ Queue.snoc 1 Queue.mkQueue
      Queue.head (Queue.tail queue) == Just 3

    it "returns empty for an empty queue" $ do
      let queue = Queue.mkQueue :: Queue.Queue Int
      Queue.head (Queue.tail queue) == Nothing

  describe "snoc" $ do
    it "adds an element to the queue" $ do
      let queue = Queue.mkQueue
      Queue.head (Queue.snoc 1 queue) == Just 1
