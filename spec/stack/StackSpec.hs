module StackSpec (main, spec) where

import Test.Hspec
import Stack

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "null" $ do
    it "is null when stack is empty" $ do
      Stack.null Stack.mkStack

    it "is not null when stack is empty" $ do
      not $ Stack.null $ Stack.cons 1 Stack.mkStack

  describe "head" $ do
    it "gets the head" $ do
      let stack = Stack.cons 1 Stack.mkStack
      Stack.head stack == Just 1

    it "returns nothing for an empty stack" $ do
      let stack = Stack.mkStack :: Stack.Stack Int
      Stack.head stack == Nothing

  describe "tail" $ do
    it "gets the tail" $ do
      let stack = Stack.cons 2 $ Stack.cons 1 Stack.mkStack
      Stack.head (Stack.tail stack) == Just 1

    it "returns empty for an empty stack" $ do
      let stack = Stack.mkStack :: Stack.Stack Int
      Stack.head (Stack.tail stack) == Nothing

  describe "cons" $ do
    it "adds an element to the stack" $ do
      let stack = Stack.mkStack
      Stack.head (Stack.cons 1 stack) == Just 1
