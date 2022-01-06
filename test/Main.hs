{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Word
import Foreign.Ptr
import Test.Tasty
import Test.Tasty.HUnit

import CodeGen.X86

main :: IO ()
main = 
    defaultMain $
        testGroup "extra-ops"
            [ mulTest ]

mulTest :: TestTree
mulTest = testCase "mul test" $ do 
    let code = saveNonVolatile $ do
          mov rax 0
          mov rax 13
          mov rcx 14
          mul rcx

    value <- (compile code :: IO Word64)
    value @?= 13 * 14

    let code = saveNonVolatile $ do
          mov rax 0
          mov eax 13
          mov ecx 14
          mul ecx

    value <- (compile code :: IO Word64)
    value @?= 13 * 14

    let code = saveNonVolatile $ do
          mov rax 0
          mov al 13
          mov cl 14
          mul cl

    value <- (compile code :: IO Word64)
    value @?= 13 * 14



foreign import ccall "dynamic" callIOVal :: FunPtr (IO Word64) -> IO Word64
instance Callable (IO Word64) where
  dynCCall = callIOVal


