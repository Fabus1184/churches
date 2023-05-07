{-# LANGUAGE DataKinds #-}

module Main where

import ChurchBoolean (ChurchBoolean, choice, true)
import ChurchFixed (ChurchFixed)
import ChurchList (ChurchL, fromList)
import ChurchNatural (ChurchNatural)
import ChurchTuple (ChurchTuple, secondCT, toCT)

main :: IO ()
main = do
    let a = 12 :: ChurchNatural
    let b = 2 :: ChurchNatural
    print $ a * b

    let cb = true :: ChurchBoolean
    print $ choice cb "abc" "def"

    let ct = toCT (3, fromList "def") :: ChurchTuple Int (ChurchL Char)
    print $ secondCT ct

    let cl = fromList [1, 2, 3] :: ChurchL ChurchNatural
    print $ sum cl

    let cf = 1.234567 :: ChurchFixed 3
    print cf
