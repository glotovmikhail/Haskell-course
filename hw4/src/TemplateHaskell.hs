{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskell
       ( chooseByIndices
       , AnotherShow (..)
       , deriveAnotherShow
       ) where


import Language.Haskell.TH

import           Data.List           (nub)
import qualified Data.Map            as Map
import qualified Data.Text           as DText


chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices len indices = do
    let unique :: [Int]
        unique = nub indices
    names <- mapM (newName . ('i' :) . show) unique
    let mapping :: Map.Map Int Name
        mapping = Map.fromList (zip unique names)
    let list :: [PatQ]
        list = [maybe wildP varP x | x <- map (mapping Map.!?) [0 .. len - 1]]
    let result :: [ExpQ]
        result = map (varE . (mapping Map.!)) indices
    lamE [tupP list] (tupE result)

class AnotherShow a where
    showText :: a -> DText.Text

deriveAnotherShow :: Name -> Q [Dec]
deriveAnotherShow name = [d| instance AnotherShow $(conT name) where
                                 showText = DText.pack . show |]
