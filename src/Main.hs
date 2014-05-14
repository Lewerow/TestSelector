{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  
-- License     :  AllRightsReserved
--
-- Maintainer  :  
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Environment(getArgs)
import Test.Framework
import Test.Framework.Tutorial
import {-@ HTF_TESTS @-} Gradeable
import DynamicSolving
import {-@ HTF_TESTS @-}  Clusterization
import {-@ HTF_TESTS @-}  Structuring
import {-@ HTF_TESTS @-}  Selection

main = htfMain htf_importedTests
