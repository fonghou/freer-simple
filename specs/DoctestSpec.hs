{-# LANGUAGE CPP #-}

module DoctestSpec where

import Test.Hspec
import Test.DocTest

spec :: Spec
spec = parallel $ describe "Error messages" $ it "should pass the doctest" $ doctest
  [ "-isrc/"
  , "--fast"
  , "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDeriveFunctor"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XLambdaCase"
  , "-XFunctionalDependencies"
  , "-XMultiParamTypeClasses"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeApplications"
  , "-XTypeFamilies"
  , "-XTypeOperators"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Control/Monad/Freer/Fail.hs"
  , "src/Control/Monad/Freer/NonDet.hs"
  , "src/Control/Monad/Freer/Resource.hs"
  , "examples/src/Coroutine.hs"
  ]
