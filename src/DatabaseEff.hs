{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module DatabaseEff where

import Polysemy

data DatabaseEff m a where
  All :: DatabaseEff m [a]
  Get :: Int -> DatabaseEff m a
  Post :: a -> DatabaseEff m a
  Put :: Int -> a -> DatabaseEff m a
  Delete :: Int -> DatabaseEff m a

makeSem ''DatabaseEff

databaseEffToIO :: Sem (DatabaseEff ': r) a -> Sem r a
databaseEffToIO = interpret (\case
  All -> return []
                            )
