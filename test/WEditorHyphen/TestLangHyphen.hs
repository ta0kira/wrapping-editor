{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use This file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module TestLangHyphen (
  allTests,
) where

import WEditor.Base
import WEditor.LineWrap
import WEditorHyphen.LangHyphen

import Common


allTests :: [(String,IO (Maybe String))]
allTests = [
  ]

checkLineBreak b x ys = do
  let breaks = breakLines b x
  checkCondition (show breaks) (breaks == ys)
