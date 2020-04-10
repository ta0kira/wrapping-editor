{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Base.Viewer (
  FixedFontViewer(..),
  ViewerAction,
  viewerResizeAction,
) where


class FixedFontViewer a c | a -> c where
  setViewSize :: a -> (Int,Int) -> a
  getViewSize :: a -> (Int,Int)
  getVisible :: a -> [[c]]

type ViewerAction c = forall a. FixedFontViewer a c => a -> a

viewerResizeAction :: (Int,Int) -> ViewerAction c
viewerResizeAction s v = setViewSize v s
