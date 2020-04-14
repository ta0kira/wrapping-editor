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

-- | Generic editor-viewport functionality.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module WEditor.Base.Viewer (
  FixedFontViewer(..),
  ViewerAction,
  viewerResizeAction,
) where


-- | Generic editor viewport for fixed-width fonts.
class FixedFontViewer a c | a -> c where
  -- | Sets the (width,height) size of the viewport. A width < 0 must disable
  --   line wrapping, and a height < 0 must disable vertical bounding.
  setViewSize :: a -> (Int,Int) -> a
  -- | Gets the (width,height) size of the viewport.
  getViewSize :: a -> (Int,Int)
  -- | Gets the visible lines in the viewport. This does not need to completely
  --   fill the viewport area, but it must not exceed it.
  getVisible :: a -> [[c]]

-- | Any action that updates a 'FixedFontViewer'.
type ViewerAction c = forall a. FixedFontViewer a c => a -> a

-- | Action to resize the viewport.
viewerResizeAction :: (Int,Int) -> ViewerAction c
viewerResizeAction s v = setViewSize v s
