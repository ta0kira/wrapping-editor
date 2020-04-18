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
  ViewAction(..),
  ViewerAction,
  viewerFillAction,
  viewerResizeAction,
  viewerShiftUpAction,
  viewerShiftDownAction,
) where


-- | Generic editor viewport for fixed-width fonts.
--
--     * @v@: Viewer type providing the operations.
--     * @c@: Character type.
class FixedFontViewer v c | v -> c where
  -- | Set the @(width,height)@ size of the viewport. Setting either to @<=0@
  --   disables bounding in that dimension.
  setViewSize :: v -> (Int,Int) -> v
  -- | Get the @(width,height)@ size of the viewport.
  getViewSize :: v -> (Int,Int)
  -- | Get the visible lines in the viewport. This does not need to completely
  --   fill the viewport area, but it must not exceed it.
  getVisible :: v -> [[c]]
  -- | Apply a view change.
  updateView :: v -> ViewAction -> v

-- | Actions that modify the view without affecting editing.
data ViewAction =
  ShiftVertical Int | -- ^ Shift the vertical offset. Negative values shift up.
  FillView            -- ^ Attempt to fill the entire viewport.
    deriving (Eq,Show)

-- | Any action that updates a 'FixedFontViewer'.
type ViewerAction c = forall v. FixedFontViewer v c => v -> v

-- | Action to resize the viewport.
viewerResizeAction :: (Int,Int) -> ViewerAction c
viewerResizeAction s v = setViewSize v s

-- | Action to shift the view upward.
viewerShiftUpAction :: Int -> ViewerAction c
viewerShiftUpAction n v = updateView v (ShiftVertical (-n))

-- | Action to shift the view downward.
viewerShiftDownAction :: Int -> ViewerAction c
viewerShiftDownAction n v = updateView v (ShiftVertical n)

-- | Action to attempt to fill the viewport.
viewerFillAction :: ViewerAction c
viewerFillAction v = updateView v FillView
