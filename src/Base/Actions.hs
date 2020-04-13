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

-- | Descriptions for generic viewer and editor actions.

{-# LANGUAGE Safe #-}

module Base.Actions (
  EditAction(..),
  EditDirection(..),
  MoveDirection(..),
) where


-- | Actions that modify data.
data EditAction c =
  InsertText [c] | -- ^ Insert a block of characters.
  DeleteText       -- ^ Delete a single character.
    deriving (Eq,Show)

-- | Modification direction, relative to the cursor.
data EditDirection =
  EditBefore | -- ^ Apply the edit before the cursor.
  EditAfter    -- ^ Apply the edit after the cursor.
    deriving (Eq,Show)

-- | Actions that change the cursor position without changing data.
data MoveDirection =
  MoveUp |     -- ^ Move up one line.
  MoveDown |   -- ^ Move down one line.
  MovePrev |   -- ^ Move backward one character.
  MoveNext |   -- ^ Move forward one character.
  MoveHome |   -- ^ Implementation-defined home operation.
  MoveEnd |    -- ^ Implementation-defined end operation.
  MovePageUp | -- ^ Implementation-defined page-up operation.
  MovePageDown -- ^ Implementation-defined page-down operation.
    deriving (Eq,Show)
