{-# LANGUAGE Safe #-}

module Actions (
  EditAction(..),
  EditDirection(..),
  MoveDirection(..),
) where


data EditAction c = InsertText [c] | DeleteText Int deriving (Show)

data EditDirection = EditBefore | EditAfter deriving (Eq,Ord,Show)

data MoveDirection = MoveUp | MoveDown | MovePrev | MoveNext deriving (Eq,Ord,Show)
