{-# LANGUAGE Safe #-}

module Base.Actions (
  EditAction(..),
  EditDirection(..),
  MoveDirection(..),
) where


data EditAction c = InsertText [c] | DeleteText deriving (Show)

data EditDirection = EditBefore | EditAfter deriving (Eq,Ord,Show)

data MoveDirection = MoveUp | MoveDown | MovePrev | MoveNext deriving (Eq,Ord,Show)