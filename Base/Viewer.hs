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
