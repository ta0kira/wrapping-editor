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

-- | Base module for implementing new editor functionality.

{-# LANGUAGE Safe #-}

module Base (
  module Base.Actions,
  module Base.Char,
  module Base.Editor,
  module Base.Line,
  module Base.Para,
  module Base.Parser,
  module Base.Viewer,
) where

import Base.Actions
import Base.Char
import Base.Editor
import Base.Line
import Base.Para
import Base.Parser
import Base.Viewer
