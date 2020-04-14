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

module WEditor.Base (
-- | Descriptions for generic viewer and editor actions.
  module WEditor.Base.Actions,
  -- | Features of character sets.
  module WEditor.Base.Char,
  -- | Generic text-editing functionality.
  module WEditor.Base.Editor,
  -- | Simple representation of viewable text lines.
  module WEditor.Base.Line,
  -- | Simple representation of text paragraphs.
  module WEditor.Base.Para,
  -- | Generic line-parsing functionality.
  module WEditor.Base.Parser,
  -- | Generic editor-viewport functionality.
  module WEditor.Base.Viewer,
) where

import WEditor.Base.Actions
import WEditor.Base.Char
import WEditor.Base.Editor
import WEditor.Base.Line
import WEditor.Base.Para
import WEditor.Base.Parser
import WEditor.Base.Viewer