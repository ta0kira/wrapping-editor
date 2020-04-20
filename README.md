# [Wrapping Editor for Haskell][home]

This library contains a simple text-editing component for fixed-size fonts. It
can either be used as a [Brick][brick] `Widget` or as the basis for creating a
new widget for another UI library.

Here are some important features:

- Supports dynamic viewport resizing while preserving the view position.
- Customizable line-wrapping policies, to include special line rendering.
- Includes a line-wrapping policy for word hyphenation.
- Efficient editing operations for long documents.

If you have any problems using this library, or if you would like to see new
features, please see the [issues page][issues]. Also check out the
[library reference][library-doc].

## Installation

This library is split into 3 separate packages:

- **`WEditor`** is the base package, containing the generic editor logic and
  simple line-wrapping and hyphenation policies. This can be used as the basis
  for a new editor widget for a UI library without depending on any of the
  other packages.

- **`WEditorHyphen`** contains language-specific hyphenation policies for use
  with both `WEditor` and `WEditorBrick`. This package is *not* required in
  order to use either of those packages. You will need to explicitly install the
  `hyphenation` package in order to have access to the rules for each supported
  language.

- **`WEditorBrick`** contains just the [Brick][brick] `Widget`.

All of these packages can be installed from [Hackage][hackage] using
[`cabal`][cabal]. You will need to explicitly install `WEditor` in order to set
up editors in either of the other packages.

- Just the base package:

  ```shell
  # cabal < 2.0
  cabal install WEditor

  # cabal >= 2.0
  cabal install --lib WEditor
  ```

- Everything:

  ```shell
  # cabal < 2.0
  cabal install WEditor WEditorBrick hyphenation WEditorHyphen

  # cabal >= 2.0
  cabal install --lib WEditor WEditorBrick hyphenation WEditorHyphen
  ```

## Using with [Brick][brick]

See [brick-example.hs][brick-example.hs] for an example program that uses
[`WrappingEditor`][WrappingEditor].

You can run the example with:

```shell
ghc -threaded example/WEditorBrick/brick-example.hs
example/WEditorBrick/brick-example README.md
```

Press `Esc` to exit when you are finished. The final contents of the editor will
be sent to `stdout` without modifying the file.

You can customize the widget using the following helper functions from the
`WrappingEditor` module:

- [**`doEditor`**][doEditor] allows you to use `FixedFontEditor` and
  `FixedFontViewer` functions to extract info from the editor, e.g., for custom
  rendering.

- [**`genericEditor`**][genericEditor] allows you to use any custom editor
  component that instantiates both `FixedFontEditor` and `FixedFontViewer`,
  e.g., an editor for a custom character type. (You might also need custom
  rendering and event handling.)

- [**`mapEditor`**][mapEditor] allows you to transform the editor with
  `FixedFontEditor` and `FixedFontViewer` functions, e.g., calling editing
  actions in a custom event handler. (If you use a custom event handler, call
  [`updateEditorExtent`][updateEditorExtent] *before* applying editor actions.)

## Using the Generic Editor

```haskell
import WEditor.LineWrap
import WEditor.Document

-- 1. Split your doc into paragraphs. Paragraph splitting is handled by the
--    caller so that the library can avoid end-of-line considerations.
paragraphs = map UnparsedPara $ lines $ "Your document contents."

-- 2. Create an editor. This example uses lazy word hyphenation.
editor = editDocument (breakWords lazyHyphen) paragraphs

-- 3a. Edit the document using actions from `Viewer` and `Editor`. Don't forget
--     to set the viewport size! If either dimension is < 1, the text will be
--     unbounded in that direction.
editor' = foldl (flip ($)) editor [
    viewerResizeAction (80,24),
    editorEndAction,
    editorEnterAction,
    editorAppendAction "Here is a new paragraph.",
    -- Resizing while editing is fine.
    viewerResizeAction (7,3),
    -- Use this if you don't like scrolling below the last line.
    viewerFillAction
  ]

-- 3b. Get the viewport contents for display. This does not necessarily fill up
--     the entire view area; pad it if necessary.
display = getVisible editor'

-- 4. Extract the edited contents.
final = unlines $ map upText $ exportData editor'
```

## Wrapping Policies

- [**`breakExact`**][breakExact] works for all character types because it breaks
  lines at exactly the editor width.

- [**`breakWords p`**][breakWords] takes a [`WordSplitter`][WordSplitter] policy
  `p` to split words. Character support depends on the existence of a
  [`WordSplitter`][WordSplitter] for the character type. In theory, it supports
  all character types.

  - [**`noHyphen`**][noHyphen] avoids splitting words if at all possible, and it
    never uses hyphens. This policy requires a [`WordChar`][WordChar] instance
    for the character type.

  - [**`lazyHyphen`**][lazyHyphen] splits words without any dictionary
    awareness, and attempts to keep at least 2 characters of the word on each
    line. This policy requires [`WordChar`][WordChar] and
    [`HyphenChar`][HyphenChar] instances for the character type.

  - Create custom word-splitting by creating a new
    [`WordSplitter`][WordSplitter]. This can be used for supporting new
    character types, adding dictionary awareness, expanding the characters that
    are considered to be a part of words, etc.

- Create a completely new wrapping policy with an instance of
  [`FixedFontParser`][FixedFontParser]. This can be used for things that
  `breakWords` cannot support, e.g., line indentation and tab expansion.

## Character Support

The generic editor can support any character type for which a wrapping policy is
available. The editor for [Brick][brick] currently only supports `Char`, but it
will likely be modified to support other character types.

[brick]: https://github.com/jtdaugherty/brick
[brick-example.hs]: https://github.com/ta0kira/wrapping-editor/blob/master/example/brick-example.hs
[cabal]: https://www.haskell.org/cabal/#install-upgrade
[ghc]: https://www.haskell.org/ghc/
[hackage]: http://hackage.haskell.org
[hackage-WEditor]: http://hackage.haskell.org/package/WEditor
[hackage-WEditorBrick]: http://hackage.haskell.org/package/WEditorBrick
[home]: https://github.com/ta0kira/wrapping-editor
[issues]: https://github.com/ta0kira/wrapping-editor/issues
[library-doc]: https://ta0kira.github.io/wrapping-editor/library

[FixedFontParser]: https://ta0kira.github.io/wrapping-editor/library/WEditor-Base-Parser.html#t:FixedFontParser
[HyphenChar]: https://ta0kira.github.io/wrapping-editor/library/WEditor-Base-Char.html#t:HyphenChar
[WordChar]: https://ta0kira.github.io/wrapping-editor/library/WEditor-Base-Char.html#t:WordChar
[WordSplitter]: https://ta0kira.github.io/wrapping-editor/library/WEditor-LineWrap.html#t:WordSplitter
[WrappingEditor]: https://ta0kira.github.io/wrapping-editor/library/WEditorBrick-WrappingEditor.html#t:WrappingEditor
[breakExact]: https://ta0kira.github.io/wrapping-editor/library/WEditor-LineWrap.html#v:breakExact
[breakWords]: https://ta0kira.github.io/wrapping-editor/library/WEditor-LineWrap.html#v:breakWords
[doEditor]: https://ta0kira.github.io/wrapping-editor/library/WEditorBrick-WrappingEditor.html#v:doEditor
[genericEditor]: https://ta0kira.github.io/wrapping-editor/library/WEditorBrick-WrappingEditor.html#v:genericEditor
[lazyHyphen]:  https://ta0kira.github.io/wrapping-editor/library/WEditor-LineWrap.html#v:lazyHyphen
[mapEditor]: https://ta0kira.github.io/wrapping-editor/library/WEditorBrick-WrappingEditor.html#v:mapEditor
[noHyphen]: https://ta0kira.github.io/wrapping-editor/library/WEditor-LineWrap.html#v:noHyphen
[updateEditorExtent]: https://ta0kira.github.io/wrapping-editor/library/WEditorBrick-WrappingEditor.html#v:updateEditorExtent
