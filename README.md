# [Wrapping Editor for Haskell][home]

This library contains a simple text-editing widget for fixed-size fonts. It can
either be used as a [Brick][brick] `Widget` or as the basis for creating a new
widget for another UI library.

Here are some important features:

- Supports dynamic viewport resizing while preserving the view position.
- Customizable line-wrapping policies, to include special line rendering.
- Efficient editing operations for long documents.

If you have any problems using this library, or if you would like to see new
features, please see the [issues page][issues].

## Installation

This library currently does not have any sort of release process. Please clone
the [repository on GitHub][home] for now.

Note that you __*do not*__ need to install [Brick][brick] to use the basic
editor component. The [Brick][brick] functionality is a thin layer on top of the
basic editor. The basic editor depends on [GHC][ghc] extensions, but it has no
dependencies otherwise.

You can run the unit tests with:

```shell
ghc test.hs && ./test
```

## Using with [Brick][brick]

See [brick-example.hs][brick-example.hs] for an example program that uses
`WrappingEditor`.

You can run the example with:

```shell
ghc -threaded brick-example.hs && ./brick-example README.md
```

Press `Esc` to exit when you are finished. The final contents of the editor will
be sent to `stdout` without modifying the file.

## Using the Basic Editor

```haskell
import LineWrap
import Document

-- 1. Split your doc into paragraphs. Paragraph splitting is handled by the
--    caller so that the library can avoid end-of-line considerations.
paragraphs = map UnparsedPara $ lines $ "Your document contents."

-- 2. Create an editor. This example uses the `breakExact` wrapping policy.
editor = editDocument breakExact paragraphs

-- 3a. Edit the document using actions from `Base.Viewer` and `Base.Editor`.
--     Don't forget to set the viewport size! When using `breakExact`, wrapping
--     can be disabled with a view width of 0.
editor' = foldl (flip ($)) editor [
    viewerResizeAction (80,24),
    editorDownAction,
    editorEnterAction,
    editorAppendAction "Here is a new paragraph.",
    -- Resizing while editing is fine.
    viewerResizeAction (7,3)
  ]

-- 3b. Get the viewport contents for display. This does not necessarily fill up
--     the entire view area; pad it if necessary.
display = getVisible editor'

-- 4. Extract the edited contents.
final = unlines $ map upText $ exportDocument editor'
```

## Included Wrapping Policies

- **`breakExact`** works for all character types because it breaks lines at
  exactly the editor width.

- **`trimSpaces`** hides spaces at the end of each line so that the leading
  edges of the paragraphs look cleaner. This has no effect on the data itself.
  This policy works for any character type with a `SpaceChar` `instance`, which
  identifies space characters.

Wrapping is disabled for included policies when the width is less than 1.

## Creating a Custom Wrapping Policy

Custom wrapping  policies can be created with a new `instance` of
`Base.FixedFontParser`.

If you want to create a policy that hyphenates words, you can implement
`renderLine` to conditionally append a hyphen for display purposes. This will
not affect how the cursor gets displayed.

## Character Support

The basic editor can support any character type for which a wrapping policy is
available. The editor for [Brick][brick] currently only supports `Char`, but it
will likely be modified to support other character types.

[brick]: https://github.com/jtdaugherty/brick
[brick-example.hs]: https://github.com/ta0kira/wrapping-editor/blob/master/brick-example.hs
[ghc]: https://www.haskell.org/ghc/
[home]: https://github.com/ta0kira/wrapping-editor
[issues]: https://github.com/ta0kira/wrapping-editor/issues
