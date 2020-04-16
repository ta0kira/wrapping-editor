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

{-# LANGUAGE Safe #-}

module TestPara (
  allTests,
) where

import WEditor.LineWrap
import WEditor.Base
import WEditor.Internal.Para

import Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("edit preserves paragraph", checkParaEdit
       commonParser
       examplePara
       examplePara
       id),
    ("edit starts at top", checkParaStructure
       commonParser
       examplePara
       ([],
        ("This is a test ",0,0),
        ["paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."])
       id),
    ("move cursor down", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related"],
        (" things work as",0,60),
        [" they are suppo",
         "sed to."]) $
        repeatAction 4 (moveParaCursor MoveDown)),
    ("move cursor below bottom", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo"],
        ("sed to.",7,97),
        []) $
        repeatAction 20 (moveParaCursor MoveDown)),
    ("move cursor above top", checkParaStructure
       commonParser
       examplePara
       ([],
        ("This is a test ",0,0),
        ["paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."]) $
        repeatAction 5 (moveParaCursor MoveUp)),
    ("move up at top sets to paragraph front", checkParaStructure
       commonParser
       examplePara
       ([],
        ("XYZThis is a te",3,3),
        ["st paragraph to",
         " make sure that",
         " paragraph-rela",
         "ted things work",
         " as they are su",
         "pposed to."]) $
       composeActions [
           repeatAction 5 (moveParaCursor MoveNext),
           moveParaCursor MoveUp,
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("move down at bottom sets to paragraph back", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo"],
        ("sed to.XYZ",10,100),
        []) $
       composeActions [
           repeatAction 7 (moveParaCursor MoveDown),
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("continue to next line", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma"],
        ("ke sure that pa",0,30),
        ["ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."]) $
        repeatAction 32 (moveParaCursor MoveNext)),
    ("continue to previous line", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma"],
        ("ke sure that pa",15,45),
        ["ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."]) $
       composeActions [
           repeatAction 5 (moveParaCursor MoveDown),
           repeatAction 33 (moveParaCursor MovePrev)
         ]),
    ("insert at front", checkParaStructure
       commonParser
       examplePara
       ([],
        ("XYZThis is a te",3,3),
        ["st paragraph to",
         " make sure that",
         " paragraph-rela",
         "ted things work",
         " as they are su",
         "pposed to."]) $
       composeActions [
           repeatAction 5 (moveParaCursor MoveDown),
           repeatAction 5 (moveParaCursor MoveNext),
           seekParaFront,
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("insert at back", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo"],
        ("sed to.XYZ",10,100),
        []) $
       composeActions [
           repeatAction 5 (moveParaCursor MoveDown),
           repeatAction 5 (moveParaCursor MoveNext),
           seekParaBack,
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("insert in middle", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related"],
        (" thiXYZngs work",7,67),
        [" as they are su",
         "pposed to."]) $
       composeActions [
           repeatAction 4 (moveParaCursor MoveDown),
           repeatAction 4 (moveParaCursor MoveNext),
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("delete after front", checkParaStructure
       commonParser
       examplePara
       ([],
        ("his is a test p",0,0),
        ["aragraph to mak",
         "e sure that par",
         "agraph-related ",
         "things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           seekParaFront,
           modifyPara commonParser DeleteText EditAfter
         ]),
    ("delete before front", checkParaStructure
       commonParser
       examplePara
       ([],
        ("This is a test ",0,0),
        ["paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."]) $
       composeActions [
           seekParaFront,
           modifyPara commonParser DeleteText EditBefore
         ]),
    ("delete before back", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo"],
        ("sed to",6,96),
        []) $
       composeActions [
           seekParaBack,
           modifyPara commonParser DeleteText EditBefore
         ]),
    ("delete after back", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo"],
        ("sed to.",7,97),
        []) $
       composeActions [
           seekParaBack,
           modifyPara commonParser DeleteText EditAfter
         ]),
    ("delete before line front", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa"],
        ("ragraph-relate ",14,59),
        ["things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           repeatAction 4 (moveParaCursor MoveDown),
           modifyPara commonParser DeleteText EditBefore
         ]),
    ("delete after line back", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa"],
        ("ragraph-related",15,60),
        ["things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           repeatAction 4 (moveParaCursor MoveDown),
           moveParaCursor MovePrev,
           modifyPara commonParser DeleteText EditAfter
         ]),
    ("split paragraph middle", do
       let para = repeatAction 3 (moveParaCursor MoveDown) $
                  repeatAction 4 (moveParaCursor MoveNext) $
                  editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           ("Top: " ++ show (upText top),
            upText top == "This is a test paragraph to make sure that paragr"),
           ("Bottom: " ++ show (upText bottom),
            upText bottom == "aph-related things work as they are supposed to.")
         ]),
    ("split paragraph front", do
       let para = seekParaFront $ editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           ("Top: " ++ show (upText top),
            upText top == ""),
           ("Bottom: " ++ show (upText bottom),
            upText bottom == "This is a test paragraph to make sure that paragraph-related things work as they are supposed to.")
         ]),
    ("split paragraph back", do
       let para = seekParaBack $ editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           ("Top: " ++ show (upText top),
             upText top == "This is a test paragraph to make sure that paragraph-related things work as they are supposed to."),
           ("Bottom: " ++ show (upText bottom),
            upText bottom == "")
         ]),
    ("prepend preserves cursor", checkParaStructure
       commonParser
       examplePara
       (["This is some ex",
         "tra stuff. This",
         " is a test para",
         "graph to make s",
         "ure that paragr"],
        ("aphXYZ-related ",6,81),
        ["things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           repeatAction 3 (moveParaCursor MoveDown),
           repeatAction 7 (moveParaCursor MoveNext),
           prependToPara commonParser (parseParaBefore commonParser $ UnparsedPara "This is some extra stuff. "),
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ]),
    ("append preserves cursor", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa"],
        ("ragraphXYZ-rela",10,55),
        ["ted things work",
          " as they are su",
          "pposed to. This",
          " is some extra ",
          "stuff."]) $
       composeActions [
           repeatAction 3 (moveParaCursor MoveDown),
           repeatAction 7 (moveParaCursor MoveNext),
           flip (appendToPara commonParser) (parseParaAfter commonParser $ UnparsedPara " This is some extra stuff."),
           modifyPara commonParser (InsertText "XYZ") EditBefore
         ])
  ]

commonParser :: BreakWords Char
commonParser = setLineWidth breakExact 15

examplePara = UnparsedPara "This is a test paragraph to make sure that paragraph-related things work as they are supposed to."

checkParaEdit p x y f = do
  let edit = f $ editPara p x
  let restored = unparsePara edit
  checkCondition (show restored) (restored == y)

checkParaStructure p x (yb,(yl,c,e),ya) f = do
  let edit = f $ editPara p x
  let before = map vlText $ viewBeforeLines $ getBeforeLines edit
  let line = vlText $ getCurrentLine edit
  let after = map vlText $ viewAfterLines $ getAfterLines edit
  let cursor = getParaCursorChar edit
  let point = getParaEditChar edit
  checkConditions [
      ("Before: " ++ show before,before == yb),
      ("Line: "   ++ show line,  line == yl),
      ("After: "  ++ show after, after == ya),
      ("Cursor: " ++ show cursor,cursor == c),
      ("Edit: "   ++ show point, point == e)
    ]
