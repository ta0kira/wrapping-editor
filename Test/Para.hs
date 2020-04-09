{-# LANGUAGE Safe #-}

module Test.Para (
  allTests,
) where

import Base.Actions
import Base.Line
import Base.Para
import Base.Parser
import Edit.Para
import Test.Common


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
        "This is a test ",
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
        " things work as",
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
        "sed to.",
        []) $
        repeatAction 20 (moveParaCursor MoveDown)),
    ("move cursor above top", checkParaStructure
       commonParser
       examplePara
       ([],
        "This is a test ",
        ["paragraph to ma",
         "ke sure that pa",
         "ragraph-related",
         " things work as",
         " they are suppo",
         "sed to."]) $
        repeatAction 5 (moveParaCursor MoveUp)),
    ("continue to next line", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma"],
        "ke sure that pa",
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
        "ke sure that pa",
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
        "XYZThis is a te",
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
        "sed to.XYZ",
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
        " thiXYZngs work",
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
        "his is a test p",
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
        "This is a test ",
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
        "sed to",
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
        "sed to.",
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
        "ragraph-relate ",
        ["things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           repeatAction 4 (moveParaCursor MoveDown),
           modifyPara commonParser DeleteText EditBefore
         ]),
    ("delete after line end", checkParaStructure
       commonParser
       examplePara
       (["This is a test ",
         "paragraph to ma",
         "ke sure that pa"],
        "ragraph-relate ",
        ["things work as ",
         "they are suppos",
         "ed to."]) $
       composeActions [
           repeatAction 4 (moveParaCursor MoveDown),
           moveParaCursor MovePrev,
           modifyPara commonParser DeleteText EditBefore
         ]),
    ("split paragraph middle", do
       let para = repeatAction 3 (moveParaCursor MoveDown) $
                  repeatAction 4 (moveParaCursor MoveNext) $
                  editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           (upText top == "This is a test paragraph to make sure that paragr",
            "Top: " ++ show (upText top)),
           (upText bottom == "aph-related things work as they are supposed to.",
            "Bottom: " ++ show (upText bottom))
         ]),
    ("split paragraph front", do
       let para = seekParaFront $ editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           (upText top == "",
            "Top: " ++ show (upText top)),
           (upText bottom == "This is a test paragraph to make sure that paragraph-related things work as they are supposed to.",
            "Bottom: " ++ show (upText bottom))
         ]),
    ("split paragraph back", do
       let para = seekParaBack $ editPara commonParser examplePara
       let (top,bottom) = splitPara commonParser para
       checkConditions [
           (upText top == "This is a test paragraph to make sure that paragraph-related things work as they are supposed to.",
            "Top: " ++ show (upText top)),
           (upText bottom == "",
            "Bottom: " ++ show (upText bottom))
         ]),
    ("prepend preserves cursor", checkParaStructure
       commonParser
       examplePara
       (["This is some ex",
         "tra stuff. This",
         " is a test para",
         "graph to make s",
         "ure that paragr"],
        "aphXYZ-related ",
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
        "ragraphXYZ-rela",
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

commonParser = setLineWidth breakExact 15

examplePara = UnparsedPara "This is a test paragraph to make sure that paragraph-related things work as they are supposed to."

checkParaEdit p x y f = do
  let edit = f $ editPara p x
  let restored = unparsePara p edit
  checkCondition (restored == y) (show restored)

checkParaStructure p x (yb,yl,ya) f = do
  let edit = f $ editPara p x
  let before = map vlText $ viewBeforeLines $ getBeforeLines edit
  let line = vlText $ getCurrentLine edit
  let after = map vlText $ viewAfterLines $ getAfterLines edit
  checkConditions [
      (before == yb,"Before: " ++ show before),
      (line == yl,"Line: " ++ show line),
      (after == ya,"After: " ++ show after)
    ]
