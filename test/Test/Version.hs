module Test.Version (versionTests) where

import qualified Data.Text as T
import Data.Version (makeVersion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Version (
    TagStyle (..),
    newerVersionWarning,
    parseTagLine,
    scripthsVersion,
    scripthsVersionText,
    stampVersion,
    tagStyleFor,
    tagVersion,
    versionTag,
 )

versionTests :: TestTree
versionTests =
    testGroup
        "Version"
        [ tagStyleForTests
        , versionTagTests
        , parseTagLineTests
        , tagVersionTests
        , stampVersionTests
        , newerVersionWarningTests
        ]

tagStyleForTests :: TestTree
tagStyleForTests =
    testGroup
        "tagStyleFor"
        [ testCase "md/markdown => NotebookTag (case-insensitive)" $ do
            tagStyleFor "x.md" @?= NotebookTag
            tagStyleFor "x.markdown" @?= NotebookTag
            tagStyleFor "X.MD" @?= NotebookTag
        , testCase "ghci/hs/other => ScriptTag" $ do
            tagStyleFor "x.ghci" @?= ScriptTag
            tagStyleFor "x.hs" @?= ScriptTag
            tagStyleFor "noext" @?= ScriptTag
        ]

versionTagTests :: TestTree
versionTagTests =
    testGroup
        "versionTag"
        [ testCase "script form" $
            versionTag ScriptTag @?= "-- scripths: " <> scripthsVersionText
        , testCase "notebook form" $
            versionTag NotebookTag @?= "<!-- scripths: " <> scripthsVersionText <> " -->"
        ]

parseTagLineTests :: TestTree
parseTagLineTests =
    testGroup
        "parseTagLine"
        [ testCase "script tag" $
            parseTagLine ScriptTag "-- scripths: 0.4.1.0"
                @?= Just (makeVersion [0, 4, 0, 1])
        , testCase "notebook tag" $
            parseTagLine NotebookTag "<!-- scripths: 0.4.1.0 -->"
                @?= Just (makeVersion [0, 4, 0, 1])
        , testCase "tolerates trailing CRLF and whitespace" $ do
            parseTagLine NotebookTag "<!-- scripths: 1.2.3 -->\r"
                @?= Just (makeVersion [1, 2, 3])
            parseTagLine ScriptTag "-- scripths: 1.2.3   " @?= Just (makeVersion [1, 2, 3])
        , testCase "tolerates a leading BOM and up to 3 leading spaces" $ do
            parseTagLine NotebookTag "\65279<!-- scripths: 1.0 -->"
                @?= Just (makeVersion [1, 0])
            parseTagLine NotebookTag "   <!-- scripths: 1.0 -->"
                @?= Just (makeVersion [1, 0])
        , testCase "4+ leading spaces is an indented code block, not a tag" $
            parseTagLine NotebookTag "    <!-- scripths: 1.0 -->" @?= Nothing
        , testCase "scripths:mime output marker is not a version tag" $
            parseTagLine NotebookTag "<!-- scripths:mime text/plain -->" @?= Nothing
        , testCase "ordinary lines are not tags" $ do
            parseTagLine ScriptTag "import Data.Text" @?= Nothing
            parseTagLine NotebookTag "# Title" @?= Nothing
        ]

tagVersionTests :: TestTree
tagVersionTests =
    testGroup
        "tagVersion"
        [ testCase "reads the first line" $
            tagVersion NotebookTag "<!-- scripths: 2.0 -->\n# Title\n"
                @?= Just (makeVersion [2, 0])
        , testCase "skips a leading shebang in a script" $
            tagVersion ScriptTag "#!/usr/bin/env scripths\n-- scripths: 2.0\nfoo\n"
                @?= Just (makeVersion [2, 0])
        , testCase "skips a leading YAML frontmatter block in a notebook" $
            tagVersion
                NotebookTag
                "---\ntitle: x\n---\n<!-- scripths: 2.0 -->\n# Title\n"
                @?= Just (makeVersion [2, 0])
        , testCase "no tag => Nothing" $
            tagVersion NotebookTag "# Title\n\nprose\n" @?= Nothing
        ]

stampVersionTests :: TestTree
stampVersionTests =
    testGroup
        "stampVersion"
        [ testCase "prepends when missing (script)" $ do
            let out = stampVersion ScriptTag "import X\n"
            tagVersion ScriptTag out @?= Just scripthsVersion
            assertBool "keeps body" ("import X" `T.isInfixOf` out)
        , testCase "prepends when missing (notebook)" $ do
            let out = stampVersion NotebookTag "# Title\n"
            tagVersion NotebookTag out @?= Just scripthsVersion
            assertBool "keeps body" ("# Title" `T.isInfixOf` out)
        , testCase "idempotent: stamp . stamp == stamp" $ do
            let once = stampVersion NotebookTag "# Title\n\nprose\n"
                twice = stampVersion NotebookTag once
            twice @?= once
        , testCase "refreshes an old tag in place without stacking" $ do
            let out = stampVersion NotebookTag "<!-- scripths: 0.0.0.1 -->\n\n# Title\n"
            tagVersion NotebookTag out @?= Just scripthsVersion
            assertBool "old version gone" (not ("0.0.0.1" `T.isInfixOf` out))
            assertBool "keeps body" ("# Title" `T.isInfixOf` out)
            assertBool "single tag" (T.count "scripths:" out == 1)
        , testCase "inserts after a YAML frontmatter block" $ do
            let src = "---\ntitle: x\n---\n# Body\n"
                out = stampVersion NotebookTag src
            tagVersion NotebookTag out @?= Just scripthsVersion
            assertBool "frontmatter preserved" ("title: x" `T.isInfixOf` out)
            assertBool "tag sits after the closing ---" $
                T.isInfixOf "---\n" out
                    && maybe False (T.isInfixOf "<!-- scripths:") (lastFrontmatterTail out)
        , testCase "frontmatter stamping is idempotent" $ do
            let once = stampVersion NotebookTag "---\ntitle: x\n---\n# Body\n"
            stampVersion NotebookTag once @?= once
        ]
  where
    -- Text after the second "---" line, where the tag must live.
    lastFrontmatterTail t = case T.splitOn "---" t of
        (_ : _ : tl : _) -> Just tl
        _ -> Nothing

newerVersionWarningTests :: TestTree
newerVersionWarningTests =
    testGroup
        "newerVersionWarning"
        [ testCase "newer => warns" $
            assertBool
                "warns"
                (maybe False (const True) (newerVersionWarning (makeVersion [99, 0])))
        , testCase "current => silent" $
            newerVersionWarning scripthsVersion @?= Nothing
        , testCase "older => silent" $
            newerVersionWarning (makeVersion [0]) @?= Nothing
        ]
