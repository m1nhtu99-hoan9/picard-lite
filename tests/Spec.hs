{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative            ( Alternative(empty)
                                                , optional
                                                )
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Trans            ( lift )
import qualified Control.Monad.Yoctoparsec.Class
                                               as Yocto
import qualified Data.Attoparsec.Text          as Atto
                                                ( parseOnly )
import qualified Data.Text                     as Text
import          PicardLite.Language.SQL.SpiderSQL.Academic
                                                ( academicLexerTests
                                                , academicParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.AssetsMaintenance
                                                ( assetsMaintenanceLexerTests
                                                , assetsMaintenanceParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Bike1
                                                ( bike1LexerTests
                                                , bike1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Car1
                                                ( car1LexerTests
                                                , car1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Chinook1
                                                ( chinook1LexerTests
                                                , chinook1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.ConcertSinger
                                                ( concertSingerLexerTests
                                                , concertSingerParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.DepartmentManagement
                                                ( departmentManagementLexerTests
                                                , departmentManagementParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Flight1
                                                ( flight1LexerTests
                                                , flight1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Geo
                                                ( geoLexerTests
                                                , geoParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Inn1
                                                ( inn1LexerTests
                                                , inn1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Lexer
                                                ( lexSpiderSQL )
import          PicardLite.Language.SQL.SpiderSQL.MatchSeason
                                                ( matchSeasonLexerTests
                                                , matchSeasonParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Parse
                                                ( ParserEnv(..)
                                                , ParserEnvWithGuards(..)
                                                , mkParserStateTC
                                                , mkParserStateUD
                                                , spiderSQL
                                                , withGuards
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Pets1
                                                ( pets1LexerTests
                                                , pets1ParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.PhoneMarket
                                                ( phoneMarketLexerTests
                                                , phoneMarketParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.ProductCatalog
                                                ( productCatalogLexerTests
                                                , productCatalogParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Scholar
                                                ( scholarLexerTests
                                                , scholarParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.StormRecord
                                                ( stormRecordLexerTests
                                                , stormRecordParserTests
                                                )
import          PicardLite.Language.SQL.SpiderSQL.Syntax
                                                ( SX(..) )
import          PicardLite.Language.SQL.SpiderSQL.TestItem
                                                ( TestItem(..) )
import qualified Test.Tasty                    as T
import qualified Test.Tasty.HUnit              as H
import           Text.Parser.Char               ( CharParsing(..)
                                                , spaces
                                                )
import           Text.Parser.Combinators        ( Parsing(..) )
import qualified Text.Trifecta.Parser          as Trifecta
import qualified Text.Trifecta.Result          as Trifecta

{- Copyright (c) 2021, ServiceNow   
 
   Permission to use, copy, modify, and/or distribute this software for  
   any purpose with or without fee is hereby granted, provided that the  
   above copyright notice and this permission notice appear in all copies.  
    
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL  
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED  
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR  
   BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES  
   OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,  
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,  
   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS  
   SOFTWARE.  
-}

-- | Run 'cabal repl test:spec' to get a REPL for the tests.
main :: IO ()
main = T.defaultMain testTree

testData :: TestItem
testData = Group
  "tests"
  [ Group
    "lexer"
    [ academicLexerTests
    , assetsMaintenanceLexerTests
    , bike1LexerTests
    , car1LexerTests
    , chinook1LexerTests
    , concertSingerLexerTests
    , departmentManagementLexerTests
    , flight1LexerTests
    , geoLexerTests
    , inn1LexerTests
    , matchSeasonLexerTests
    , pets1LexerTests
    , phoneMarketLexerTests
    , productCatalogLexerTests
    , scholarLexerTests
    , stormRecordLexerTests
    ]
  , Group
    "parser"
    [ academicParserTests
    , assetsMaintenanceParserTests
    , bike1ParserTests
    , car1ParserTests
    , chinook1ParserTests
    , concertSingerParserTests
    , departmentManagementParserTests
    , flight1ParserTests
    , geoParserTests
    , inn1ParserTests
    , matchSeasonParserTests
    , pets1ParserTests
    , phoneMarketParserTests
    , productCatalogParserTests
    , scholarParserTests
    , stormRecordParserTests
    ]
  ]

testTree :: T.TestTree
testTree = toTest testData
 where
  withEnv parserEnv p =
    runReaderT (p <* optional (lift $ spaces <* char ';') <* lift eof) parserEnv
  attoParseOnly = Atto.parseOnly
  trifectaParseOnly p query = Trifecta.parseString p mempty (Text.unpack query)
  yoctoParseOnly p query =
    foldMap @[]
        (\case
          (Yocto.Done a []) -> pure a
          (Yocto.Done _ _ ) -> empty
          (Yocto.Partial _) -> empty
        )
      $ do
          p' <- Yocto.runParser p
          Yocto.feedOnly p' (Text.unpack query)
  toTest (Group name tests) = T.testGroup name $ toTest <$> tests
  toTest (LexQueryExpr sqlSchema query) =
    H.testCase ("Lex " <> show query)
      $ let p = withEnv sqlSchema lexSpiderSQL
        in  case attoParseOnly p query of
              Left  e -> H.assertFailure e
              Right _ -> pure ()
  toTest (ParseQueryExprWithoutGuards sqlSchema query) =
    H.testCase ("Parse without guards " <> show query)
      $ let p = withEnv
              (ParserEnv (ParserEnvWithGuards (const id)) sqlSchema)
              (spiderSQL SUD mkParserStateUD)
        in  case attoParseOnly p query of
              Left  e -> H.assertFailure e
              Right _ -> pure ()
  toTest (ParseQueryExprWithGuards sqlSchema query) =
    H.testCase ("Parse with guards " <> show query)
      $ let p = withEnv
              (ParserEnv (ParserEnvWithGuards (withGuards SUD)) sqlSchema)
              (spiderSQL SUD mkParserStateUD)
        in  -- case yoctoParseOnly p query of
            --   _ : _ -> pure ()
            --   [] -> H.assertFailure "empty"
            -- case attoParseOnly p query of
            --   Left e -> H.assertFailure e
            --   Right _ -> pure ()
            case trifectaParseOnly p query of
              Trifecta.Failure Trifecta.ErrInfo {..} ->
                H.assertFailure (show _errDoc)
              Trifecta.Success _ -> pure ()
  toTest (ParseQueryExprWithGuardsAndTypeChecking sqlSchema query) =
    H.testCase ("Parse and type check " <> show query)
      $ let p = withEnv
              (ParserEnv (ParserEnvWithGuards (withGuards STC)) sqlSchema)
              (spiderSQL STC mkParserStateTC)
        in  case yoctoParseOnly p query of
              _ : _ -> pure ()
              []    -> H.assertFailure "empty"
  toTest (ParseQueryExprFails sqlSchema query) =
    H.testCase ("Fail " <> show query)
      $ let p = withEnv
              (ParserEnv (ParserEnvWithGuards (withGuards SUD)) sqlSchema)
              (spiderSQL SUD mkParserStateUD)
        in  case attoParseOnly p query of
              Left  _ -> pure ()
              Right a -> H.assertFailure $ show a
  toTest (ParseQueryExprFailsTypeChecking sqlSchema query) =
    H.testCase ("Type checking fail " <> show query)
      $ let p = withEnv
              (ParserEnv (ParserEnvWithGuards (withGuards STC)) sqlSchema)
              (spiderSQL STC mkParserStateTC)
        in  case yoctoParseOnly p query of
              a : _ -> H.assertFailure $ show a
              []    -> pure ()
