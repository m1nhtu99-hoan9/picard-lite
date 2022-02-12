{-# LANGUAGE OverloadedStrings #-}

module PicardLite.Language.SQL.SpiderSQL.MatchSeason where

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text                     as Text
                                                ( Text )
import           PicardLite.Language.SQL.SpiderSQL.TestItem
                                                ( TestItem(..) )
import           PicardLite.Types               ( ColumnType(..)
                                                , SQLSchema(..)
                                                )

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

matchSeasonSchema :: SQLSchema
matchSeasonSchema =
  let
    columnNames = HashMap.fromList
      [ ("1" , "Country_id")
      , ("10", "Country")
      , ("11", "Team")
      , ("12", "Draft_Pick_Number")
      , ("13", "Draft_Class")
      , ("14", "College")
      , ("15", "Player_ID")
      , ("16", "Player")
      , ("17", "Years_Played")
      , ("18", "Total_WL")
      , ("19", "Singles_WL")
      , ("2" , "Country_name")
      , ("20", "Doubles_WL")
      , ("21", "Team")
      , ("3" , "Capital")
      , ("4" , "Official_native_language")
      , ("5" , "Team_id")
      , ("6" , "Name")
      , ("7" , "Season")
      , ("8" , "Player")
      , ("9" , "Position")
      ]
    columnTypes = HashMap.fromList
      [ ("1" , ColumnType_NUMBER)
      , ("10", ColumnType_NUMBER)
      , ("11", ColumnType_NUMBER)
      , ("12", ColumnType_NUMBER)
      , ("13", ColumnType_TEXT)
      , ("14", ColumnType_TEXT)
      , ("15", ColumnType_NUMBER)
      , ("16", ColumnType_TEXT)
      , ("17", ColumnType_TEXT)
      , ("18", ColumnType_TEXT)
      , ("19", ColumnType_TEXT)
      , ("2" , ColumnType_TEXT)
      , ("20", ColumnType_TEXT)
      , ("21", ColumnType_NUMBER)
      , ("3" , ColumnType_TEXT)
      , ("4" , ColumnType_TEXT)
      , ("5" , ColumnType_NUMBER)
      , ("6" , ColumnType_TEXT)
      , ("7" , ColumnType_NUMBER)
      , ("8" , ColumnType_TEXT)
      , ("9" , ColumnType_TEXT)
      ]
    tableNames = HashMap.fromList
      [("0", "country"), ("1", "team"), ("2", "match_season"), ("3", "player")]
    columnToTable = HashMap.fromList
      [ ("1" , "0")
      , ("10", "2")
      , ("11", "2")
      , ("12", "2")
      , ("13", "2")
      , ("14", "2")
      , ("15", "3")
      , ("16", "3")
      , ("17", "3")
      , ("18", "3")
      , ("19", "3")
      , ("2" , "0")
      , ("20", "3")
      , ("21", "3")
      , ("3" , "0")
      , ("4" , "0")
      , ("5" , "1")
      , ("6" , "1")
      , ("7" , "2")
      , ("8" , "2")
      , ("9" , "2")
      ]
    tableToColumns = HashMap.fromList
      [ ("0", ["1", "2", "3", "4"])
      , ("1", ["5", "6"])
      , ("2", ["7", "8", "9", "10", "11", "12", "13", "14"])
      , ("3", ["15", "16", "17", "18", "19", "20", "21"])
      ]
    foreignKeys = HashMap.fromList [("10", "1"), ("11", "5"), ("21", "5")]
    primaryKeys = ["1", "5", "7", "15"]
  in
    SQLSchema { sqlSchema_columnNames    = columnNames
              , sqlSchema_columnTypes    = columnTypes
              , sqlSchema_tableNames     = tableNames
              , sqlSchema_columnToTable  = columnToTable
              , sqlSchema_tableToColumns = tableToColumns
              , sqlSchema_foreignKeys    = foreignKeys
              , sqlSchema_primaryKeys    = primaryKeys
              }

matchSeasonQueries :: [Text.Text]
matchSeasonQueries =
  [ "select college from match_season group by college having count(*) >= 2 order by college desc"
  ]

matchSeasonQueriesFails :: [Text.Text]
matchSeasonQueriesFails = []

matchSeasonParserTests :: TestItem
matchSeasonParserTests =
  Group "matchSeason"
    $  (   ParseQueryExprWithGuardsAndTypeChecking matchSeasonSchema
       <$> matchSeasonQueries
       )
    <> (ParseQueryExprWithGuards matchSeasonSchema <$> matchSeasonQueries)
    <> (ParseQueryExprWithoutGuards matchSeasonSchema <$> matchSeasonQueries)
    <> (ParseQueryExprFails matchSeasonSchema <$> matchSeasonQueriesFails)

matchSeasonLexerTests :: TestItem
matchSeasonLexerTests =
  Group "matchSeason" $ LexQueryExpr matchSeasonSchema <$> matchSeasonQueries
