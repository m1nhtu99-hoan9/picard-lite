{-# LANGUAGE OverloadedStrings #-}

module PicardLite.Language.SQL.SpiderSQL.StormRecord where

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

stormRecordSchema :: SQLSchema
stormRecordSchema =
  let
    columnNames = HashMap.fromList
      [ ("1" , "Storm_ID")
      , ("10", "Region_id")
      , ("11", "Storm_ID")
      , ("12", "Number_city_affected")
      , ("2" , "Name")
      , ("3" , "Dates_active")
      , ("4" , "Max_speed")
      , ("5" , "Damage_millions_USD")
      , ("6" , "Number_Deaths")
      , ("7" , "Region_id")
      , ("8" , "Region_code")
      , ("9" , "Region_name")
      ]
    columnTypes = HashMap.fromList
      [ ("1" , ColumnType_NUMBER)
      , ("10", ColumnType_NUMBER)
      , ("11", ColumnType_NUMBER)
      , ("12", ColumnType_NUMBER)
      , ("2" , ColumnType_TEXT)
      , ("3" , ColumnType_TEXT)
      , ("4" , ColumnType_NUMBER)
      , ("5" , ColumnType_NUMBER)
      , ("6" , ColumnType_NUMBER)
      , ("7" , ColumnType_NUMBER)
      , ("8" , ColumnType_TEXT)
      , ("9" , ColumnType_TEXT)
      ]
    tableNames = HashMap.fromList
      [("0", "storm"), ("1", "region"), ("2", "affected_region")]
    columnToTable = HashMap.fromList
      [ ("1" , "0")
      , ("10", "2")
      , ("11", "2")
      , ("12", "2")
      , ("2" , "0")
      , ("3" , "0")
      , ("4" , "0")
      , ("5" , "0")
      , ("6" , "0")
      , ("7" , "1")
      , ("8" , "1")
      , ("9" , "1")
      ]
    tableToColumns = HashMap.fromList
      [ ("0", ["1", "2", "3", "4", "5", "6"])
      , ("1", ["7", "8", "9"])
      , ("2", ["10", "11", "12"])
      ]
    foreignKeys = HashMap.fromList [("10", "7"), ("11", "1")]
    primaryKeys = ["1", "7", "10"]
  in
    SQLSchema { sqlSchema_columnNames    = columnNames
              , sqlSchema_columnTypes    = columnTypes
              , sqlSchema_tableNames     = tableNames
              , sqlSchema_columnToTable  = columnToTable
              , sqlSchema_tableToColumns = tableToColumns
              , sqlSchema_foreignKeys    = foreignKeys
              , sqlSchema_primaryKeys    = primaryKeys
              }

stormRecordQueries :: [Text.Text]
stormRecordQueries =
  [ "select avg(damage_millions_usd), max(damage_millions_usd) from storm where max_speed > 1000"
  , "select sum(number_deaths), sum(damage_millions_usd) from storm where max_speed > (select avg(max_speed) from storm)"
  , "select name, damage_millions_usd from storm order by max_speed desc"
  ]

stormRecordQueriesFails :: [Text.Text]
stormRecordQueriesFails = []

stormRecordParserTests :: TestItem
stormRecordParserTests =
  Group "stormRecord"
    $  (   ParseQueryExprWithGuardsAndTypeChecking stormRecordSchema
       <$> stormRecordQueries
       )
    <> (ParseQueryExprWithGuards stormRecordSchema <$> stormRecordQueries)
    <> (ParseQueryExprWithoutGuards stormRecordSchema <$> stormRecordQueries)
    <> (ParseQueryExprFails stormRecordSchema <$> stormRecordQueriesFails)

stormRecordLexerTests :: TestItem
stormRecordLexerTests =
  Group "stormRecord" $ LexQueryExpr stormRecordSchema <$> stormRecordQueries
