{-# LANGUAGE OverloadedStrings #-}

module PicardLite.Language.SQL.SpiderSQL.PhoneMarket where

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

phoneMarketSchema :: SQLSchema
phoneMarketSchema =
  let
    columnNames = HashMap.fromList
      [ ("1" , "Name")
      , ("10", "Ranking")
      , ("11", "Market_ID")
      , ("12", "Phone_ID")
      , ("13", "Num_of_stock")
      , ("2" , "Phone_ID")
      , ("3" , "Memory_in_G")
      , ("4" , "Carrier")
      , ("5" , "Price")
      , ("6" , "Market_ID")
      , ("7" , "District")
      , ("8" , "Num_of_employees")
      , ("9" , "Num_of_shops")
      ]
    columnTypes = HashMap.fromList
      [ ("1" , ColumnType_TEXT)
      , ("10", ColumnType_NUMBER)
      , ("11", ColumnType_NUMBER)
      , ("12", ColumnType_NUMBER)
      , ("13", ColumnType_NUMBER)
      , ("2" , ColumnType_NUMBER)
      , ("3" , ColumnType_NUMBER)
      , ("4" , ColumnType_TEXT)
      , ("5" , ColumnType_NUMBER)
      , ("6" , ColumnType_NUMBER)
      , ("7" , ColumnType_TEXT)
      , ("8" , ColumnType_NUMBER)
      , ("9" , ColumnType_NUMBER)
      ]
    tableNames =
      HashMap.fromList [("0", "phone"), ("1", "market"), ("2", "phone_market")]
    columnToTable = HashMap.fromList
      [ ("1" , "0")
      , ("10", "1")
      , ("11", "2")
      , ("12", "2")
      , ("13", "2")
      , ("2" , "0")
      , ("3" , "0")
      , ("4" , "0")
      , ("5" , "0")
      , ("6" , "1")
      , ("7" , "1")
      , ("8" , "1")
      , ("9" , "1")
      ]
    tableToColumns = HashMap.fromList
      [ ("0", ["1", "2", "3", "4", "5"])
      , ("1", ["6", "7", "8", "9", "10"])
      , ("2", ["11", "12", "13"])
      ]
    foreignKeys = HashMap.fromList [("11", "6"), ("12", "2")]
    primaryKeys = ["2", "6", "11"]
  in
    SQLSchema { sqlSchema_columnNames    = columnNames
              , sqlSchema_columnTypes    = columnTypes
              , sqlSchema_tableNames     = tableNames
              , sqlSchema_columnToTable  = columnToTable
              , sqlSchema_tableToColumns = tableToColumns
              , sqlSchema_foreignKeys    = foreignKeys
              , sqlSchema_primaryKeys    = primaryKeys
              }

phoneMarketQueries :: [Text.Text]
phoneMarketQueries =
  [ "select t2.name from phone_market as t1 join phone as t2 on t1.phone_id = t2.phone_id group by t2.name order by sum(t1.num_of_stock) desc"
  , "select t2.name from phone_market as t1 join phone as t2 on t1.phone_id = t2.phone_id group by t2.name having sum(t1.num_of_stock) >= 2000"
  , "select t2.name from phone_market as t1 join phone as t2 on t1.phone_id = t2.phone_id group by t2.name having sum(t1.num_of_stock) >= 2000 order by sum(t1.num_of_stock) desc"
  ]

phoneMarketQueriesFails :: [Text.Text]
phoneMarketQueriesFails = []

phoneMarketParserTests :: TestItem
phoneMarketParserTests =
  Group "phoneMarket"
    $  (   ParseQueryExprWithGuardsAndTypeChecking phoneMarketSchema
       <$> phoneMarketQueries
       )
    <> (ParseQueryExprWithGuards phoneMarketSchema <$> phoneMarketQueries)
    <> (ParseQueryExprWithoutGuards phoneMarketSchema <$> phoneMarketQueries)
    <> (ParseQueryExprFails phoneMarketSchema <$> phoneMarketQueriesFails)

phoneMarketLexerTests :: TestItem
phoneMarketLexerTests =
  Group "phoneMarket" $ LexQueryExpr phoneMarketSchema <$> phoneMarketQueries
