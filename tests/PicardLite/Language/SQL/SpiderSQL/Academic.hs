{-# LANGUAGE OverloadedStrings #-}

module PicardLite.Language.SQL.SpiderSQL.Academic where

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text                     as Text
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

academicSchema :: SQLSchema
academicSchema =
  let columnNames = HashMap.fromList
        [ ("1" , "aid")
        , ("10", "aid")
        , ("11", "did")
        , ("12", "cid")
        , ("13", "did")
        , ("14", "homepage")
        , ("15", "jid")
        , ("16", "name")
        , ("17", "did")
        , ("18", "jid")
        , ("19", "keyword")
        , ("2" , "homepage")
        , ("20", "kid")
        , ("21", "did")
        , ("22", "kid")
        , ("23", "abstract")
        , ("24", "cid")
        , ("25", "citation_num")
        , ("26", "jid")
        , ("27", "pid")
        , ("28", "reference_num")
        , ("29", "title")
        , ("3" , "name")
        , ("30", "year")
        , ("31", "did")
        , ("32", "pid")
        , ("33", "continent")
        , ("34", "homepage")
        , ("35", "name")
        , ("36", "oid")
        , ("37", "pid")
        , ("38", "kid")
        , ("39", "aid")
        , ("4" , "oid")
        , ("40", "pid")
        , ("41", "cited")
        , ("42", "citing")
        , ("5" , "cid")
        , ("6" , "homepage")
        , ("7" , "name")
        , ("8" , "did")
        , ("9" , "name")
        ]
      columnTypes = HashMap.fromList
        [ ("1" , ColumnType_NUMBER)
        , ("10", ColumnType_NUMBER)
        , ("11", ColumnType_NUMBER)
        , ("12", ColumnType_NUMBER)
        , ("13", ColumnType_NUMBER)
        , ("14", ColumnType_TEXT)
        , ("15", ColumnType_NUMBER)
        , ("16", ColumnType_TEXT)
        , ("17", ColumnType_NUMBER)
        , ("18", ColumnType_NUMBER)
        , ("19", ColumnType_TEXT)
        , ("2" , ColumnType_TEXT)
        , ("20", ColumnType_NUMBER)
        , ("21", ColumnType_NUMBER)
        , ("22", ColumnType_NUMBER)
        , ("23", ColumnType_TEXT)
        , ("24", ColumnType_NUMBER)
        , ("25", ColumnType_NUMBER)
        , ("26", ColumnType_NUMBER)
        , ("27", ColumnType_NUMBER)
        , ("28", ColumnType_NUMBER)
        , ("29", ColumnType_TEXT)
        , ("3" , ColumnType_TEXT)
        , ("30", ColumnType_NUMBER)
        , ("31", ColumnType_NUMBER)
        , ("32", ColumnType_NUMBER)
        , ("33", ColumnType_TEXT)
        , ("34", ColumnType_TEXT)
        , ("35", ColumnType_TEXT)
        , ("36", ColumnType_NUMBER)
        , ("37", ColumnType_NUMBER)
        , ("38", ColumnType_NUMBER)
        , ("39", ColumnType_NUMBER)
        , ("4" , ColumnType_NUMBER)
        , ("40", ColumnType_NUMBER)
        , ("41", ColumnType_NUMBER)
        , ("42", ColumnType_NUMBER)
        , ("5" , ColumnType_NUMBER)
        , ("6" , ColumnType_TEXT)
        , ("7" , ColumnType_TEXT)
        , ("8" , ColumnType_NUMBER)
        , ("9" , ColumnType_TEXT)
        ]
      tableNames = HashMap.fromList
        [ ("0" , "author")
        , ("1" , "conference")
        , ("10", "domain_publication")
        , ("11", "organization")
        , ("12", "publication_keyword")
        , ("13", "writes")
        , ("14", "cite")
        , ("2" , "domain")
        , ("3" , "domain_author")
        , ("4" , "domain_conference")
        , ("5" , "journal")
        , ("6" , "domain_journal")
        , ("7" , "keyword")
        , ("8" , "domain_keyword")
        , ("9" , "publication")
        ]
      columnToTable = HashMap.fromList
        [ ("1" , "0")
        , ("10", "3")
        , ("11", "3")
        , ("12", "4")
        , ("13", "4")
        , ("14", "5")
        , ("15", "5")
        , ("16", "5")
        , ("17", "6")
        , ("18", "6")
        , ("19", "7")
        , ("2" , "0")
        , ("20", "7")
        , ("21", "8")
        , ("22", "8")
        , ("23", "9")
        , ("24", "9")
        , ("25", "9")
        , ("26", "9")
        , ("27", "9")
        , ("28", "9")
        , ("29", "9")
        , ("3" , "0")
        , ("30", "9")
        , ("31", "10")
        , ("32", "10")
        , ("33", "11")
        , ("34", "11")
        , ("35", "11")
        , ("36", "11")
        , ("37", "12")
        , ("38", "12")
        , ("39", "13")
        , ("4" , "0")
        , ("40", "13")
        , ("41", "14")
        , ("42", "14")
        , ("5" , "1")
        , ("6" , "1")
        , ("7" , "1")
        , ("8" , "2")
        , ("9" , "2")
        ]
      tableToColumns = HashMap.fromList
        [ ("0" , ["1", "2", "3", "4"])
        , ("1" , ["5", "6", "7"])
        , ("10", ["31", "32"])
        , ("11", ["33", "34", "35", "36"])
        , ("12", ["37", "38"])
        , ("13", ["39", "40"])
        , ("14", ["41", "42"])
        , ("2" , ["8", "9"])
        , ("3" , ["10", "11"])
        , ("4" , ["12", "13"])
        , ("5" , ["14", "15", "16"])
        , ("6" , ["17", "18"])
        , ("7" , ["19", "20"])
        , ("8" , ["21", "22"])
        , ("9", ["23", "24", "25", "26", "27", "28", "29", "30"])
        ]
      foreignKeys = HashMap.fromList
        [ ("10", "1")
        , ("11", "8")
        , ("12", "5")
        , ("13", "8")
        , ("17", "8")
        , ("18", "15")
        , ("21", "8")
        , ("22", "20")
        , ("24", "5")
        , ("26", "15")
        , ("31", "8")
        , ("32", "27")
        , ("37", "27")
        , ("38", "20")
        , ("39", "1")
        , ("40", "27")
        , ("41", "27")
        , ("42", "27")
        ]
      primaryKeys =
        [ "1"
        , "5"
        , "8"
        , "11"
        , "13"
        , "15"
        , "17"
        , "20"
        , "21"
        , "27"
        , "31"
        , "36"
        , "38"
        , "39"
        ]
  in  SQLSchema { sqlSchema_columnNames    = columnNames
                , sqlSchema_columnTypes    = columnTypes
                , sqlSchema_tableNames     = tableNames
                , sqlSchema_columnToTable  = columnToTable
                , sqlSchema_tableToColumns = tableToColumns
                , sqlSchema_foreignKeys    = foreignKeys
                , sqlSchema_primaryKeys    = primaryKeys
                }

academicQueries :: [Text.Text]
academicQueries =
  [ "SELECT * FROM journal"
  , "SELECT homepage FROM journal"
  , "SELECT homepage FROM journal WHERE name  =  \"PVLDB\";"
  , "SELECT homepage FROM author WHERE name  =  \"H. V. Jagadish\";"
  , "SELECT abstract FROM publication WHERE title  =  \"Making database systems usable\";"
  , "SELECT YEAR FROM publication WHERE title  =  \"Making database systems usable\";"
  , "SELECT title FROM publication WHERE YEAR  >  2000;"
  , "SELECT homepage FROM conference WHERE name  =  \"VLDB\";"
  , "SELECT keyword FROM keyword;"
  , "SELECT name FROM organization;"
  , "SELECT name FROM organization WHERE continent  =  \"North America\";"
  , "SELECT homepage FROM organization WHERE name  =  \"University of Michigan\";"
  , "SELECT reference_num FROM publication WHERE title  =  \"Making database systems usable\";"
  , "SELECT citation_num FROM publication WHERE title  =  \"Making database systems usable\";"
  , "SELECT title FROM publication WHERE citation_num  >  200;"
  , "SELECT t1.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\" AND t4.year  =  2010;"
  , "SELECT t1.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\" AND t4.year  >  2010;"
  , "SELECT t1.name FROM author as t1"
  , "SELECT t1.name FROM writes AS t3 JOIN author AS t1 ON t3.aid = t1.aid"
  , "SELECT t1.* FROM publication AS t4 JOIN writes AS t3 ON t3.pid = t4.pid JOIN author AS t1 ON t3.aid = t1.aid"
  , "SELECT t1.name FROM publication AS t4 JOIN writes AS t3 ON t3.pid = t4.pid JOIN author AS t1 ON t3.aid = t1.aid"
  , "SELECT t1.name FROM publication AS t4 JOIN author AS t1 ON t3.aid = t1.aid JOIN writes AS t3 ON t3.pid = t4.pid"
  , "SELECT * FROM publication JOIN conference ON conference.cid = publication.cid"
  , "SELECT * FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid"
  , "SELECT * FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid"
  , "SELECT t1.* FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN author AS t1 ON t3.aid  =  t1.aid JOIN writes AS t3 ON t3.pid  =  t4.pid"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\" AND t4.year  =  2002;"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\" AND t4.year  <  2002;"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\" AND t4.year  <  2002 AND t4.year  >  1995;"
  , "SELECT t3.name FROM DOMAIN AS t3 JOIN domain_journal AS t1 ON t3.did  =  t1.did JOIN journal AS t2 ON t2.jid  =  t1.jid WHERE t2.name  =  \"PVLDB\";"
  , "SELECT t1.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\";"
  , "SELECT t2.name FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t2.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t2.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t2.name FROM domain_author AS t3 JOIN author AS t1 ON t3.aid  =  t1.aid JOIN DOMAIN AS t2 ON t2.did  =  t3.did WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t1.name FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t3.title  =  \"Making database systems usable\";"
  , "SELECT t1.name FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t2.title  =  \"Making database systems usable\";"
  , "SELECT t3.title FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\";"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\";"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  >  2000;"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  >  2000;"
  , "SELECT t4.title FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"PVLDB\";"
  , "SELECT t4.title FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"VLDB\";"
  , "SELECT t3.title FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\" AND t3.year  >  2000;"
  , "SELECT t4.title FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"PVLDB\" AND t4.year  >  2000;"
  , "SELECT t4.title FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"VLDB\" AND t4.year  >  2000;"
  , "SELECT t2.name FROM domain_conference AS t3 JOIN conference AS t1 ON t3.cid  =  t1.cid JOIN DOMAIN AS t2 ON t2.did  =  t3.did WHERE t1.name  =  \"VLDB\";"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\";"
  , "SELECT t1.keyword FROM DOMAIN AS t3 JOIN domain_keyword AS t2 ON t3.did  =  t2.did JOIN keyword AS t1 ON t1.kid  =  t2.kid WHERE t3.name  =  \"Databases\";"
  , "SELECT t3.title FROM publication_keyword AS t2 JOIN keyword AS t1 ON t2.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t2.pid WHERE t1.keyword  =  \"Natural Language\";"
  , "SELECT t1.keyword FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t3.pid WHERE t2.title  =  \"Making database systems usable\";"
  , "SELECT t1.keyword FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\";"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t2.name  =  \"VLDB\";"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\";"
  , "SELECT t1.keyword FROM organization AS t6 JOIN author AS t2 ON t6.oid  =  t2.oid JOIN writes AS t4 ON t4.aid  =  t2.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN publication_keyword AS t3 ON t5.pid  =  t3.pid JOIN keyword AS t1 ON t3.kid  =  t1.kid WHERE t6.name  =  \"University of Michigan\";"
  , "SELECT t5.title FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t5 ON t5.pid  =  t3.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.keyword  =  \"User Study\";"
  , "SELECT t4.title FROM publication_keyword AS t2 JOIN keyword AS t1 ON t2.kid  =  t1.kid JOIN publication AS t4 ON t4.pid  =  t2.pid JOIN journal AS t3 ON t4.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t1.keyword  =  \"Keyword search\";"
  , "SELECT t4.title FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t4 ON t4.pid  =  t3.pid JOIN conference AS t2 ON t4.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" AND t1.keyword  =  \"Information Retrieval\";"
  , "SELECT t2.name FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t1.keyword  =  \"Relational Database\";"
  , "SELECT t2.name FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\";"
  , "SELECT t2.name FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\" AND t2.continent  =  \"North America\";"
  , "SELECT t1.name FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid WHERE t2.name  =  \"University of Michigan\";"
  , "SELECT t1.name FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\" AND t2.name  =  \"University of Michigan\";"
  , "SELECT t4.title FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\";"
  , "SELECT t4.title FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\" AND t4.year  >  2000;"
  , "SELECT t5.title FROM organization AS t3 JOIN author AS t1 ON t3.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN conference AS t2 ON t5.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" AND t3.name  =  \"University of Michigan\";"
  , "SELECT t5.title FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN journal AS t3 ON t5.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t2.name  =  \"University of Michigan\";"
  , "SELECT t5.title FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN journal AS t3 ON t5.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t2.name  =  \"University of Michigan\" AND t5.year  >  2000;"
  , "SELECT t3.title FROM DOMAIN AS t2 JOIN domain_publication AS t1 ON t2.did  =  t1.did JOIN publication AS t3 ON t3.pid  =  t1.pid WHERE t2.name  =  \"Databases\" AND t3.citation_num  >  200;"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.citation_num  >  200;"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.citation_num  >  200;"
  , "SELECT t3.title FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\" AND t3.citation_num  >  200;"
  , "SELECT t4.title FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"PVLDB\" AND t4.citation_num  >  200;"
  , "SELECT t4.title FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"VLDB\" AND t4.citation_num  >  200;"
  , "SELECT title FROM publication WHERE citation_num  >  200 AND YEAR  >  2000;"
  , "SELECT t3.title FROM DOMAIN AS t2 JOIN domain_publication AS t1 ON t2.did  =  t1.did JOIN publication AS t3 ON t3.pid  =  t1.pid WHERE t2.name  =  \"Databases\" AND t3.citation_num  >  200 AND t3.year  >  2000;"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.citation_num  >  200 AND t2.year  >  2000;"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.citation_num  >  200 AND t2.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t3.title )  ,  t3.year FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\" GROUP BY t3.year;"
  , "SELECT COUNT ( DISTINCT t1.name ) FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t3.title  =  \"Making database systems usable\";"
  , "SELECT YEAR  ,  SUM ( citation_num ) FROM publication WHERE title  =  \"Making database systems usable\" GROUP BY YEAR;"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t3 JOIN cite AS t1 ON t3.pid  =  t1.cited JOIN publication AS t2 ON t2.pid  =  t1.citing WHERE t3.title  =  \"Making database systems usable\" AND t2.year  <  2010;"
  , "SELECT COUNT ( DISTINCT t3.title ) FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\";"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\";"
  , "SELECT COUNT ( DISTINCT title ) FROM publication WHERE YEAR  >  2000;"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"PVLDB\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"VLDB\";"
  , "SELECT COUNT ( DISTINCT t3.title ) FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\" AND t3.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"PVLDB\" AND t4.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t2.name  =  \"VLDB\" AND t4.year  >  2000;"
  , "SELECT COUNT ( DISTINCT keyword ) FROM keyword;"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM DOMAIN AS t3 JOIN domain_keyword AS t2 ON t3.did  =  t2.did JOIN keyword AS t1 ON t1.kid  =  t2.kid WHERE t3.name  =  \"Databases\";"
  , "SELECT COUNT ( DISTINCT t3.title ) FROM publication_keyword AS t2 JOIN keyword AS t1 ON t2.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t2.pid WHERE t1.keyword  =  \"Natural Language\";"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t3.pid WHERE t2.title  =  \"Making database systems usable\";"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t2.name  =  \"VLDB\";"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\";"
  , "SELECT COUNT ( DISTINCT t1.keyword ) FROM organization AS t6 JOIN author AS t2 ON t6.oid  =  t2.oid JOIN writes AS t4 ON t4.aid  =  t2.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN publication_keyword AS t3 ON t5.pid  =  t3.pid JOIN keyword AS t1 ON t3.kid  =  t1.kid WHERE t6.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t5 ON t5.pid  =  t3.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.keyword  =  \"User Study\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication_keyword AS t2 JOIN keyword AS t1 ON t2.kid  =  t1.kid JOIN publication AS t4 ON t4.pid  =  t2.pid JOIN journal AS t3 ON t4.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t1.keyword  =  \"Keyword search\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM publication_keyword AS t3 JOIN keyword AS t1 ON t3.kid  =  t1.kid JOIN publication AS t4 ON t4.pid  =  t3.pid JOIN conference AS t2 ON t4.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" AND t1.keyword  =  \"Information Retrieval\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t1.keyword  =  \"Relational Database\";"
  , "SELECT SUM ( t3.citation_num ) FROM publication_keyword AS t2 JOIN keyword AS t1 ON t2.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t2.pid WHERE t1.keyword  =  \"Natural Language\";"
  , "SELECT COUNT ( DISTINCT name ) FROM organization;"
  , "SELECT COUNT ( DISTINCT name ) FROM organization WHERE continent  =  \"North America\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\" AND t2.continent  =  \"North America\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM domain_author AS t6 JOIN author AS t1 ON t6.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t6.did JOIN domain_publication AS t2 ON t3.did  =  t2.did JOIN organization AS t5 ON t5.oid  =  t1.oid JOIN publication AS t4 ON t4.pid  =  t2.pid WHERE t3.name  =  \"Databases\" AND t5.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t4.title ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\" AND t4.year  >  2000;"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM organization AS t3 JOIN author AS t1 ON t3.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN conference AS t2 ON t5.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" AND t3.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN journal AS t3 ON t5.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t2.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t4 ON t4.aid  =  t1.aid JOIN publication AS t5 ON t4.pid  =  t5.pid JOIN journal AS t3 ON t5.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" AND t2.name  =  \"University of Michigan\" AND t5.year  >  2000;"
  , "SELECT SUM ( t4.citation_num ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t1.name ) FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid WHERE t2.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t1.name ) FROM domain_author AS t4 JOIN author AS t1 ON t4.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t4.did JOIN organization AS t2 ON t2.oid  =  t1.oid WHERE t3.name  =  \"Databases\" AND t2.name  =  \"University of Michigan\";"
  , "SELECT COUNT ( DISTINCT t1.name ) FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\";"
  , "SELECT COUNT ( DISTINCT t1.name ) FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\";"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  <  2000;"
  , "SELECT COUNT ( DISTINCT t2.title ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  <  2000;"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\";"
  , "SELECT t2.citation_num FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\";"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  =  2005;"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  <  2005;"
  , "SELECT t2.year  ,  SUM ( t2.citation_num ) FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" GROUP BY t2.year;"
  , "SELECT COUNT ( DISTINCT t2.title )  ,  t2.year FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" GROUP BY t2.year;"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\";"
  , "SELECT t2.citation_num FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\";"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  =  2005;"
  , "SELECT SUM ( t2.citation_num ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  <  2005;"
  , "SELECT t2.year  ,  SUM ( t2.citation_num ) FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" GROUP BY t2.year;"
  , "SELECT COUNT ( DISTINCT t2.title )  ,  t2.year FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" GROUP BY t2.year;"
  , "SELECT t2.name FROM writes AS t4 JOIN author AS t2 ON t4.aid  =  t2.aid JOIN publication AS t7 ON t4.pid  =  t7.pid JOIN writes AS t5 ON t5.pid  =  t7.pid JOIN writes AS t6 ON t6.pid  =  t7.pid JOIN author AS t1 ON t5.aid  =  t1.aid JOIN author AS t3 ON t6.aid  =  t3.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t3.name  =  \"Divesh Srivastava\";"
  , "SELECT t2.name FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" AND t5.year  >  2000;"
  , "SELECT t5.title FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Divesh Srivastava\";"
  , "SELECT t5.title FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Yunyao Li\" AND t5.year  >  2005;"
  , "SELECT t6.title FROM publication AS t6 JOIN journal AS t4 ON t6.jid  =  t4.jid JOIN writes AS t3 ON t3.pid  =  t6.pid JOIN writes AS t5 ON t5.pid  =  t6.pid JOIN author AS t1 ON t5.aid  =  t1.aid JOIN author AS t2 ON t3.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Yunyao Li\" AND t4.name  =  \"PVLDB\";"
  , "SELECT t6.title FROM publication AS t6 JOIN journal AS t4 ON t6.jid  =  t4.jid JOIN writes AS t3 ON t3.pid  =  t6.pid JOIN writes AS t5 ON t5.pid  =  t6.pid JOIN author AS t1 ON t5.aid  =  t1.aid JOIN author AS t2 ON t3.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Yunyao Li\" AND t4.name  =  \"PVLDB\" AND t6.year  >  2005;"
  , "SELECT t2.name FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t5.title FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Divesh Srivastava\" AND t5.year  <  2000;"
  , "SELECT t2.name FROM publication AS t7 JOIN cite AS t5 ON t7.pid  =  t5.citing JOIN publication AS t6 ON t6.pid  =  t5.cited JOIN writes AS t3 ON t3.pid  =  t7.pid JOIN writes AS t4 ON t4.pid  =  t6.pid JOIN author AS t2 ON t3.aid  =  t2.aid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Divesh Srivastava\";"
  , "SELECT COUNT ( DISTINCT t5.title ) FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Divesh Srivastava\" AND t5.year  <  2000;"
  , "SELECT COUNT ( DISTINCT t7.title ) FROM writes AS t4 JOIN author AS t2 ON t4.aid  =  t2.aid JOIN publication AS t7 ON t4.pid  =  t7.pid JOIN writes AS t5 ON t5.pid  =  t7.pid JOIN writes AS t6 ON t6.pid  =  t7.pid JOIN author AS t1 ON t5.aid  =  t1.aid JOIN author AS t3 ON t6.aid  =  t3.aid WHERE t2.name  =  \"Cong Yu\" AND t1.name  =  \"H. V. Jagadish\" AND t3.name  =  \"Yunyao Li\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT COUNT ( DISTINCT t2.name ) FROM publication AS t7 JOIN cite AS t5 ON t7.pid  =  t5.citing JOIN publication AS t6 ON t6.pid  =  t5.cited JOIN writes AS t3 ON t3.pid  =  t7.pid JOIN writes AS t4 ON t4.pid  =  t6.pid JOIN author AS t2 ON t3.aid  =  t2.aid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\";"
  , "SELECT t5.title FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"H. V. Jagadish\" AND t1.name  =  \"Divesh Srivastava\" AND t5.citation_num  >  200;"
  , "SELECT t2.name FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t2.name ORDER BY COUNT ( DISTINCT t3.title ) DESC LIMIT 1;"
  , "SELECT t2.name FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t2.name ORDER BY COUNT ( DISTINCT t3.title ) DESC LIMIT 1;"
  , "SELECT t3.name FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t3.name ORDER BY COUNT ( DISTINCT t2.title ) DESC LIMIT 1;"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" GROUP BY t1.keyword ORDER BY COUNT ( DISTINCT t3.title ) DESC LIMIT 1;"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" GROUP BY t1.keyword ORDER BY COUNT ( DISTINCT t2.title ) DESC LIMIT 1;"
  , "SELECT t1.keyword FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" GROUP BY t1.keyword ORDER BY COUNT ( DISTINCT t3.title ) DESC LIMIT 1;"
  , "SELECT t1.name FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\" GROUP BY t1.name ORDER BY SUM ( t4.citation_num ) DESC LIMIT 1;"
  , "SELECT t1.name FROM DOMAIN AS t4 JOIN domain_publication AS t2 ON t4.did  =  t2.did JOIN publication AS t5 ON t5.pid  =  t2.pid JOIN writes AS t3 ON t3.pid  =  t5.pid JOIN author AS t1 ON t3.aid  =  t1.aid JOIN organization AS t6 ON t6.oid  =  t1.oid WHERE t4.name  =  \"Databases\" AND t6.name  =  \"University of Michigan\" GROUP BY t1.name ORDER BY SUM ( t5.citation_num ) DESC LIMIT 1;"
  , "SELECT t5.title FROM writes AS t3 JOIN author AS t2 ON t3.aid  =  t2.aid JOIN publication AS t5 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t5.pid JOIN author AS t1 ON t4.aid  =  t1.aid WHERE t2.name  =  \"Divesh Srivastava\" AND t1.name  =  \"H. V. Jagadish\" ORDER BY t5.citation_num DESC LIMIT 1;"
  , "SELECT t2.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" GROUP BY t2.name HAVING COUNT ( DISTINCT t4.title )  >  10;"
  , "SELECT t2.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" GROUP BY t2.name ORDER BY COUNT ( DISTINCT t4.title ) DESC LIMIT 1;"
  , "SELECT t2.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" GROUP BY t2.name HAVING COUNT ( DISTINCT t4.title )  >  10;"
  , "SELECT t2.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t1.name  =  \"H. V. Jagadish\" GROUP BY t2.name ORDER BY COUNT ( DISTINCT t4.title ) DESC LIMIT 1;"
  , "SELECT title FROM publication ORDER BY citation_num DESC LIMIT 1;"
  , "SELECT t3.title FROM DOMAIN AS t2 JOIN domain_publication AS t1 ON t2.did  =  t1.did JOIN publication AS t3 ON t3.pid  =  t1.pid WHERE t2.name  =  \"Databases\" ORDER BY t3.citation_num DESC LIMIT 1;"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" ORDER BY t2.citation_num DESC LIMIT 1;"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" ORDER BY t2.citation_num DESC LIMIT 1;"
  , "SELECT t3.title FROM writes AS t2 JOIN author AS t1 ON t2.aid  =  t1.aid JOIN publication AS t3 ON t2.pid  =  t3.pid WHERE t1.name  =  \"H. V. Jagadish\" ORDER BY t3.citation_num DESC LIMIT 1;"
  , "SELECT title FROM publication WHERE YEAR  >  2000 ORDER BY citation_num DESC LIMIT 1;"
  , "SELECT t3.title FROM DOMAIN AS t2 JOIN domain_publication AS t1 ON t2.did  =  t1.did JOIN publication AS t3 ON t3.pid  =  t1.pid WHERE t2.name  =  \"Databases\" AND t3.year  >  2000 ORDER BY t3.citation_num DESC LIMIT 1;"
  , "SELECT t2.title FROM publication AS t2 JOIN journal AS t1 ON t2.jid  =  t1.jid WHERE t1.name  =  \"PVLDB\" AND t2.year  >  2000 ORDER BY t2.citation_num DESC LIMIT 1;"
  , "SELECT t2.title FROM publication AS t2 JOIN conference AS t1 ON t2.cid  =  t1.cid WHERE t1.name  =  \"VLDB\" AND t2.year  >  2000 ORDER BY t2.citation_num DESC LIMIT 1;"
  , "SELECT t1.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\" GROUP BY t1.name HAVING COUNT ( DISTINCT t4.title )  >  10;"
  , "SELECT t1.name FROM publication AS t4 JOIN journal AS t2 ON t4.jid  =  t2.jid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"PVLDB\" GROUP BY t1.name ORDER BY COUNT ( DISTINCT t4.title ) DESC LIMIT 1;"
  , "SELECT t2.name FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t2.name HAVING COUNT ( DISTINCT t3.title )  >  10;"
  , "SELECT t2.name FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t2.name HAVING COUNT ( DISTINCT t3.title )  >  60;"
  , "SELECT t3.name FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t1.keyword  =  \"Relational Database\" GROUP BY t3.name HAVING COUNT ( DISTINCT t2.title )  >  60;"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t4.pid JOIN conference AS t2 ON t3.cid  =  t2.cid WHERE t2.name  =  \"VLDB\" GROUP BY t1.keyword HAVING COUNT ( DISTINCT t3.title )  >  100;"
  , "SELECT t1.keyword FROM publication_keyword AS t4 JOIN keyword AS t1 ON t4.kid  =  t1.kid JOIN publication AS t2 ON t2.pid  =  t4.pid JOIN journal AS t3 ON t2.jid  =  t3.jid WHERE t3.name  =  \"PVLDB\" GROUP BY t1.keyword HAVING COUNT ( DISTINCT t2.title )  >  100;"
  , "SELECT t1.keyword FROM publication_keyword AS t5 JOIN keyword AS t1 ON t5.kid  =  t1.kid JOIN publication AS t3 ON t3.pid  =  t5.pid JOIN writes AS t4 ON t4.pid  =  t3.pid JOIN author AS t2 ON t4.aid  =  t2.aid WHERE t2.name  =  \"H. V. Jagadish\" GROUP BY t1.keyword HAVING COUNT ( DISTINCT t3.title )  >  10;"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\" GROUP BY t1.name HAVING COUNT ( DISTINCT t4.title )  >  10;"
  , "SELECT t1.name FROM publication AS t4 JOIN conference AS t2 ON t4.cid  =  t2.cid JOIN writes AS t3 ON t3.pid  =  t4.pid JOIN author AS t1 ON t3.aid  =  t1.aid WHERE t2.name  =  \"VLDB\" GROUP BY t1.name ORDER BY COUNT ( DISTINCT t4.title ) DESC LIMIT 1;"
  , "SELECT t1.name FROM organization AS t2 JOIN author AS t1 ON t2.oid  =  t1.oid JOIN writes AS t3 ON t3.aid  =  t1.aid JOIN publication AS t4 ON t3.pid  =  t4.pid WHERE t2.name  =  \"University of Michigan\" GROUP BY t1.name HAVING SUM ( t4.citation_num )  >  5000;"
  , "SELECT t1.name FROM domain_author AS t6 JOIN author AS t1 ON t6.aid  =  t1.aid JOIN DOMAIN AS t3 ON t3.did  =  t6.did JOIN organization AS t5 ON t5.oid  =  t1.oid JOIN writes AS t2 ON t2.aid  =  t1.aid JOIN publication AS t4 ON t2.pid  =  t4.pid WHERE t3.name  =  \"Databases\" AND t5.name  =  \"University of Michigan\" GROUP BY t1.name HAVING SUM ( t4.citation_num )  >  5000;"
  , "SELECT t1.name FROM writes AS t3 JOIN author AS t1 ON t3.aid = t1.aid"
  ]

academicQueriesFailsTypeChecking :: [Text.Text]
academicQueriesFailsTypeChecking =
  ["SELECT homepage FROM journal WHERE name = 0"]

academicParserTests :: TestItem
academicParserTests =
  Group "academic"
    $  (   ParseQueryExprWithGuardsAndTypeChecking academicSchema
       <$> academicQueries
       )
    <> (ParseQueryExprWithGuards academicSchema <$> academicQueries)
    <> (ParseQueryExprWithoutGuards academicSchema <$> academicQueries)
    <> (ParseQueryExprFails academicSchema <$> [])
    <> (   ParseQueryExprFailsTypeChecking academicSchema
       <$> academicQueriesFailsTypeChecking
       )

academicLexerTests :: TestItem
academicLexerTests =
  Group "academic" $ LexQueryExpr academicSchema <$> academicQueries
