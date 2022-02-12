{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use camelCase" #-}

module PicardLite.Types
  ( ColumnId
  , TableId
  , ColumnType
    ( ColumnType_BOOLEAN
    , ColumnType_NUMBER
    , ColumnType_OTHERS
    , ColumnType_TEXT
    , ColumnType_TIME
    , ColumnType__UNKNOWN
    )
  , DBId
  , SQLSchema
    ( SQLSchema
    , sqlSchema_columnNames
    , sqlSchema_columnToTable
    , sqlSchema_columnTypes
    , sqlSchema_foreignKeys
    , sqlSchema_primaryKeys
    , sqlSchema_tableNames
    , sqlSchema_tableToColumns
    )
  ) where

import           Control.Arrow                  ( first )
import qualified Control.DeepSeq               as DeepSeq
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Default                  as Default
import qualified Data.Function                 as Function
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.Hashable                 as Hashable
import qualified Data.Int                      as Int
import qualified Data.List                     as List
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import qualified Data.Ord                      as Ord
import           Data.Ord                       ( Ordering(..)
                                                , compare
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Prelude                       as P
import           Prelude                        ( (.) )

hmMapKeys
  :: (P.Eq b, Hashable.Hashable b) => (a -> b) -> HashMap a x -> HashMap b x
hmMapKeys f = HashMap.fromList . P.map (first f) . HashMap.toList

hmToSortedList :: (Ord.Ord a, Ord.Ord b) => HashMap a b -> [(a, b)]
hmToSortedList = List.sort . P.map (\(k, v) -> (k, v)) . HashMap.toList

keyToStr :: Aeson.ToJSON k => k -> Text.Text
keyToStr = decodeUtf8 . LBS.toStrict . Aeson.encode

class Enum a where
  fromEnum :: a -> P.Int
  toEnum   :: P.Int -> a

type ColumnId = Text.Text
type TableId = Text.Text
type DBId = Text.Text

-- ColumnType
data ColumnType = ColumnType_BOOLEAN
                | ColumnType_TEXT
                | ColumnType_NUMBER
                | ColumnType_TIME
                | ColumnType_OTHERS
                | ColumnType__UNKNOWN P.Int
                  deriving (P.Eq, P.Show)

instance Enum ColumnType where
  toEnum 1   = ColumnType_BOOLEAN
  toEnum 2   = ColumnType_TEXT
  toEnum 3   = ColumnType_NUMBER
  toEnum 4   = ColumnType_TIME
  toEnum 5   = ColumnType_OTHERS
  toEnum val = ColumnType__UNKNOWN val
  fromEnum ColumnType_BOOLEAN        = 1
  fromEnum ColumnType_TEXT           = 2
  fromEnum ColumnType_NUMBER         = 3
  fromEnum ColumnType_TIME           = 4
  fromEnum ColumnType_OTHERS         = 5
  fromEnum (ColumnType__UNKNOWN val) = val

instance P.Ord ColumnType where
  compare = Function.on P.compare fromEnum

instance Aeson.ToJSON ColumnType where
  toJSON = Aeson.toJSON . fromEnum

instance DeepSeq.NFData ColumnType where
  rnf __ColumnType = P.seq __ColumnType ()

instance Default.Default ColumnType where
  def = ColumnType_BOOLEAN

instance Hashable.Hashable ColumnType where
  hashWithSalt _salt _val = Hashable.hashWithSalt _salt (fromEnum _val)

-- SQLSchema
data SQLSchema = SQLSchema
  { sqlSchema_columnNames    :: HashMap.HashMap ColumnId Text.Text
  , sqlSchema_columnTypes    :: HashMap.HashMap ColumnId ColumnType
  , sqlSchema_tableNames     :: HashMap.HashMap TableId Text.Text
  , sqlSchema_columnToTable  :: HashMap.HashMap ColumnId TableId
  , sqlSchema_tableToColumns :: HashMap.HashMap TableId [ColumnId]
  , sqlSchema_foreignKeys    :: HashMap.HashMap ColumnId ColumnId
  , sqlSchema_primaryKeys    :: [ColumnId]
  }
  deriving (P.Eq, P.Show)

instance Aeson.ToJSON SQLSchema where
  toJSON (SQLSchema colNames colTypes tblNames colToTbls tblToCols fKeys pKeys)
    = Aeson.object
      (  "columnNames"
      .= hmMapKeys keyToStr colNames
      :  "columnTypes"
      .= hmMapKeys keyToStr colTypes
      :  "tableNames"
      .= hmMapKeys keyToStr tblNames
      :  "columnToTable"
      .= hmMapKeys keyToStr colToTbls
      :  "tableToColumns"
      .= hmMapKeys keyToStr tblToCols
      :  "foreignKeys"
      .= hmMapKeys keyToStr fKeys
      :  "primaryKeys"
      .= pKeys
      :  P.mempty
      )

instance DeepSeq.NFData SQLSchema where
  rnf (SQLSchema colNames colTypes tblNames colToTbls tblToCols fKeys pKeys) =
    DeepSeq.rnf colNames
      `P.seq` DeepSeq.rnf colTypes
      `P.seq` DeepSeq.rnf tblNames
      `P.seq` DeepSeq.rnf colToTbls
      `P.seq` DeepSeq.rnf tblToCols
      `P.seq` DeepSeq.rnf fKeys
      `P.seq` DeepSeq.rnf pKeys
      `P.seq` ()

instance Default.Default SQLSchema where
  def = SQLSchema HashMap.empty
                  HashMap.empty
                  HashMap.empty
                  HashMap.empty
                  HashMap.empty
                  HashMap.empty
                  Default.def

instance Hashable.Hashable SQLSchema where
  hashWithSalt salt (SQLSchema colNames colTypes tblNames colToTbls tblToCols fKeys pKeys)
    = let h0 = Hashable.hashWithSalt salt (hmToSortedList colNames)
          h1 = Hashable.hashWithSalt h0 (hmToSortedList colTypes)
          h2 = Hashable.hashWithSalt h1 (hmToSortedList tblNames)
          h3 = Hashable.hashWithSalt h2 (hmToSortedList colToTbls)
          h4 = Hashable.hashWithSalt h3 (hmToSortedList tblToCols)
          h5 = Hashable.hashWithSalt h4 (hmToSortedList fKeys)
      in  Hashable.hashWithSalt h5 pKeys


instance Ord.Ord SQLSchema where
  compare (SQLSchema colNames1 colTypes1 tblNames1 colToTbls1 tblToCols1 fKeys1 pKeys1) (SQLSchema colNames2 colTypes2 tblNames2 colToTbls2 tblToCols2 fKeys2 pKeys2)
    = foldl1'
      step
      [ compare (hmToSortedList colNames1)  (hmToSortedList colNames2)
      , compare (hmToSortedList colTypes1)  (hmToSortedList colTypes2)
      , compare (hmToSortedList tblNames1)  (hmToSortedList tblNames2)
      , compare (hmToSortedList colToTbls1) (hmToSortedList colToTbls2)
      , compare (hmToSortedList tblToCols1) (hmToSortedList tblToCols2)
      , compare (hmToSortedList fKeys1)     (hmToSortedList fKeys2)
      , compare pKeys1                      pKeys2
      ]
   where
    step EQ x = x
    step x  _ = x
