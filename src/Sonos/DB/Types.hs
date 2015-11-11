module Sonos.DB.Types where

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import qualified Data.Text as T


data Track = Track Int T.Text Int T.Text Int Int deriving (Show)

instance FromRow Track where
  fromRow = Track <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Track where
  toRow (Track id_ pa tno tn al ar) = toRow (id_, pa, tno, tn, al, ar)

data Album = Album Int T.Text T.Text Int deriving (Show)

instance FromRow Album where
  fromRow = Album <$> field <*> field <*> field <*> field

instance ToRow Album where
  toRow (Album id_ pa an ar) = toRow (id_, pa, an, ar)

data Artist = Artist Int T.Text T.Text deriving (Show)

instance FromRow Artist where
  fromRow = Artist <$> field <*> field <*> field

instance ToRow Artist where
  toRow (Artist id_ pa an) = toRow (id_, pa, an)
