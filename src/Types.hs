module Types
    ( Article (..)
    , Comment (..)
    , Listing (..)
    ) where

import Data.Text.Lazy
import Data.Time.Calendar (CalendarDiffDays)

data Article = Article {
    aId :: Text
  , aUrl :: Text
  , aTitle :: Text
  , aAuthor :: Text
  , aText :: Text
  , aComments :: [Comment]
  } deriving Show

data Comment = Comment {
    cId :: Text
  , cAuthor :: Text
  , cText :: Text
  , cChildren :: [Comment]
  } deriving Show

newtype Listing = Listing {
    lListing :: [Article]
  } deriving Show

data Ban = Ban {
    user :: Text
  , date :: Text
  , duration :: CalendarDiffDays
  , evidence :: [Text]
} deriving Show
