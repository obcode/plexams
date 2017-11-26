{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.Groups
  ( Group(..)
  , Degree(..)
  , allDegrees
  , Registrations(..)
  , parseGroup
  ) where

import           Data.Aeson
import           Data.Char    (digitToInt)
import qualified Data.Map     as M
import           Data.Monoid  ((<>))
import           GHC.Generics
import           TextShow     (TextShow, showb)

data Group = Group
    { groupDegree        :: Degree
    , groupSemester      :: Maybe Int
    , groupSubgroup      :: Maybe Subgroup
    , groupRegistrations :: Maybe Integer
    }
  deriving (Eq, Ord, Generic)

instance FromJSON Group
instance ToJSON Group

instance Show Group where
    show (Group d mI mS mReg) = show d
      ++ maybe "" show mI
      ++ maybe "" show mS
      ++ maybe "" (("("++) . (++")") . show) mReg

instance TextShow Group where
  showb (Group d mI mS mReg) = showb d
    <> maybe "" showb mI
    <> maybe "" showb mS
    <> maybe "" (("("<>) . (<>")") . showb) mReg

data Degree = IB | IC | IF | GO | IG | IN | IS | ALL
  deriving (Show, Eq, Ord, Read, Enum, Generic)

instance FromJSON Degree
instance ToJSON Degree

allDegrees :: [Degree]
allDegrees = [IB .. IS]

instance TextShow Degree where
  showb IB = "IB"
  showb IC = "IC"
  showb IF = "IF"
  showb GO = "GO"
  showb IG = "IG"
  showb IN = "IN"
  showb IS = "IS"
  showb ALL = "ALL"

data Subgroup = A | B | C
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Subgroup
instance ToJSON Subgroup

instance TextShow Subgroup where
  showb A = "A"
  showb B = "B"
  showb C = "C"

instance Read Group where
    readsPrec _ str = [(parseGroup str, "")]

parseGroup :: String -> Group
parseGroup str = Group
    { groupDegree = str2Degree $ take 2 str
    , groupSemester = if length str > 2
                          then Just (digitToInt $ str !! 2)
                          else Nothing
    , groupSubgroup = if length str > 3
                          then Just (char2Subgroup $ str !! 3)
                          else Nothing
    , groupRegistrations = Nothing
    }
  where
    str2Degree str' = case str' of
                         "IB" -> IB
                         "IC" -> IC
                         "IF" -> IF
                         "GO" -> GO
                         "IG" -> IG
                         "IN" -> IN
                         "IS" -> IS
                         _    -> error $ "unknown group: " ++ str
    char2Subgroup c = case c of
                          'A' -> A
                          'B' -> B
                          'C' -> C
                          _   -> error $ "unknown group: " ++ str

data Registrations = Registrations
    { regsGroup :: String
    , regs      :: M.Map Integer Integer -- Ancode x Sum
    }
  deriving (Show)
