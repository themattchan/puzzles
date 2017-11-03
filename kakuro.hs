{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables,
             UndecidableInstances, OverlappingInstances,
             FlexibleInstances, TypeSynonymInstances
#-}
import Control.Monad
import Data.List
import Data.List.Split
import Data.Word
import System.Random

-- puzzle format:
-- each line is a row
-- cells are vertical bar separated
-- uninitialised cells are 0
-- constraint cells are n\m,  0 for no constraint
--   0\0 for nonfillable

newtype Blank = Blank { blankSymb :: String }
  deriving (Eq, Show)

type Puzzle = [[Cell]]
data Cell = White Blank
          | Black Int
          -- ^ solved
          | Grey (Maybe Int) (Maybe Int)
          -- ^ vertical, horizontal
          deriving (Eq, Show)

-- probably won't get a collision
makeBlank :: IO Blank
makeBlank = do
  i :: Word16 <- randomIO
  return $ Blank $ "b"++show i

readPuzzleRaw :: IO [String]
readPuzzleRaw = lines . filter (/= ' ') <$> getContents

parseKakuro :: [String] -> IO Puzzle
parseKakuro = traverse (traverse parseCell . splitOn "|")
  where
    parseCell s = case map read $ splitOn "\\" s of
      [x] | x == 0 -> White <$> makeBlank -- single cells should always be 0
      [x,y] -> pure $ Grey (maybeConstraint x) (maybeConstraint y)
      cell -> error $ "parseCell " ++ s

    maybeConstraint = mfilter (/= 0) . pure

nonFillable = Grey Nothing Nothing

data Constraint = Constraint Int [Blank]

genConstraints :: Puzzle -> [Constraint]
genConstraints p = go True p ++ go False (transpose p)
  where
    getConstraint horiz (Grey x y)
      | horiz = y
      | otherwise = x

    go dir = foldMap (oneLine dir)
      where
        -- TODO TODO
        oneLine = undefined

class MyShow a where
    myShow :: a -> String
instance MyShow String where
    myShow = id
instance Show a => MyShow a where
    myShow = show

data Sexpr = forall a. MyShow a => Sexpr [a]

instance Show Sexpr where
  show (Sexpr xs) = "("++ unwords (map myShow xs) ++ ")"

allVariables :: Puzzle -> [Sexpr]
allVariables = foldMap (foldMap getVar)
  where
    getVar (White (Blank v)) = [intVar v]
    getVar _ = []

intVar :: String -> Sexpr
intVar v = Sexpr ["declare-const", v, "Int"]

-- compile to z3. should i write this in racket instead?
-- https://rise4fun.com/Z3/tutorial/guide
