module DND
  ( Character(..)
  , ability
  , modifier
  , character
  )
where

import           Test.QuickCheck                ( Gen
                                                , Arbitrary(..)
                                                , arbitrary
                                                , choose
                                                )

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

instance Arbitrary Character where
  arbitrary = do
    s  <- ability
    d  <- ability
    co <- ability
    i  <- ability
    w  <- ability
    ch <- ability
    let h = 10 + modifier co
    return $ Character s d co i w ch h

modifier :: Int -> Int
modifier = floor . flip (/) 2.0 . fromIntegral . subtract 10

ability :: Gen Int
ability = choose (3, 18)

character :: Gen Character
character = arbitrary
