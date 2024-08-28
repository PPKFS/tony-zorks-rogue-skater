module Main where

import TZRS.Prelude hiding ((.=))

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Graph
import qualified Data.Set as S
import qualified GHC.List as L

(!!) :: KM.KeyMap Value -> Key -> Value
(!!) m k = fromMaybe (error "b") $ m KM.!? k

toObj :: Value -> Object
toObj (Object o) = o
toObj _ = error "a"

type QuestDatabase = Map Text Quest

data Quest = Quest
  { properties :: Value
  , isChapterBreak :: Bool
  , questId :: Int
  , rewards :: Value
  , name :: Text
  , tasks :: Value
  , prereqs :: [Int]
  } deriving stock (Eq, Ord, Show, Generic)
{-
  "questLines:9": {
    "0:10": {
      "lineID:3": 0,
      "order:3": 0,
      "properties:10": {
        "betterquesting:10": {
          "bg_image:8": "",
          "bg_size:3": 256,
          "desc:8": "",
          "icon:10": {
            "Count:3": 1,
            "Damage:2": 0,
            "OreDict:8": "",
            "id:8": "avaritia:cosmic_meatballs"
          },
          "name:8": "Dimensional Ascension",
          "visibility:8": "NORMAL"
        }
      },
      "quests:9": {
        "0:10": {
          "id:3": 0,
          "sizeX:3": 24,
          "sizeY:3": 24,
          "x:3": 0,
          "y:3": -12
        },
-}
data QuestLine = QL
  { lineId :: Int
  , order :: Int
  , properties :: Value
  , quests :: Map Text QuestLineQuest
  } deriving stock (Eq, Ord, Show, Generic)

instance FromJSON QuestLine where
  parseJSON = withObject "ql" $ \o -> do
    li <- o .: "lineID:3"
    or' <- o .: "order:3"
    prop <- o .: "properties:10"
    qs <- o .: "quests:9"
    pure $ QL li or' prop qs

instance ToJSON QuestLine where
  toJSON ql = object
    [ "lineID:3" .= lineId ql
    , "order:3" .= order ql
    , "properties:10" .= view #properties ql
    , "quests:9" .= quests ql
    ]
data QuestLineQuest = QLQ
  { qlqId :: Int
  , sizeX :: Int
  , sizeY :: Int
  , x :: Int
  , y :: Int
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON QuestLineQuest where
  toJSON qlq = object
    [ "id:3" .= qlqId qlq
    , "sizeX:3" .= sizeX qlq
    , "sizeY:3" .= sizeY qlq
    , "x:3" .= x qlq
    , "y:3" .= y qlq
    ]
instance FromJSON QuestLineQuest where
  parseJSON = withObject "qlq" $ \o -> do
    qlqId <- o .: "id:3"
    sx <- o .: "sizeX:3"
    sy <- o .: "sizeY:3"
    x <- o .: "x:3"
    y <- o .: "y:3"
    pure $ QLQ qlqId sx sy x y
data QuestFile = QF
  { questDatabase :: QuestDatabase
  , questLines :: Map String QuestLine
  } deriving stock (Eq, Ord, Show, Generic)

data QF2 = QF2
  { questDb :: Value
  , questL :: Map String QuestLine

  }

instance FromJSON QF2 where
  parseJSON = withObject "qdb" $ \o -> do
    qd <- o .: "questDatabase:9"
    ql <- o .: "questLines:9"
    pure $ QF2 qd ql

instance ToJSON QF2 where
  toJSON qf = object
    [ "questDatabase:9" .= questDb qf
    , "questLines:9" .= questL qf
    ]

instance FromJSON QuestFile where
  parseJSON = withObject "qdb" $ \o -> do
    qd <- o .: "questDatabase:9"
    ql <- o .: "questLines:9"
    pure $ QF qd ql

instance FromJSON Quest where
  parseJSON = withObject "quest" $ \o -> do
    qp <- o .: "properties:10"
    questId <- o .: "questID:3"
    r <- o .: "rewards:9"
    t <- o .: "tasks:9"
    pr <- o .: "preRequisites:11"
    bq <- (toObj qp) .: "betterquesting:10"
    n <- (toObj bq) .: "name:8"
    isChBr <- (isInfixOf "Welcome to chapter ") <$> (toObj bq .: "desc:8")
    pure $ Quest qp isChBr questId r n t pr

main :: IO ()
main = do
  (v :: Either String QuestFile) <- eitherDecodeFileStrict' ("DefaultQuests.json")
  v2 <- case v of
    Left err -> error (toText err)
    Right x -> return x
  let mainQs = S.fromList $ (questLines v2) M.! "0:10" & quests & M.elems & map qlqId
  let filteredQuests = filter (\x -> questId x `S.member` mainQs) (M.elems $ questDatabase v2)
  let chapterQs = filter (\x -> isChapterBreak x && questId x `S.member` mainQs) (M.elems $ questDatabase v2)
  let (g, nFromVert, vertFromK) = graphFromEdges (map (\q -> (q, questId q, prereqs q)) filteredQuests)
  let (chapterG, chapterNFromVert, chapVertFromK) = graphFromEdges (map (\q -> (q, questId q, prereqs q)) chapterQs)
  let flippedG = topSort $ transposeG g
  let flippedChapterG = topSort $ transposeG chapterG
  print (map (\v' -> let x = nFromVert v' in view (_1 % #name) x) flippedG)
  print (map (\v' -> let x = nFromVert v' in view (_1 % #name) x) flippedChapterG)
  mapM_ (\q -> do
    print (chapterNFromVert q)
   ) flippedChapterG
  print (length filteredQuests)
  let reachableBy = map (\x -> (questId x, filter (\y -> path (transposeG g) (fromMaybe (error "") $ vertFromK (questId x)) (fromMaybe (error "") $ vertFromK (questId y)) ) filteredQuests )) chapterQs
      sortedReach = sortBy (\a b -> compare (length $ snd a) (length $ snd b)) reachableBy
      reachWithoutPrior = foldl'
        (\(acc, seen) (qi, x) -> let newly = filter (\y -> not $ (questId y) `S.member` seen) x in ((qi, newly):acc, S.union seen (S.fromList $ map questId newly))) ([], S.empty) sortedReach
  mapM_ (\x -> print $ (second length) x) (reverse $ fst reachWithoutPrior)
  let newQuestLines = M.fromList $ zipWith (\i (chapter, qs) -> (show i <> ":10", QL
          { lineId = i
          , order = i
          , properties = object
            [ "betterquesting:10" .= object
              [ "bg_image:8" .= String "",
                "bg_size:3" .= Number 256,
                "desc:8" .= String "",
                "icon:10" .= object
                  [ "Count:3" .= Number 1,
                    "Damage:2" .= Number 0,
                    "OreDict:8" .= String "",
                    "id:8" .= String "avaritia:cosmic_meatballs"
                ]
              ]
            , "name:8" .= String (show i),
              "visibility:8" .= String "NORMAL"
            ]
          , quests = M.fromList $ zipWith (\i' q ->
            (show i' <> ":10", QLQ
              { qlqId = questId q
              , sizeX = 24
              , sizeY = 24
              , x = 24 * i'
              , y = 0

              })) [0..] qs
          })) [15..] (reverse $ fst reachWithoutPrior)
  let newQFile = M.union (questLines v2) newQuestLines


  (v3 :: Either String QF2) <- eitherDecodeFileStrict' ("DefaultQuests.json")
  v4 <- case v3 of
    Left err -> error (toText err)
    Right x -> return x
  encodeFile "DefaultQuests2.json" (v4 { questL = newQFile})
