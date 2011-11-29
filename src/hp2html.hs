import Data.Char(isSpace)
import Data.Maybe(mapMaybe)
import Data.Word(Word)
import qualified Data.Map as Map
import Numeric(showFFloat)
import System.FilePath((</>), replaceExtension)
import System.Environment
import Paths_hp2html

data Profile = Profile
  { job         :: String
  , date        :: String
  , sampleUnit  :: String
  , valueUnit   :: String
  , samples     :: [Sample]
  } deriving Show

data Sample = Sample
  { start       :: Double
  , end         :: Double
  , sampleData  :: [(String, Word)]
  } deriving Show




main :: IO ()
main =
  do args <- getArgs
     dataDir <- getDataDir
     let file = case args of
                  []    -> Nothing
                  x : _ -> Just x

     txt <- case file of
              Nothing -> getContents
              Just f  -> readFile f

     let js = toFlot $ pProfile emptyProfile $ map pKeyVal $ lines txt
         html = htmlWrapper dataDir js

     case file of
       Nothing -> putStrLn html
       Just f  -> writeFile (replaceExtension f "html") html


--------------------------------------------------------------------------------
-- flot

htmlWrapper :: FilePath -> String -> String
htmlWrapper dataPath jsData = unlines $
  [ "<html>"
  , "<head>"
  , css    "hp2html.css"
  , script "jquery-1.7.1.min.js"
  , script "flot/jquery.flot.min.js"
  , script "flot/jquery.flot.stack.min.js"
  , script "flot/jquery.flot.resize.min.js"
  , script "flot/jquery.flot.selection.min.js"
  , script "hp2html.js"
  , "  <script>"
  ] ++ [jsData] ++
  [ "  </script>"
  , "</head>"
  , "<body>"
  , "<div class='toolbar'>"
  , "  <span class='stackControls'>"
  , "    <label for='unstack'>not stacked</label>"
  , "    <input type='radio' name='view' id='unstack' value='notStack'"
  , "                                                             checked />"
  , "    <label for='stack'>stacked</label>"
  , "    <input type='radio' name='view' id='stack' value='stack' />"
  , "  </span>"
  , "  <span id='point'>(hover over curves)</span>"
  , "</div>"
  , "<div id='placeholder'></div>"
  , "</body>"
  , "</html>"
  ]
  where
  css x    = "<link rel='stylesheet' type='text/css' href=" ++ file x ++ ">"
  script x = "<script src=" ++ file x ++ "></script>"
  file x   = "'" ++ concatMap esc (dataPath </> "data" </> x) ++ "'"
  esc '\'' = "\\'"
  esc x    = [x]




toFlot :: Profile -> String
toFlot profile = "function getData() { return " ++ js ++ "; }"
  where
  js = list (map toJS1 flotSeries)
  toJS1 (l,ds) = "{ label: " ++ show l ++ ", data:\n" ++
                                            list (map point ds) ++ "}"

  list [] = "[]"
  list [x]  = "[" ++ x ++ "]"
  list (x:xs) = unlines $ ("[ " ++ x) : [ ", " ++ y | y <- xs ] ++ [ "]" ]

  point (x,y) = "[" ++ showFFloat (Just 2) x ("," ++ show y ++ "]")

  endOfTime = case samples profile of
                [] -> 0
                xs -> end (last xs)


  flotSeries = do (l, m) <- Map.toList
                          $ foldr addSample Map.empty
                          $ samples profile
                  return (l, Map.toList
                           $ Map.insertWith (\_ x -> x) 0 0
                           $ Map.insertWith (\_ x -> x) endOfTime 0
                           $ m)


  addSample :: Sample -> Map.Map String (Map.Map Double Word) ->
                         Map.Map String (Map.Map Double Word)
  addSample sample series = foldr (addSampleData (end sample)) series
                          $ sampleData sample

  addSampleData when (key,val) =
    Map.insertWith (Map.unionWith (+)) key (Map.singleton when val)


--------------------------------------------------------------------------------
-- Parsing

emptyProfile :: Profile
emptyProfile = Profile
  { job         = ""
  , date        = ""
  , sampleUnit  = ""
  , valueUnit   = ""
  , samples     = []
  }

pNumber :: Read a => String -> Maybe a
pNumber cs = case reads cs of
               [(n,ds)] | all isSpace ds -> Just n
               _                         -> Nothing

pKeyVal :: String -> (String,String)
pKeyVal l = case break isSpace l of
              (as,bs) -> (as, dropWhile isSpace bs)


pSampleData :: (String,String) -> Maybe (String,Word)
pSampleData (k,v) = do n <- pNumber v
                       return (k,n)

pSamples :: [(String,String)] -> [Sample]
pSamples ls =
  case dropWhile (("BEGIN_SAMPLE" /=) . fst) ls of
    [] -> []
    (_,sTxt) : ls1 ->
      case break (("END_SAMPLE" ==) . fst) ls1 of
        (ds,(_,eTxt) : ls2) ->
           case (pNumber sTxt, pNumber eTxt) of
             (Just s, Just e) ->    -- ignore malformed sample entries.
                Sample { start = s
                       , end = e
                       , sampleData = mapMaybe pSampleData ds
                       }
                : pSamples ls2
             _ -> pSamples ls2    -- ignire malformed samples
        _ -> [] -- ignore unterminated samples


pProfile :: Profile -> [(String,String)] -> Profile
pProfile p ls =
  case ls of
    (key,val) : ls1
      | key == "JOB"          -> pProfile p { job        = val } ls1
      | key == "DATE"         -> pProfile p { date       = val } ls1
      | key == "SAMPLE_UNIT"  -> pProfile p { sampleUnit = val } ls1
      | key == "VALUE_UNIT"   -> pProfile p { valueUnit  = val } ls1
    _ -> p { samples = pSamples ls }




