import Network.HTTP
import Data.List 
import Data.List.Split
import System.Directory 
import Data.Char 


courseURLs = "http://www.cs.ucdavis.edu/courses/descriptions/"


extractLinks :: String -> [String]
extractLinks = cleanup . filterRefs . splitOnTags  
  where splitOnTags     = splitOn " " . unwords . splitOn ">" . unwords . splitOn "<"
        filterRefs      = filter ("href" `isPrefixOf`)
        cleanup         = map ( (!! 1)  . splitWhen (orEqual '\"' '\'' ))
        orEqual o1 o2 s = s == o1 || s == o2


filterECSLinks :: [String] -> [String] 
filterECSLinks = filter ("ecs" `isInfixOf`)  

getCourseHTML :: IO String
getCourseHTML = do 
  response <- simpleHTTP (getRequest courseURLs)
  getResponseBody response

getECSLinks :: String -> [String] 
getECSLinks = filterECSLinks . extractLinks 


numberStringToInt :: String -> Int 
numberStringToInt = read . cleanNumber

  where cleanNumber []     = []
        cleanNumber (x:xs) = if isDigit x then x : cleanNumber xs else cleanNumber xs



main = do 
 
  rawHTML <- getCourseHTML

  let ecsLinks = getECSLinks rawHTML

  mapM_ print ecsLinks


  return rawHTML
