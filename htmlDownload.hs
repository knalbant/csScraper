import Network.HTTP 
import Data.List 
import Data.List.Split 
import System.Directory 
import Data.Char 
import Text.Read
import Data.Maybe
import Control.Arrow

courseURLs           = "http://www.cs.ucdavis.edu/courses/descriptions/"
graduateCourseNumber = 200
courseDirectory      = "exp_course_desc"

extractLinks :: String -> [String]
extractLinks = cleanup . filterRefs . splitOnTags  
  where splitOnTags     = splitOn " " . unwords . splitOn ">" . unwords . splitOn "<"
        filterRefs      = filter ("href" `isPrefixOf`)
        cleanup         = map ( (!! 1)  . splitWhen (orEqual '\"' '\'' ))
        orEqual o1 o2 s = s == o1 || s == o2

filterECSLinks :: [String] -> [String] 
filterECSLinks = filter ("ecs" `isInfixOf`)  

getHTMLBody:: String -> IO String
getHTMLBody url = do 
  response <- simpleHTTP (getRequest url)
  getResponseBody response

getECSLinks :: String -> [String] 
getECSLinks = filterECSLinks . extractLinks 

numberStringToInt :: String -> Maybe Int 
numberStringToInt = readMaybe . cleanNumber
  where cleanNumber []     = []
        cleanNumber (x:xs) = if isDigit x then x : cleanNumber xs else cleanNumber xs

getCourseInt :: String -> Int 
getCourseInt = fromJustList . filter isJust . maybeIntList  
  where maybeIntList = map numberStringToInt . splitOn "-" 
        fromJustList [x] = fromJust x

isUndergraduateCourse = (<graduateCourseNumber) . getCourseInt

capitalize :: String -> String 
capitalize = map toUpper 

getCourseTitle s =  capitalize $ addTheL $ (!! 1) $ splitOn "-" s
  where addTheL st = if "89A" `isSuffixOf` capitalize st then st ++ "-L" else st --a hacky fix but a fix  
        
zipURLsAndTitles = map (getCourseTitle &&& id) 

getTitlesAndUrls :: String -> [String] 
getTitlesAndUrls = zipURLsAndTitles . filter isUndergraduateCourse . getECSLinks 

main = do 
  rawHTML <- getHTMLBody courseURLs 
  let titlesAndURLs = getTitlesAndUrls rawHTML

  createDirectoryIfMissing False "exp_course_dec" 


  print titlesAndURLs

  return rawHTML




