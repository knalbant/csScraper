import System.Directory
import Data.List 
import Data.Char

courseDirectory = "exp_course_desc/"


getFileNames = getDirectoryContents courseDirectory >>= return . filter (".html" `isSuffixOf`) 
  
getBeginningOfPreregs = dropWhile ( not . isInfixOf "Prerequisite" ) . words 

dropUntilCredit = takeWhile (not . isInfixOf "Credit") 

getPrereqLine = dropUntilCredit . getBeginningOfPreregs 

takeUntilNotECS = takeWhile helper 
  where helper s = let st = map toUpper (filter isAlphaNum s) 
                   in  st == "OR" || all isDigit (init st) || st == "ECS" || "COURSE" `isPrefixOf` st 
        
dropUntilECSCourses = dropWhile (not . helper)
  where helper s = let st = map toUpper s
                   in  "COURSE" `isPrefixOf` st || st == "ECS" 

getPrereqNums = filterCourseNums . cleanedUp

  where relevantCourses = takeUntilNotECS . dropUntilECSCourses . getPrereqLine
        cleanedUp       = map (filter isAlphaNum ) . relevantCourses
        filterCourseNums = filter (all isDigit . init)
--        validCourseIDChars s = isDigit s || s == 'A' || s == 'B' || s == 'C'

makeCourseLine courseNum prereqs = courseNum ++ ": " ++ (intercalate " " prereqs)

courseNamesFromFileNames = map (takeWhile (/='.'))


printOneCourseLine (name, path) = do html <- readFile path
                                     let prereqs = getPrereqNums html
                                     let cleanedPrereqs = map ( takeWhile (\s -> isDigit s || s == 'A' || s == 'B' || s == 'C' || s == 'D') ) prereqs
                                     putStrLn $ makeCourseLine name cleanedPrereqs

main = do
  htmls <- getFileNames
  let courseNames = courseNamesFromFileNames htmls

  let filepaths = map (courseDirectory ++) htmls  

  let namesAndPaths = zip courseNames filepaths 

  mapM printOneCourseLine namesAndPaths 
  
