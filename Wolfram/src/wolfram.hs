--
-- EPITECH PROJECT, 2021
-- B-FUN-400-STG-4-1-wolfram-auguste.thomann
-- File description:
-- wolfram
--

import System.Environment
import System.Exit
import Data.Char

rule30 :: Char -> Char -> Char -> String
rule30 '*' ' ' ' ' = "*"
rule30 ' ' '*' '*' = "*"
rule30 ' ' '*' ' ' = "*"
rule30 ' ' ' ' '*' = "*"
rule30 a b c       = " "

rule90 :: Char -> Char -> Char -> String
rule90 '*' '*' ' ' = "*"
rule90 '*' ' ' ' ' = "*"
rule90 ' ' '*' '*' = "*"
rule90 ' ' ' ' '*' = "*"
rule90 a b c       = " "

rule110 :: Char -> Char -> Char -> String
rule110 '*' '*' ' ' = "*"
rule110 '*' ' ' '*' = "*"
rule110 ' ' '*' '*' = "*"
rule110 ' ' '*' ' ' = "*"
rule110 ' ' ' ' '*' = "*"
rule110 a b c       = " "

checkChar :: String -> Int -> Char
checkChar str pos | pos < 0 || pos >= length str = ' '
checkChar str pos = (str!!pos)

checkNumber :: String -> String -> Int -> IO()
checkNumber "--move" str n | (str!!0) == '-' = move str n
checkNumber prec str n = noMove str n

checkArgs :: Int -> Int -> [String] -> IO()
checkArgs end i args =
    loopCheckArgs end i args >>
    loopCheckRepeat "--rule" 0 end i args >>
    loopCheckRepeat "--start" 0 end i args >>
    loopCheckRepeat "--lines" 0 end i args >>
    loopCheckRepeat "--window" 0 end i args >>
    loopCheckRepeat "--move" 0 end i args

checkRule :: String -> Int -> IO()
checkRule str n | str /= "--rule" && str /= "--move" && str /= "--window" &&
    str /= "--start" && str /= "--lines"
        = failure
 (" " ++ show n ++ "-->\"" ++ str ++ "\" is an invalid instruction")
checkRule str n = return()

searchOtherArgs :: String -> Int -> Int -> Int -> [String] -> Int
searchOtherArgs search def end i args | i == end = def
searchOtherArgs search def end i args | (args!!i) == search && i == (end-1) = 66766985
searchOtherArgs search def end i args | (args!!i) /= search && i == (end-1) = def
searchOtherArgs search def end i args | (args!!i) == search = read(args!!(i+1))
searchOtherArgs search def end i args = searchOtherArgs search def end (i+1) args

getArguments :: Int -> [String] -> IO()
getArguments end args = do
    let rule = getRule end 0 args
    ruleErrorsCase rule
    let start = searchOtherArgs "--start" 0 end 0 args
    otherErrors start "start"
    let line = searchOtherArgs "--lines" (-2) end 0 args
    otherErrors line "lines"
    let window = searchOtherArgs "--window" 80 end 0 args
    otherErrors window "window"
    let move = searchOtherArgs "--move" 0 end 0 args
    otherErrors move "move"
    wolfram rule start line window move "*" (-1) 1 0

getNextRule :: Int -> [String] -> Int
getNextRule i args = do
    let tab = ["30", "90", "110"]
    validRuleCase 0 (len tab) (args!!(i+1)) tab

getRule :: Int -> Int -> [String] -> Int
getRule end i args | i == end = -1
getRule end i args | (args!!i) == "--rule" && i == (end-1) = -2
getRule end i args | (args!!i) /= "--rule" && i == (end-1) = -1
getRule end i args | (args!!i) == "--rule" = getNextRule i args
getRule end i args = getRule end (i+1) args

ruleErrorsCase :: Int -> IO()
ruleErrorsCase (-1) = failure "No \"--rule\" detected !"
ruleErrorsCase (-2) = failure "It is not any value after \"--rule\" !"
ruleErrorsCase (-3) = failure "The value after \"--rule\" must be 30, 90 or 110 !"
ruleErrorsCase rule = putStr ""

validRuleCase :: Int -> Int -> String -> [String] -> Int
validRuleCase i end search tab | i == (end-1) && (tab!!i) /= search = -3
validRuleCase i end search tab | (tab!!i) == search = read (tab!!i)
validRuleCase i end search tab = validRuleCase (i+1) end search tab

charToString :: Char -> String
charToString c = [c]

numberCase :: String -> Bool
numberCase ""  = False
numberCase "." = False
numberCase xs  =
    case dropWhile isDigit xs of
        ""       -> True
        ('.':ys) -> all isDigit ys
        _        -> False

isPair :: Int -> Bool
isPair n = mod n 2 == 0

len :: (Num b) => [a] -> b
len [] = 0
len xs = sum [1 | _ <- xs]

loop :: Int -> Int -> String -> Int -> String -> Int -> String
loop a z second pos firstPrint rule | a == z = second
loop a z second pos firstPrint 90 =
    loop (a+1) z (second++rule90 (checkChar firstPrint (pos-1))
    (checkChar firstPrint pos) (checkChar firstPrint (pos+1)))
    (pos+1) firstPrint 90
loop a z second pos firstPrint 30 =
    loop (a+1) z (second++rule30 (checkChar firstPrint (pos-1))
    (checkChar firstPrint pos) (checkChar firstPrint (pos+1)))
    (pos+1) firstPrint 30
loop a z second pos firstPrint rule =
    loop (a+1) z (second++rule110 (checkChar firstPrint (pos-1))
    (checkChar firstPrint pos) (checkChar firstPrint (pos+1)))
    (pos+1) firstPrint rule

loopCheckArgs :: Int -> Int -> [String] -> IO()
loopCheckArgs end i args | i == end = return()
loopCheckArgs end i args | isPair i = checkRule (args!!i) i
loopCheckArgs end i args = checkNumber (args!!(i-1)) (args!!i) i

loopCheckRepeat :: String -> Int -> Int -> Int -> [String] -> IO()
loopCheckRepeat search count end i args | i == (end+1) = return()
loopCheckRepeat search count end i args | count == 2 =
    failure ("The argument \"" ++ search ++ "\" is present more than one time")
loopCheckRepeat search count end i args | i < end && (args!!i) == search =
    loopCheckRepeat search (count+1) end (i+1) args
loopCheckRepeat search count end i arg = loopCheckRepeat search count end (i+1) arg

wolfram :: Int -> Int -> Int -> Int -> Int -> String -> Int -> Int -> Int ->IO()
wolfram rule start 0 window move firstPrint pos z i = return()
wolfram rule start line window move firstPrint pos z i | i >= start = do
    let secondPrint = (loop 0 z "" (-1) firstPrint rule)
    let specialNb = ((window `div` 2)-(length secondPrint)`div`2) + move
    putStrLn (specialPrint secondPrint window move specialNb)
    wolfram rule start (line-1) window move secondPrint (-1) (z+2) (i+1)
wolfram rule start line window move firstPrint pos z i = do
    let secondPrint = (loop 0 z "" (-1) firstPrint rule)
    wolfram rule start line window move secondPrint (-1) (z+2) (i+1)

fillSpaces :: Int -> String -> Int -> String
fillSpaces i str window | i == window = str
fillSpaces i str window = fillSpaces (i+1) (str++" ") window

otherString :: Int -> Int -> String -> String
otherString start end text = take (end - start) (drop start text)

specialNbSup :: Int -> Int -> String -> String
specialNbSup specialNb window str | specialNb > window = fillSpaces 0 "" window
specialNbSup specialNb window str =
    fillSpaces 0 "" specialNb ++ otherString 0 (window-specialNb) str ++
    fillSpaces 0 "" (window-(length (fillSpaces 0 "" specialNb ++
    otherString 0 (window-specialNb) str)))

specialNbInf :: Int -> Int -> String -> String
specialNbInf specialNb window str | length str <= (-specialNb) =
    fillSpaces 0 "" window
specialNbInf specialNb window str |
    length (otherString (-specialNb) ((-specialNb)+window) str) < 80 =
    otherString (-specialNb)
    ((-specialNb)+window) str++fillSpaces 0 ""
    (window-length (otherString (-specialNb) ((-specialNb)+window) str))
specialNbInf specialNb window str =
    otherString (-specialNb) ((-specialNb)+window) str

specialPrint :: String -> Int -> Int -> Int -> String
specialPrint str window move specialNb | specialNb >= 0 =
    specialNbSup specialNb window str
specialPrint str window move specialNb = specialNbInf specialNb window str

failure :: String -> IO()
failure str = putStrLn str >>
    exitWith (ExitFailure 84)

otherErrors :: Int -> String -> IO()
otherErrors 66766985 searched  =
    failure ("It is not any value after \"--" ++ searched ++ "\" !")
otherErrors value searchedelse = putStr ""

move :: String -> Int -> IO()
move str n | numberCase(removeMoins str) = return()
move str n =
 failure ("The arg " ++ show n ++ "-->\"" ++ str ++ "\" is an invalid value !")

noMove :: String -> Int -> IO()
noMove str n | numberCase str = return()
noMove str n =
 failure ("The arg " ++ show n ++ "-->\"" ++ str ++ "\" is an invalid value !")

removeMoins :: String -> String
removeMoins str = [ x | x <- str, not (x `elem` "-") ]

main :: IO()
main = do
    args <- getArgs
    let end = (len args) :: Int
    if end < 2
        then failure "It must be an \"--rule\" followed by 30, 90 or 110 number"
    else checkArgs end 0 args
    getArguments end args
    exitWith ExitSuccess