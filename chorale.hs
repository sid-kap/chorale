import Control.Applicative
import Data.List (permutations, nub, intersperse, transpose, inits, group, minimumBy)
import Data.Char (toLower, isUpper, isLetter)
import System.Environment (getArgs)
import Debug.Trace (trace)

type Pitch = Integer
type Interval = Integer
type Note = Integer
type Chord = (Pitch, ChordType)

type ChordType = [Interval]
type PitchedChord = [Pitch]

majorChord :: ChordType
majorChord = [0,4,7]

minorChord :: ChordType
minorChord = [0,3,7]

dimChord :: ChordType
dimChord   = [0,3,6]

augChord :: ChordType
augChord   = [0,4,8]

maj7Chord :: ChordType
maj7Chord  = [0,4,7,11]

dom7Chord :: ChordType
dom7Chord  = [0,4,7,10]

min7Chord :: ChordType
min7Chord  = [0,3,7,10]

toPitchedChord :: Chord -> PitchedChord
toPitchedChord (pitch, chordType) = map (+pitch) chordType

-- c2 = 0
type Range = (Pitch, Pitch)

bassRange    = (9, 28) -- (4, 28)
tenorRange   = (12, 26) -- (12, 31)
altoRange    = (22, 36) -- (19, 36)
sopranoRange = (24, 40) --(24, 43)

ranges :: [Range]
ranges = [bassRange, tenorRange, altoRange, sopranoRange]

ascending :: PitchedChord -> Bool
ascending (x:xs)
 | null xs     = True
 | x < head xs = ascending xs
 | otherwise   = False

withinOctaveGaps :: PitchedChord -> Bool
withinOctaveGaps (x:xs)
 | null xs                              = True
 | x < (head xs) && x + 12 >= (head xs) = True
 | otherwise                            = False

isValid :: PitchedChord -> Bool
isValid xx@(x:xs) = (x < (head xs)) && withinOctaveGaps xs

inRangeChords :: PitchedChord -> [PitchedChord]
inRangeChords pChord = filter isValid $ concatMap (combine.process) producedChords
    where process = zipWith clamp ranges
          producedChords = shuffle pChord

clamp :: Range -> Note -> [Pitch]
clamp range@(lo, hi) x
 | x < lo    = clamp range (x+12)
 | x > hi    = clamp range (x-12)
 | otherwise = takeWhile (>=lo) [x,x-12..] ++ takeWhile (<=hi) [x+12, x+24..]

shuffle :: [a] -> [[a]]
shuffle xx@(x:xs) = permutations $ x:xx

shuffle1 :: Eq a => [a] -> [[a]]
shuffle1 xs = nub $ concatMap (\x->permutations $ x:xs) xs

combine :: [[a]] -> [[a]]
combine [] = [[]]
combine (x:xs) = (:) <$> x <*> (combine xs)


-- Functions for transitions between chords

badTransition :: PitchedChord -> PitchedChord -> Bool
badTransition [] b = False
badTransition  a b = parallel (a,b) 7 || parallel (a,b) 12 -- || jumpsMoreThan 7 (a,b))

bestTransition :: PitchedChord -> [PitchedChord] -> PitchedChord
bestTransition previousChord chords = snd $ minimumBy compareScores $ map ((,) previousChord) chords
    where compareScores a b = compare (transitionScore a) (transitionScore b)


--jumpsMoreThan :: Interval -> (PitchedChord, PitchedChord) -> Bool
--jumpsMoreThan interval (a,b) = any tooBigJumps pitchPairs
--    where pitchPairs = zip a b
--          tooBigJumps (a,b) = abs (b - a) > interval

parallel :: (PitchedChord, PitchedChord) -> Interval -> Bool
parallel (a,b) interval = any parallelPitches pitchPairs
    where pitchPairs = pairs $ zip a b
          parallelPitches ((a,a'),(b,b')) = b-a == interval && a'-a == b'-b


pairs :: Eq a => [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = pairsOf x xs ++ pairs xs
    where pairsOf x xs = map ( (,) x ) xs


-- transitionScore (a,b) | trace ("scores " ++ show jScore ++ " " ++ show oScore) False = jScore + oScore
--    where jScore = jumpScore (a,b)
--          oScore = opennessScore (a,b)
transitionScore :: (PitchedChord, PitchedChord) -> Float
transitionScore (a,b) = jScore - oScore
    where jScore = jumpScore (a,b)
          oScore = opennessScore (a,b)

jumpScore :: (PitchedChord, PitchedChord) -> Float
jumpScore (a,b) = fromIntegral $ sum $ map (\(x,y)->square (x-y)) pitchPairs
    where pitchPairs = zip a b

opennessScore :: (PitchedChord, PitchedChord) -> Float
opennessScore (_,b) = transformOpenness $ map distance pitchPairs
    where pitchPairs = zip b (tail b)
          distance (a,b) =  (a-b)

transformOpenness :: [Integer] -> Float
transformOpenness xInts = (*0.2) $ sum $ map (\x-> square (x-5) ) xs
    where xs = map fromIntegral xInts

square :: Num a => a -> a
square x = x * x

-- Main functions

main = do
    [file] <- getArgs
    text <- readFile file
    putStrLn $ printProgression $ makeProgression [] $ map parseChord (words text)

selectChord :: Chord -> PitchedChord
selectChord = head . chordOptions

chordOptions :: Chord -> [PitchedChord]
chordOptions = inRangeChords . toPitchedChord

makeProgression :: PitchedChord -> [Chord] -> [PitchedChord]
makeProgression lastChord (chord:chords) = selectedChord:(makeProgression selectedChord chords)
    where selectedChord = bestTransition lastChord $ filter (not.badTransition lastChord) (chordOptions chord)
makeProgression lastChord [] = []

-- Test functions
test :: [PitchedChord]
test = nub $ inRangeChords $ toPitchedChord (0, majorChord)

test1 :: String
test1 = concatMap (\x-> "<<" ++ (concat $ intersperse " " $ map toLilypond x) ++ ">> ") test

test2 :: String
test2 = printProgression test




-- Functions for converting data to Lilypond output
toLilypond :: Integer -> String
toLilypond pitch = note ++ (mRanges !! octave) ++ "2"
    where mRanges = ",":(inits $ repeat '\'')
          (octave, rem) = divMod (fromIntegral pitch) 12
          note = ["c", "cis", "d", "dis", "e", "f", "fis", "g",
                  "gis", "a", "ais", "b"] !! rem

chordsToVoices :: [PitchedChord] -> [String]
chordsToVoices xs = map (\x -> "{" ++ (concat $ intersperse " " $ map toLilypond x) ++ "}\n") (transpose xs)

voicedChordsToLilypondSplits :: [String] -> String
voicedChordsToLilypondSplits xs = concat $ zipWith (++) ["bassMusic = ","tenorMusic = ","altoMusic = ","sopMusic = "] xs

printProgression :: [PitchedChord] -> String
printProgression = voicedChordsToLilypondSplits . chordsToVoices


-- Functions for converting string input to data                  
parseChord :: String -> Chord
parseChord str = (pitch, chordType)
    where pitch = parseChordPitch (map toLower str)
          chordType = parseChordType str

parseChordPitch :: String -> Integer
parseChordPitch str
 | str == "i"    = 0
 | str == "iib"  = 1
 | str == "ii"   = 2
 | str == "iiib" = 3
 | str == "iii"  = 4
 | str == "iv"   = 5
 | str == "vb"   = 6
 | str == "v"    = 7
 | str == "vib"  = 8
 | str == "vi"   = 9
 | otherwise    = error ("Bad Chord: " ++ str)
    where lowerStr = map toLower str

parseChordType :: String -> ChordType
parseChordType str 
 | length (group cases) == 1 = if (and cases) then majorChord else minorChord
 | otherwise                 = error ("Chord " ++ str ++ " should be written in all uppercase or all lowercase.")
    where cases = map (\c -> isLetter c && isUpper c) $ filter (/= 'b') str
