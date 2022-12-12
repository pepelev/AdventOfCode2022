module Task12 where

import qualified Data.Array as A
import qualified Data.Set as S
import Data.Char (ord)

width = length $ head $ lines $ input
height = length $ lines input

ground = A.listArray (1, height) $ map (A.listArray (1, width)) $ lines input

get (x, y) =
    if 1 <= x && x <= width && 1 <= y && y <= height
    then ground A.! y A.! x
    else '0'

findAll what = filter (\location -> get location == what) [(x, y) | x <- [1..width], y <- [1..height]]
find what = head $ findAll what 

start = find 'S'
end = find 'E'

type Queue a = ([a], [a])
type Point = (Int, Int)

empty = ([], [])

isEmpty ([], []) = True
isEmpty _ = False

top (_, toPull) = head toPull
push a ([], []) = ([], [a])
push a (pushed, toPull) = (a:pushed, toPull)

pull (pushed, [toPull]) = (toPull, ([], reverse pushed))
pull (pushed, toPull)   = (head toPull, (pushed, tail toPull))

moves (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

norm 'E' = 'z'
norm 'S' = 'a'
norm a = a

can a b =
    let a' = norm $ get a
        b' = norm $ get b
    in  ord b' - ord a' <= 1 && b' /= '0'

-- path :: Queue [Point] -> S.Set Point -> [Point]
path ([], []) v = ([], v)
path queue visited =
    let (current@(c:cs), queue') = pull queue
        mv = filter (\x -> S.notMember x visited && can c x) $ moves c
        visited' = S.union visited $ S.fromList mv
        queue'' = foldr push queue' $ map (:current) mv
    in  if get c == 'E'
        then (current, S.empty)
        else path queue'' visited'

answer1 = path (push [start] empty) S.empty

batch n [] = []
batch n list =
    let part = take n list
        rest = drop n list
    in  part : batch n rest

test a@(x, y) = S.member a (snd answer1)

prt =
    let p = [if test (x, y) then 'X' else ' ' | y <- [1..height], x <- [1..width]]
    in  putStrLn $ unlines $ batch width p

--

starts = start : findAll 'a'
startQ = foldr push empty $ map (\x -> [x]) starts
answer2 = path startQ (S.fromList starts)

input = "abaaaaaccccccccaaaaaaaaaaccccaaaaaaaaaccccaacccccccccccccccccccccaaaaaaaaaaaccaaaaaaaaaccccccccccaaaaaaaacaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa\n\
\abaaaaaaaccccccaaaaaaaaaacccccaaaaaacccccaaaacccccccccccccaacccccaaaaaaaaaaaacaaaaaaaaacccccccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccccaaaa\n\
\abacaaaaaccccccccaaaaaacccccccaaaaaacccccaaaacccccccccccccaacccaaaaaaaaaaaaaacaaaaaaaaccccccccccaaaaaaaaaaaaaaaaccccccccccccccccccccccccccaaaccccccccccccccaaaa\n\
\abccaacccccccccccaaaaaaccccccaaaaaaacccccaaaacccaacccccaaaaaaaaaaaaaaaaaaaaacaaaaaaaccccccccccccacaaaaaccaaaaaaaccccccccccccccccccccccccccaaccccccccccccccaaaaa\n\
\abcaaccccccccaaaaaaaaaaccccccaaacaaaccccccccccccaaaaaccaaaaaaaaaaaaaaaaaaaaaccaccaaacccccccccccccccaaaacccaaaaaaccccccccccccccccccccccccccaaacccccccccccccaaaca\n\
\abcccccccccccaaaaaaaaaaaaacccccccccacccccccccccaaaaacccccaaaacaaaaaaaaaaaaccccaaaaaaccccccccaaacccccaaccccaacccccccccccccccccccccccaaaaccaaaccccccccccccccccccc\n\
\abccccccccccaaaaaaccccaaaacccccccccccccccccccccaaaaaaccccaaaaaaaaaaaaaacaaacccaaaaaaaacccccaaaaaacccccccccccccccccccccccccccccccccccaaaaaaaaacccccccccccccccccc\n\
\abacccccccccaaaaaacccccaaacaaacccccccccacccaaccccaaaacccaaacaaaaaaaaaaacccccccaaaaaaaacccccaaaaaaccccccccccccccccccccccccccccccccjjjjjjjaaaaaaaaaccccaacccccccc\n\
\abaccccccccccaaaaaccaaaaaaaaaaccaccccccaacaaacccaaccccccaacccaaaaaaaaaaacccccccaaaaaaacccccaaaaaccccccccccccccccccccccccccccccccijjjjjjjjjhhhhhhhhhcaaaaaaccccc\n\
\abaacccccccccaaaccccaaaaaaaaaaaaaccccccaaaaacccccccccccccccccccccaaaaaaacccccccaaaaaccccccccaaaaacccccccccccccccaaaaccccccccccciijjjjjjjjjhhhhhhhhhhcaaaaaccccc\n\
\abaaccccccccccccccccccaaaaaacaaaaaacccccaaaaaacccccccccccaaacccccaaaccccccccccaaaaaaccccccccaacaacccccccccccccccaaaaccccccccccciiioooooojjhhhhpphhhhhaaaaaccccc\n\
\abacccccccccccccccccccaaaaaaccaaaaacccaaaaaaaacccccccccccaaaaaaccaacccccccccccccccaaccccccccacccccccccccccccccccaaaaccccccccccciiioooooooooopppppphiiaaaaaacccc\n\
\abaccccccccccaaaaaccccaaaaaaaaaaaaccccaaaaaaaacccccccccaaaaaaaaccccccccccccaaaaccccccccccccaaacccccccccccccccccccaaccccccccccciiinnoouuooooopppppppiiaaaaaacccc\n\
\abcccccccccccaaaaaccccaaacaaaaccaacccccccaaccccccccccccaaaaaaaaccccccccccccaaaaccccccccaaacaaacccccccccccccccccccccccccccccccciiinnotuuuuoopuuuupppiiaaacaccccc\n\
\abccccccccccaaaaaaccccaccccccccccccccccccaaccccccccccccaaaaaaacccccccaaacccaaaaccccccccaaaaaaaaaacccaacccccccccccccccccccccccciiinntttuuuuuuuuuuuppiiiaaccccccc\n\
\abaaccccccccaaaaaaccccccccccccccccccaaaacccccccccccccccccaaaaaacccccaaaaccccaaccccccccccaaaaaaaaacccaaacaaacaaaccccccccccaaccciiinntttxxuuuuuuuuuppiiiccccccccc\n\
\abaacccccccccaaaaaccccccccccccccccccaaaaccccccccaccccccccaaaaaacccccaaaaccccccccccccccccccaaaaacccccaaaaaaacaaaacccccccccaaaaiiiinnttxxxxuuyyyuvvppiiiccccccccc\n\
\abaacccccccccaaaccccccccccccccccccccaaaaccaaacaaaacccccccaaccccccccccaaacccccccccccccccccaaaaaaccccccaaaaaacaaaacccccccccaaaaiiinnnttxxxxxxyyyvvppqiiiccccccccc\n\
\abaccccccccccccccccccccaaccccccccccccaacccaaaaaaaacccccccccccccccccccccccccccccccccccaacaaaaaaacccaaaaaaaaccaaaccccccccaaaaahhhinnntttxxxxxyyyvvqqqiiiccccccccc\n\
\abcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaaaaaacaaacccaaaaaaaaaccccccccccccaaaaahhhnnnttttxxxxyyyvvvqqqiiiccccccccc\n\
\SbcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaacccccccacccaaaaaaaaaacccccccccccccaahhhnnntttxxxEzzzyyyvvvqqqjjjcccccccc\n\
\abcccccccccccccccccccaaaaacccccccccccccaaaaaaaaaaaacccccccccccccccccccccccccccccccaaaaaaaccccccccccccaaacaaacccccccccccccahhhmmmtttxxxxxyyyyyyyvvvqqqjjjccccccc\n\
\abccccccccccccccccccccaaaaacccccccccaacaaaaaaaaaaaaccccaccaaaccccccccccccccccccccaaaaaaaaccccccccccccaaacccccccccccccaaccahhhmmmtttxxxyywyyyyyyvvvqqqjjjccccccc\n\
\abccccccccccccccccccccaaaaacccccccccaaaaaaaacaaacccccccaaaaaaccccaccaaaccccccccccaaaaaaaaccccccccccccaacccccccccccccaaaccchhhmmmsssxxwwwyyywyyvvvvqqqjjjccccccc\n\
\abccaacccccccccccccccccccccccccccccccaaaaaaccaaacccccccaaaaaaccccaacaaacccccccccccacaaacccccccccccccccccccccccccaaaaaaaccchhhmmmssssswwwwyywwvvvvvqqqjjjdcccccc\n\
\abccaaaccaaccccccccccccccccccccccccaaaaaaaaccaaccccccccaaaaaaacccaaaaaccccccccccccccaaacccccccccccccccccccccccccaaaaaaaaaahhhmmmmsssssswwywwwrvvqqqqqjjjdddcccc\n\
\abccaaaaaaacccccaaaccccccccccccccccaaaaacaacccccccccccaaaaaaaaccccaaaaaaccccccccaaaccccccccccccccccccccccccccccccaaaaaaaaahhhgmmmmmsssswwwwwrrrrrqqqjjjjdddcccc\n\
\abcccaaaaaacccccaaacacccccccccccccccccaaaccaacccccccccaaaaaaaaccaaaaaaaacaaccccaaaacccccccccccccccccccccccccccccccaaaaaaaccggggmmmmmmssswwwwrrrrrkjjjjjddddcccc\n\
\abaaaaaaaaccccaacaaaaaccccaaacccccccccaaaccaaccccccccccccaaaccccaaaaacaaaaaccccaaaacccccccccccaacccccccccccaaccccaaaaaacccccgggggmmmllssswwrrrkkkkkjjjddddacccc\n\
\abaaaaaaaaacccaaaaaaaaccccaaaacccccccccaaaaaaccccccccccccaaacccccccaacccaaacaaacaaaccccccccccaaaacccccccaaaaaacccaaaaaaacccccgggggglllsssrrrrrkkkkkkdddddaacccc\n\
\abaaaaaaaaaacccaaaaaccccccaaaaccccccccccaaaaaaaccccccaaccccccccccccaaaaaaaaaaaaccccccccccccccaaaacccccccaaaaaccccaaccaaacccccccggggglllsrrrrrkkkeeedddddaaccccc\n\
\abaacaaaaaaaccccaaaaacccccaaaccccccccccccaaaaaaccccaaaaccccccccccccccaaaaaaaaacccccccccccccccaaaacccccccaaaaaaacccccccaacccccccccgggglllrrrrkkkeeeeeddaaaaccccc\n\
\abaaaaaacccccccaaacaacccccccccccccccccccaaaaaccccccaaaaaaccccccccaaacccaaaaacccccccccccccccccccccccccccaaaaaaaacccccccccccccccccccggfllllllkkkeeeeeccaaaaaacccc\n\
\abaaaaacccccccccaacccccccccccccccccccccaaaaaacccccccaaaacccccacccaaccccaaaaaaccccccccccccccccccccccccccaaaaaaaacccccccccccaacccccccffflllllkkkeeeccccaaaaaacccc\n\
\abaaaaacccccccccccccccccccccccccccaacccccccaaccccccaaaaacccccaaaaaaaccaaaaaaaaaaccccccccccccccccccccccccacaaacccccccccaaccaaccccccccfffllllkeeeecccccaacccccccc\n\
\abaaaacccccccccccccccccccccccccccaaacccccccccccccccaacaacccccaaaaaaccaaaaacaaaaaccccccccccccccccccccccccccaaacccccccccaaaaaaccccccccffffffffeeeeccccccccccccccc\n\
\abaaaccccccccccccccccccccccccccccaaaaacacccccccccccccccccccccaaaaaaaaaaaaccaaaaaaaaccccccaaccccccccccaaaaacccccccccccccaaaaaaacccccccfffffffeeaaccccccccccccaaa\n\
\abaacccccaacaacccccccccccccaacaaaaaaaaaacccccccaaccccccccccccaaaaaaaaaaacccaaaaaaaacccccaaacaacccccccaaaaaccccccccccaaccaaaaaaccccccccafffffeaacccccccccccccaaa\n\
\abaaaccccaaaaacccccccccccccaacaaaaaaaaaaccccccaaaccccccccccccaaaaaaaaaaaccccaaaaaccccccccaaaaaccccccaaaaaacccccccaacaaaaaaaaccccccccccaaacccccccccccccccccccaaa\n\
\abccccccccaaaaacccccccccaaaaaaaaaaaaaacccccaaaaacaaccccccaaaaaaaaaaaaaaaaaaaaaaaaacccccaaaaaacccccccaaaaaacccccccaaaaaaaacaaccccccccccaaaccccccccccccccccaaaaaa\n\
\abcccccccaaaaaacccccccccaaaaaaaaaaaaaacccccaaaaaaaaccccccaaaaacaaaaaaaaaaaaaaaaaaacccccaaaaaaaacccccaaaaaaccccccaaaaaaaaccaacccccccccccccccccccccccccccccaaaaaa"