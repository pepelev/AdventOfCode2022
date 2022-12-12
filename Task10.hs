module Task10 where

ops = lines input

get x _ [] = x
get x 0 _ = x
get x n ("noop":ops) = get x (n - 1) ops
get x n (op:ops) =
    let [addx, value] = words op
    in  if n <= 2 then x else get (x + read value) (n - 2) ops

get' n = get 1 n ops

answer1 = sum $ map (\clock -> clock * get' clock) [20,60 .. 220]

---

sprite n =
    let g = get' n
    in  [max 0 (g - 1) .. min 39 (g + 1)]

crt :: [(Int, Int)]
crt = [(x, y) | y <- [0..5], x <- [0..39]]

indexed :: [a] -> [(Int, a)]
indexed xs = zip [1..] xs

pixels =
    let ind = indexed crt
        lit = filter (\(index, (x, y)) -> x `elem` (sprite index)) ind
    in  map snd lit

draw = map (\x -> if x `elem` pixels then '#' else ' ') crt

batch n [] = []
batch n list =
    let part = take n list
        rest = drop n list
    in  part : batch n rest

answer2 = do
    let [a, b, c, d, e, f] = batch 40 draw
    putStrLn a
    putStrLn b
    putStrLn c
    putStrLn d
    putStrLn e
    putStrLn f

input = "noop\n\
\noop\n\
\addx 5\n\
\addx 1\n\
\noop\n\
\noop\n\
\addx 3\n\
\addx 1\n\
\addx 6\n\
\noop\n\
\addx -1\n\
\addx 5\n\
\addx 1\n\
\noop\n\
\addx 4\n\
\addx 1\n\
\noop\n\
\addx -6\n\
\addx 12\n\
\noop\n\
\addx 3\n\
\addx 1\n\
\addx -26\n\
\addx -12\n\
\addx 5\n\
\addx 19\n\
\addx -3\n\
\addx -13\n\
\addx 2\n\
\noop\n\
\addx 3\n\
\addx 2\n\
\noop\n\
\addx 3\n\
\addx 15\n\
\addx -8\n\
\addx 2\n\
\addx 6\n\
\noop\n\
\addx -23\n\
\addx 20\n\
\addx 3\n\
\addx 2\n\
\addx 5\n\
\addx -40\n\
\noop\n\
\noop\n\
\addx 3\n\
\addx 6\n\
\addx -2\n\
\noop\n\
\addx 5\n\
\noop\n\
\noop\n\
\addx 5\n\
\addx -2\n\
\addx 9\n\
\noop\n\
\noop\n\
\noop\n\
\addx -14\n\
\addx 17\n\
\noop\n\
\noop\n\
\addx 8\n\
\noop\n\
\noop\n\
\addx -2\n\
\addx 4\n\
\noop\n\
\noop\n\
\addx -35\n\
\noop\n\
\noop\n\
\noop\n\
\addx -1\n\
\addx 5\n\
\addx 6\n\
\noop\n\
\addx -4\n\
\addx 5\n\
\addx 2\n\
\addx 3\n\
\noop\n\
\addx 5\n\
\addx 14\n\
\addx -10\n\
\addx -25\n\
\addx 1\n\
\addx 38\n\
\addx -6\n\
\addx 2\n\
\addx 3\n\
\addx -2\n\
\addx -38\n\
\noop\n\
\addx 9\n\
\addx -4\n\
\noop\n\
\addx 25\n\
\addx -20\n\
\noop\n\
\addx 3\n\
\addx 2\n\
\addx 5\n\
\addx 2\n\
\addx -9\n\
\addx 14\n\
\addx -2\n\
\noop\n\
\noop\n\
\addx 7\n\
\addx 3\n\
\addx -2\n\
\addx 2\n\
\noop\n\
\addx 3\n\
\addx -38\n\
\noop\n\
\addx 7\n\
\noop\n\
\noop\n\
\addx 1\n\
\noop\n\
\addx 3\n\
\addx 1\n\
\noop\n\
\noop\n\
\addx 6\n\
\noop\n\
\addx 4\n\
\addx 1\n\
\noop\n\
\noop\n\
\addx 4\n\
\addx 1\n\
\addx 7\n\
\addx -3\n\
\addx 5\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\noop"