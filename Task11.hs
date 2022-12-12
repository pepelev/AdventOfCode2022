module Task11 where

import Data.List (sortBy)
import qualified Data.Array as A

data Monkey = Monkey { index :: Int  
                     , items :: [Integer]  
                     , operation :: Operation
                     , divisor :: Integer
                     , trueIndex :: Int  
                     , falseIndex :: Int
                     , score :: Integer
                     } deriving (Show)

batch n [] = []
batch n list =
    let part = take n list
        rest = drop n list
    in  part : batch n rest

data Operation = Plus Integer | Multiply Integer | Sqr deriving (Eq, Show)

op "+" = (+)
op "*" = (*)

apply (Plus a) b = a + b
apply (Multiply a) b = a * b
apply (Sqr) b = b * b

parseOp ["*", "old"] = Sqr
parseOp ["*", a] = Multiply (read a)
parseOp ["+", a] = Plus (read a)

parse [header, items, operation, test, true, false] = 
    let index = read $ take 1 (words header !! 1)
        itms  = read ("[" ++ (concat $ drop 2 $ words items) ++ "]")
        ["Operation:", "new", "=", "old", sign, operand] = words operation
        oper = parseOp [sign, operand]
        ["Test:", "divisible", "by", divisor'] = words test
        divisor = read divisor'
        ["If", "true:", "throw", "to", "monkey", trueIndex'] = words true
        trueIndex = read trueIndex'
        ["If", "false:", "throw", "to", "monkey", falseIndex'] = words false
        falseIndex = read falseIndex'
    in  Monkey {
        index = index,
        items = itms,
        operation = oper,
        divisor = divisor,
        trueIndex = trueIndex,
        falseIndex = falseIndex,
        score = 0
    }

monkeys = A.listArray (0 :: Int, 7) $ map (parse . (take 6)) $ batch 7 $ lines input

orderBy f list = sortBy (\a b -> (f a) `compare` (f b)) list
topBy n f list = take n $ reverse $ orderBy f list

throw monkey item =
    let worry = apply (operation monkey) item `mod` divoid -- `div` 3
    in  if (worry `mod` (divisor monkey)) == 0
        then (trueIndex monkey, worry)
        else (falseIndex monkey, worry)

throws monkey = map (throw monkey) $ items monkey

turn monkey =
    let score' = score monkey + fromIntegral (length (items monkey))
        monkey' = monkey { items = [], score = score' }
    in  (monkey', throws monkey)

append worry monkey = monkey { items = items monkey ++ [worry] }

update f i array =
    let item = array A.! i
    in  array A.// [(i, f item)]

round monkeys =
    let turn' monkeys i =
            let current = monkeys A.! i
                (turned, throws) = turn current
                patched = monkeys A.// [(index current, turned)]
                catched = foldl (\arr (who, worry) -> update (append worry) who arr) patched throws
            in  catched
    in  foldl turn' monkeys [0..7]

divisors = [13, 19, 11, 17, 3, 7, 5, 2]
divoid = product divisors

answer1 = product $ topBy 2 id $ map score $ A.elems $ last $ take 21 $ iterate Task11.round monkeys

answer2 = product $ topBy 2 id $ map score $ A.elems $ last $ take 10001 $ iterate Task11.round monkeys

input = "Monkey 0:\n\
\  Starting items: 53, 89, 62, 57, 74, 51, 83, 97\n\
\  Operation: new = old * 3\n\
\  Test: divisible by 13\n\
\    If true: throw to monkey 1\n\
\    If false: throw to monkey 5\n\
\\n\
\Monkey 1:\n\
\  Starting items: 85, 94, 97, 92, 56\n\
\  Operation: new = old + 2\n\
\  Test: divisible by 19\n\
\    If true: throw to monkey 5\n\
\    If false: throw to monkey 2\n\
\\n\
\Monkey 2:\n\
\  Starting items: 86, 82, 82\n\
\  Operation: new = old + 1\n\
\  Test: divisible by 11\n\
\    If true: throw to monkey 3\n\
\    If false: throw to monkey 4\n\
\\n\
\Monkey 3:\n\
\  Starting items: 94, 68\n\
\  Operation: new = old + 5\n\
\  Test: divisible by 17\n\
\    If true: throw to monkey 7\n\
\    If false: throw to monkey 6\n\
\\n\
\Monkey 4:\n\
\  Starting items: 83, 62, 74, 58, 96, 68, 85\n\
\  Operation: new = old + 4\n\
\  Test: divisible by 3\n\
\    If true: throw to monkey 3\n\
\    If false: throw to monkey 6\n\
\\n\
\Monkey 5:\n\
\  Starting items: 50, 68, 95, 82\n\
\  Operation: new = old + 8\n\
\  Test: divisible by 7\n\
\    If true: throw to monkey 2\n\
\    If false: throw to monkey 4\n\
\\n\
\Monkey 6:\n\
\  Starting items: 75\n\
\  Operation: new = old * 7\n\
\  Test: divisible by 5\n\
\    If true: throw to monkey 7\n\
\    If false: throw to monkey 0\n\
\\n\
\Monkey 7:\n\
\  Starting items: 92, 52, 85, 89, 68, 82\n\
\  Operation: new = old * old\n\
\  Test: divisible by 2\n\
\    If true: throw to monkey 0\n\
\    If false: throw to monkey 1"