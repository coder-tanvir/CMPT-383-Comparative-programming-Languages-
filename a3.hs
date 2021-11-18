-- 1
snoc:: a -> [a] -> [a]
snoc x [] = [x]
snoc x lst = (head lst):(snoc x (tail lst))
-- 2
myappend :: [a] -> [a] -> [a]
myappend x []         = x
myappend x (y:ys)     = myappend (snoc y x) ys
-- 3
snoc:: a -> [a] -> [a]
snoc x [] = [x]
snoc x lst = (head lst):(snoc x (tail lst))
myreverse:: [a]->[a]
myreverse lst  
             |(null (tail lst)) = (snoc (head lst) []) 
             |otherwise = (snoc (head lst) (myreverse (tail lst)))
-- 4
snoc:: a -> [a] -> [a]
snoc x [] = [x]
snoc x lst = (head lst):(snoc x (tail lst))
myappend :: [a] -> [a] -> [a]
myappend x []         = x
myappend x (y:ys)     = myappend (snoc y x) ys
myreverse:: [a]->[a]
myreverse lst  
             |(null (tail lst)) = (snoc (head lst) []) 
             |otherwise = (snoc (head lst) (myreverse (tail lst)))


primesearch :: Integer -> Integer -> Bool
primesearch n d
    | n <= 2      = False
    | d == 1    = True
    | n `mod` d == 0    = False
    | otherwise     = primesearch n (d-1)

isprime :: Integer -> Bool
isprime n   
        | n < 0  =(error "negative number")
        |otherwise = primesearch n (n-1)

reverseInt :: Integer -> Integer
reverseInt n = read $ myreverse (show n) :: Integer

emirpsearch :: Integer -> Bool
emirpsearch n
    | n == reverseInt n  = False
    | otherwise  = isprime n && isprime (reverseInt n)

count_emirps :: Integer -> Integer
count_emirps n
    | n == 0   = 0
    | emirpsearch n = 1 + count_emirps (n - 1)
    | otherwise  = count_emirps (n - 1)


-- 5
biggest_sum:: [[Integer]]->[Integer]
biggest_sum lst
               | (lengthlst lst) == 1 = (head lst)
               | (lengthlst lst) == 2 = (bigger (head lst) (head (tail lst)))
               | otherwise = (bigger (head lst) (biggest_sum (tail lst)))  

sum1::[Integer]->Integer
sum1 [] = 0
sum1(first:rest)= first +(sum1 rest)


lengthlst::[[Integer]]->Integer
lengthlst lst 
           |null lst =0 
           |otherwise =1+lengthlst(tail lst)

bigger::[Integer]->[Integer]->[Integer]
bigger a b 
         |(sum1 a) < (sum1 b) =b 
         | otherwise = a
              

-- 6
greatest:: (a->Int)->[a]->a
greatest f lst 
              | (lengthf lst) == 1 = (head lst)
              | (lengthf lst) == 2 = (bigger_of_two  f (head lst) (head (tail lst)))
              | otherwise = (bigger_of_two  f (head lst) (greatest f (tail lst)))

bigger_of_two::(a->Int)->a->a->a
bigger_of_two  f lst1 lst2 
                         | (f lst1) < (f lst2) =lst2 
                         |otherwise =   lst1


lengthf:: [a]->Int
lengthf lst 
         | null lst= 0 
         |otherwise = 1 + lengthf (tail lst)


-- 7
is_bit::Int->Bool
is_bit x
    |(x==0) =True
    |(x==1) =True
    |otherwise =False

-- 8
flip_bit::Int->Int
flip_bit x
    |(x==0) =1
    |(x==1) =0
    |otherwise =(error "Bits are only 0 and 1")

-- 9
is_bit::Int->Bool
is_bit x
    |(x==0) =True
    |(x==1) =True
    |otherwise =False

is_bit_seq1:: [Int]->Bool
is_bit_seq1 lst
           | (null lst) = True
           | otherwise = (is_bit (head lst)) && (is_bit_seq1 (tail lst))
          
is_bit_seq2:: [Int]->Bool
is_bit_seq2 lst = if (null lst) then True else (is_bit (head lst)) && (is_bit_seq1 (tail lst))

is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 [] = True
is_bit_seq3 xs = all is_bit xs
-- 10
flip_bit::Int->Int
flip_bit x
    |(x==0) =1
    |(x==1) =0
    |otherwise =(error "Bits are only 0 and 1")
    
invert_bits1:: [Int]->[Int]
invert_bits1 x 
             | null x = [] 
             |otherwise =(flip_bit (head x)):(invert_bits1 (tail x))

invert_bits2:: [Int]->[Int]
invert_bits2 x = (map flip_bit x)

invert_bits3:: [Int]->[Int]
invert_bits3 x = [flip_bit y | y <- x]

-- 11
bit_count:: [Int]->(Int,Int)
bit_count x = ((oneandzero x) , ((length x) - (oneandzero x))) 

oneandzero:: [Int]->Int
oneandzero x 
            | null x = 0 
            | ((head x)==0) =1+ (oneandzero (tail x))
            | otherwise =(oneandzero (tail x))


-- 12
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n = sequence (replicate n [0, 1])

data List a = Empty | Cons a (List a)
    deriving Show
-- 13
toList::[a]->List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs) 
data List a = Empty | Cons a (List a)
    deriving Show

-- 14
toHaskellList :: List a -> [a]
toHaskellList Empty      = []
toHaskellList (Cons h r) = h : (toHaskellList r)
data List a = Empty | Cons a (List a)
    deriving Show

-- 15
append :: List a -> List a -> List a
append Empty (Cons head tail)                     = (Cons head tail)
append (Cons head1 tail1) (Cons head2 tail2)      = (Cons head1 (append tail1 (Cons head2 tail2)))
data List a = Empty | Cons a (List a)
    deriving Show



-- 16
removeAll:: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons a lst) = if (f a) == True then (removeAll f lst) else Cons a (removeAll f lst)  
data List a = Empty | Cons a (List a)
    deriving Show

                   
                         