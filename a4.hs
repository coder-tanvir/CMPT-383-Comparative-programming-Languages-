data Token = Num Double | Err String   | Operator [Char]                --Token Types are double number, operator as a list of char and error string to define errors
  deriving (Show)

calc:: String->String                                            --words is turning a string into list of string , calc_caller is turning it to a list of tokens to be evaluated by evaluator 
calc str = (evaluator (calc_caller (words str)) [] 0)

calcStack:: String->String
calcStack str  
          |(null (evaluator (calc_caller (words str)) [] 1)) = [] 
          |otherwise= (evaluator (calc_caller (words str)) [] 1) 

--Custom functions :- 

str_double::String->Double                                                             --turning string to double so that numbers can be calculated         
str_double str = read str :: Double

double_str::Double->String                                                             --turning double to string put in stack
double_str x = show x

my_append:: [Token]->[Token]->[Token]                                                   --Basically append function with token type
my_append lst1 lst2 = if (null (tail lst1)) then (head lst1):lst2 else (head lst1):(my_append (tail lst1) lst2)

my_lengthlst::[Double]->Int                                                             --length function that takes a list of double and returns a integer stating the length of list                                               
my_lengthlst lst  
             |null lst = 0  
             |otherwise=1 + my_lengthlst (tail lst)

my_sum:: [Double]->Double                                                                 --sum function counting the sum of a given list
my_sum lst  
         |null lst = 0 
         |null (tail lst)= (head lst)
         |otherwise=(head lst) + (my_sum (tail lst)) 

my_mul:: [Double]->Double                                                                  --mul function multiplying all the element of the list of double type
my_mul lst 
        |null lst = 1  
        |otherwise = (head lst) * (my_mul (tail lst))


 
calc_caller:: [String]->[Token]                                                             --calc_caller is just turning the input string to lst consisting of same elements of form Token lst
calc_caller lst 
           | (null lst) = []
           |(head lst) == "+all" = (my_append ([Operator "+all"]) (calc_caller (tail lst)))         --It has mathmatical operators like +,-,*,\,inc,dec
           |(head lst) == "*all" = (my_append ([Operator "*all"]) (calc_caller (tail lst)))                
           |(head lst) == "dup" = (my_append ([Operator "dup"]) (calc_caller (tail lst)))           --additional operators like +all, *all ,dup,pop are also included 
           |(head lst) == "pop" = (my_append ([Operator "pop"]) (calc_caller (tail lst)))
           |(head lst) == "clear" = (my_append ([Operator "clear"]) (calc_caller (tail lst)))        --some of the operators are binary and some are unary
           |(head lst) == "swap" = (my_append ([Operator "swap"]) (calc_caller (tail lst)))            
           | (head lst) == "inc" =(my_append ([Operator "inc"]) (calc_caller (tail lst)))
           | (head lst) == "+" = (my_append ([Operator "+"]) (calc_caller (tail lst)))        --For a valid calc command it has to be consisted of numbers and operator.
           | (head lst) == "-" = (my_append ([Operator "-"]) (calc_caller (tail lst)))
           | (head lst) == "*" = (my_append ([Operator "*"]) (calc_caller (tail lst)))           -- If the head of the list is none of the operators then it should be a number in the head of the 
           | (head lst) == "/" = (my_append ([Operator "/"]) (calc_caller (tail lst)))
           | (head lst) == "dec" = (my_append ([Operator "dec"]) (calc_caller (tail lst)))       --list and it has to be changed to number from string before passing to the evaluator function.
           |(head lst) == "sqrt" = (my_append ([Operator "sqrt"]) (calc_caller (tail lst)))
           |(head lst) == "sin" = (my_append ([Operator "sin"]) (calc_caller (tail lst)))
           |(head lst) == "cos" = (my_append ([Operator "cos"]) (calc_caller (tail lst)))
           |(head lst) == "inv" = (my_append ([Operator "inv"]) (calc_caller (tail lst)))
           | otherwise = (my_append ([Num (str_double (head lst))]) (calc_caller (tail lst)))  
            

           
evaluator::[Token]->[Double]->Int->String                                               --takes a token and double list and returns a integer and a string

evaluator ((Operator "+"):rest) lst i                                                                                                --For binary operators length of lists must be greater than or equal to 2
                              |((my_lengthlst lst)>= 2) = (evaluator rest (((head lst) + (head (tail lst))):(tail (tail lst))) i)   --head of the list is the first element
        
                              |otherwise= (evaluator ((Err "Binary operator need 2 element to add"):rest) lst i)                       -- head of tail of list is second element and next is operator
evaluator ((Operator "-"):rest) lst i 
                               |((my_lengthlst lst) >= 2) = (evaluator rest (((head (tail lst)) - (head lst)):(tail (tail lst))) i) 
                               |otherwise= (evaluator ((Err"Binary operator need 2 element to subtract"):rest) lst i)
evaluator ((Operator "*"):rest) lst i 
                               |((my_lengthlst lst) >= 2) = (evaluator rest (((head lst) * (head (tail lst))):(tail (tail lst))) i)  
                               |otherwise=(evaluator ((Err "Binary operator nedd 2 element to multiply"):rest) lst i)
evaluator ((Operator "/"):rest) lst i
                           | (my_lengthlst lst) >= 2 && ((head lst)==0) = (double_str ((head (tail lst)) / (head lst)))
                           | (my_lengthlst lst) >= 2 = (evaluator rest (((head (tail lst)) / (head lst)):(tail (tail lst))) i) 
                           | otherwise = (evaluator ((Err "Binary operator nedd 2 element to divide"):rest) lst i)
evaluator ((Operator "inc"):rest) lst i                                                                                         --For all unary operator length of list must be greater than or equal to one
                                | ((my_lengthlst lst) >= 1) =(evaluator rest ((1 + (head lst)):(tail lst)) i)  
                                |otherwise =(evaluator ((Err "insufficient argument"):rest) lst i)
evaluator ((Operator "dec"):rest) lst i 
                                 |((my_lengthlst lst) >= 1) = (evaluator rest (((head lst) - 1):(tail lst)) i)  
                                 |otherwise=(evaluator ((Err "insufficient argument"):rest) lst i)
evaluator ((Operator "sqrt"):rest) lst i 
                                  |((my_lengthlst lst) >= 1) = (evaluator rest ((sqrt (head lst)):(tail lst)) i) 
                                  |otherwise=(evaluator ((Err"insufficient argument"):rest) lst i)
evaluator ((Operator "sin"):rest) lst i  
                                |((my_lengthlst lst) >= 1) = (evaluator rest ((sin (head lst)):(tail lst)) i)  
                                |otherwise=(evaluator ((Err "insufficient argument"):rest) lst i)
evaluator ((Operator "cos"):rest) lst i  
                                 |((my_lengthlst lst) >= 1) = (evaluator rest ((cos (head lst)):(tail lst)) i)  
                                 |otherwise=(evaluator ((Err "insufficient argument"):rest) lst i)
evaluator ((Operator "inv"):rest) lst i
                             | ((my_lengthlst lst) >= 1) && ((head lst)==0) = (double_str (1/0))                          --head of the list cannot be o cause that makes inv result in "infinity".
                             | ((my_lengthlst lst) >= 1) = (evaluator rest ((1 / (head lst)):(tail lst)) i) 
                             | otherwise = (evaluator ((Err "insufficient argument"):rest) lst i)
evaluator ((Operator "+all"):rest) lst i                             
                                  |((my_lengthlst lst) >= 1) = (evaluator rest [(my_sum lst)] i)                           --all operaot gets executed on all the elements of the input
                                  |otherwise=(evaluator ((Err "insufficient argument must have >= 1 numbers for +all"):rest) lst i)
evaluator ((Operator "*all"):rest) lst i  
                                  |((my_lengthlst lst) >= 1) = (evaluator rest [(my_mul lst)] i)  
                                  |otherwise=(evaluator ((Err "insufficient argument must have >= 1 numbers for +all"):rest) lst i)
evaluator ((Operator "dup"):rest) lst i                                                                                      --head lst:lst is just pushing the head again in the list
                                 |((my_lengthlst lst) >= 1) = (evaluator rest (head lst:lst) i)  
                                 |otherwise=(evaluator ((Err "insufficient argument must have >= 1 numbers for +all"):rest) lst i)
evaluator ((Operator "pop"):rest) lst i 
                                 |((my_lengthlst lst) >= 1) = (evaluator rest (tail lst) i)                                           
                                 |otherwise=(evaluator ((Err "insufficient argument must have >= 1 numbers for +all"):rest) lst i)
evaluator ((Operator "clear"):rest) lst i 
                                   |((my_lengthlst lst) >= 1) = (evaluator rest [] i)  
                                   |otherwise=(evaluator ((Err "insufficient argument must have >= 1 numbers for +all"):rest) lst i)
evaluator ((Num a):rest) lst i = (evaluator rest (a:lst) i)                                                                   --variable "i" iskeeping track of the stack
evaluator [] lst2 i  
                  |i==0 = (show (head lst2))                                                                                  --i=0 means only element is in top of lst2
                  |otherwise="top" ++ " " ++ (show lst2) ++ " " ++ "bottom"  
evaluator ((Err str):rest) lst i = str                                                                                                         --error string
 


