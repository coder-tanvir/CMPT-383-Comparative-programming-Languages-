#lang racket
#| some of the functions name are differnt  If a part is commented as test, then it is only atest not part of the program. For the last question (check countsys test-suite) this
command will run the test case|#


q1.
(define (my-length lst)
  (cond
    [(empty? lst)  0]
    [else   (+ 1 (my-length (rest lst)))]))

q2.
(define (onion n)
  
    (cond [(= n 0) '()]
          [(< n 0) "negative"]
          [else (list (onion (- n 1)))]
          )
    )

q3.
(define (my-last lst)
  (if (zero? (length (cdr lst)))
      (car lst)
      (my-last (cdr lst))
     
      )

  )

q4.
(define (double-up lst)
        (if (null? lst)
            '()
            (cons (car lst)(cons (car lst) (double-up (cdr lst))))))

q5.
(define (swap-ends lst)
  ( cons (reverse (cdr lst))(car lst))
  )

q6.


q7.
(define (is-relation? lst)
  ( or
       (empty? lst)
       (integer? (car lst))
       
       (equal? (car lst)(is-relation?(cdr lst))))
        
  )
q8.

q9.
(define (is-relation? lst)
  ( or
       (empty? lst)
       (integer? (car lst))
       
       (equal? (car lst)(is-relation?(cdr lst))))
        
  )
q10.
(define countsys
  (lambda (lst)
    (cond [(null? lst) 0]
          [(equal? (atom (car lst)) #t) (+ 1 (countsys (cdr lst)))]
          [else (countsys (cdr lst))]
          )
    )
  )
q11.
(define (fold-right f init lst)
  (if (empty? lst) init
      (f (first lst) (fold-right f init (rest lst)))))
(define countsys
 (lambda (lst)
   
         (fold-right (lambda (x y) (if (atom x) (+ y 1) (+ y 0)))  0 lst)
  
   )
  )
(define atom
  (lambda (x)
    ( cond [(symbol? x) #t]
        [(number? x) #t]
         [else #f]
     )
    )
    )
q12.
#lang racket
(define check
   (lambda (f suite)
     (cond [(equal? (my-length (checker2 f suite)) (my-length2 test-suite)) "all tests passed" ]
           [else (checker2 f suite) ]

       )

  ))

(define my-length
 (lambda (lst)
   (cond [(null? lst) 0]
         [(equal? (car lst) "passed  , ") (+ 1 (my-length (cdr lst)))]
         [else (my-length (cdr lst))  ]
         )
   )
  )

(define my-length2
 (lambda (lst)
   (cond [(null? lst) 0]
         [else (+ 1 (my-length2 (cdr lst)))]
         )
   )
  )

(define test-suite
  '(((1 2) 2)
    ((a) 1)
    ((2) 1)
    (("a") 0)
    ((a b) 2)
    ((3 1) 2)
    ((a 2) 2)
    (("a" b) 1)
    ((1 2 3 4) 4)
    ((a b c d) 4)
    ((1 a 2 3 b c) 6)
    ((1 a () 2 3 "m" b c (4 s 2)) 6)))

#| test 1 |#
(define atom
  (lambda (x)
    ( cond [(symbol? x) #t]
        [(number? x) #t]
         [else #f]
     )
    )
    )
(define countsys
  (lambda (lst)
    (cond [(null? lst) 0]
          [(equal? (atom (car lst)) #t) (+ 1 (countsys (cdr lst)))]
          [else (countsys (cdr lst))]
          )
    )
  )

(define checker2
  (lambda (f suite)
       (cond [(null? suite) '()]
            [(equal? (f (car (car suite))) (car (cdr (car suite)))) (cons "passed  , " (checker2 f (cdr suite))) ]
            [else (cons (format "fail: function input ~a and returned output ~a and expected output ~a ," ' (car (car suite)) (f (car (car suite))) (car (cdr (car suite)))) (checker2 f (cdr suite))) ]
                   

      )
       
  ))
   #| test 2 |#

(define countsys2
  (lambda (lst)
    (cond [(null? lst) 0]
          [(equal? (atom (car lst)) #t) (+ 2 (countsys2 (cdr lst)))]
          [else (countsys2 (cdr lst))]
          )
    )
  )