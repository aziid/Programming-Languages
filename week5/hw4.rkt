
#lang racket

(provide (all-defined-out)) ;; test file "hw4test.rkt"

;; 1
;; Number Number Natrual -> (listof Number)
;; produce a list of numbers from lo to hi seperated by stride
(define (sequence lo hi stride)
  (if (> lo hi)
      null
      (cons lo (sequence (+ lo stride) hi stride))))

;; 2
;; (listof String) String -> (listof String)
;; produce a list of strings with each string appended with given suffix
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;; 3
;; (listof X) Number -> X
;; produce the i-th element from the given list xs, where i is the remainder
;; of n divided by the list length
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
;; Stream Natrual -> (listof X)
;; produce a list of first n values of stream s
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; 5
;; () -> Stream
;; produce a natural number stream of which numbers divisble by 5 are negated
(define funny-number-stream
    (letrec ([neg-five (lambda (x) (if (zero? (remainder x 5)) (- x) x))]
             [f (lambda (x) (cons (neg-five x) (lambda () (f (+ x 1)))))])
      (lambda () (f 1))))

;; 6
;; () -> Stream
;; produce a stream thats alternates between "dan.jpg" and "dog.jpg"
(define dan-then-dog
  (letrec ([alt (lambda (x) (if (zero? (remainder x 2)) "dan.jpg" "dog.jpg"))] 
           [f (lambda (x) (cons (alt x) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; 7
;; Stream -> Stream
;; produce a stream whose i-th element is a pair of 0 and the i-th element of
;; given stream
(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car x)) (lambda () (f ((cdr x))))))])
    (lambda () (f (s)))))
  
;; 8
;; (listof X) (listof Y) -> Stream
;; produce a stream that spits pairs of elements from list xs and ys
;; Assume:: xs and ys are not empty
(define (cycle-lists xs ys)
  (letrec ([pr (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [f (lambda (x) (cons (pr x) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))
  
;; 9
;; X Vector -> (pairof X Y) or #f
;; produce the fisrt element of vec whose car field is v, otherwise false  
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([n-th (vector-ref vec n)])
                      (if (and (pair? n-th) (equal? (car n-th) v))
                          n-th
                          (f (+ n 1))))))])
    (f 0)))
  
;; 10
;; (listof X) Number -> assoc
;; produce a function that behaves like assoc with a cahe of n elements
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo pos new-ans)
                              (set! pos (remainder (+ pos 1) n))
                              (print memo)
                              new-ans)
                            #f)))))])
    f))

;; 11
;; () -> Macro
;; produce a macro that evaluates e1 once and e2 many times until the value of
;; the evaluation of e2 is not less than that of e1
;; Assume:: expressions e1 and e2 evaluate to numbers
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([a e1]
              [f (lambda ()
                   (if (>= e2 a)
                       #t
                       (f)))])
       (f))]))