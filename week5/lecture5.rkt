#lang racket

(provide (all-defined-out))

;; lambda
(define cube
  (lambda (x)
    (* x x x)))

;; recursion
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

;; pow in curry form
(define curry-pow
  (lambda (x)
    (lambda (y)
      (pow x y))))

(define three-to-the
    (curry-pow 3))

;; sum all numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

;;append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys)))) 

;;map
(define (my-map f xs)
  (if (null? xs)
      xs
      (cons (f (car xs)) (my-map f (cdr xs)))))

;; Dynamic typing

(define YS (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))

;; sum arbitrary deep list

(define (deep-sum xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (deep-sum (cdr xs)))
          (+ (deep-sum (car xs)) (deep-sum (cdr xs))))))



;;streams
(define ones
  (lambda () (cons 1 ones)))

(define (f x)
  (cons x (lambda () (f (add1 x)))))

(define (g x)
  (lambda () (cons x (g (add1 x)))))


(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))


;;memoization
(define fibon
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1)) (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))


;;macro
;my-if
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))
;comment-out
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore out) out]))
;;my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e) (mcons #f (lambda () e))]))
; for
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (letrec ([loop (lambda (it)
                 (if (> it h)
                     #t
                     (begin body (loop (+ it 1)))))])
         (loop l)))]))
           