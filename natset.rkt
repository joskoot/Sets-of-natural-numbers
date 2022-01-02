#lang racket

(provide (all-defined-out))

(define empty-natset 0)

(define total-natset -1)

(define natset? natural?)

(define (make-natset . nat-list)
 (define (range? arg)
  (and
   (pair? arg) (list? arg) (= (length arg) 2)
   (exact-nonnegative-integer? (car arg))
   (exact-nonnegative-integer? (cadr arg))
   (< (car arg) (cadr arg))))
 (define (range->natset range)
  (define from (car range))
  (define to (cadr range))
  (arithmetic-shift (sub1 (arithmetic-shift 1 (- to from))) from))
 (for/fold ((natset 0)) ((arg (in-list nat-list)))
  (cond
   ((exact-nonnegative-integer? arg)
    (bitwise-ior natset (make-single-natset arg)))
   ((range? arg) (bitwise-ior natset (range->natset arg)))
   (else
    (raise-argument-error 'make-natset
    "(or/c natural? (and/c (cons/c natural? natural?) (</c (car arg) (cdr arg))" arg)))))

; ((cons/dc [from exact-nonnegative-integer?] [to (from) (>/c from)]) '(10 10))

(define (make-single-natset i) (arithmetic-shift 1 i))

(define natset-union bitwise-ior)

(define natset-intersection bitwise-and)

(define (natset-complement natset) (- (add1 natset)))

(define (natset-subtract natset . natsets)
 (cond
  ((null? natsets) natset)
  (else (bitwise-and natset (bitwise-not (apply bitwise-ior natsets))))))

(define (natset-member? natset i) (not (zero? (bitwise-and natset (make-single-natset i)))))

(define (nat-subset? a b) (= (bitwise-and a b) b))

(define (natset->string natset #:min-width (min-width 0))
 (define (align ch str)
  (define n (string-length str))
  (cond
   ((< n min-width) (string-append (make-string (- min-width n) ch) str))
   (else str)))
 (cond
  ((zero? natset) (align #\0 "0"))
  ((> natset 0) (align #\0 (string-append "0" (~r natset #:base 2))))
  ((= natset -1) (align #\1 "1"))
  (else
   (align #\1
    (apply string
     (cons #\1
      (for/list ((ch (in-string (~r (bitwise-not natset) #:base 2))))
       (case ch ((#\0) #\1) ((#\1) #\0)))))))))

#;(for/list ((k (in-range 0 10)))
    (define natset (make-natset k))
    (define compl (natset-complement natset))
    (list (natset->string natset #:min-width 20) (natset->string compl #:min-width 20)
          (natset->string natset) (natset->string compl)))

#;(for ((k (in-range 0 10))) (printf "~s ~s ~a ~a~n" k (- (add1 k)) (natset->string k)
                                     (natset->string (- (add1 k)))))