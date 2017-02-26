
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (let [(x (+ low stride))]
    (if (> low high)
        null
        (cons low (sequence x high stride)))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (let [(pr (s))]
    (if (zero? n)
        null
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))
    
(define funny-number-stream
  (letrec ([f (lambda (n)
                (cons (if (zero? (remainder n 5)) (- n) n)
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))
    
(define dan-then-dog
  (letrec ([f (lambda (n)
             (cons (if (odd? n) "dan.jpg" "dog.jpg")
                   (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s)))
                      (lambda () (f (cdr (s))))))])
    (lambda () (f stream))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
  (lambda () (f 0))))

(define (vector-assoc value vector)
  (letrec ([length (vector-length vector)]
           [vector-assoc-rec
            (lambda (x)
              (cond [(= x length) #f]
                    [(not (pair? (vector-ref vector x))) (vector-assoc-rec (+ x 1))]
                    [(equal? (car (vector-ref vector x)) value) (vector-ref vector x)]
                    [#t (vector-assoc-rec (+ x 1))]))])
    (vector-assoc-rec 0)))


(define (cached-assoc xs n)
  (letrec ([x 0]
           [memo (make-vector n #f)]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      (cdr ans)
                      (begin
                        (let ([new-ans (assoc v xs)])
                          (vector-set! memo x (cons v new-ans))
                          (set! x (remainder (+ x 1) n))
                          new-ans)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do body)
     (let ([v1 e1])
       (letrec ([loop (lambda ()
                        (let ((v2 body))
                          (if (>= v2 v1)
                              #t
                              (loop))))])
         (loop)))]))