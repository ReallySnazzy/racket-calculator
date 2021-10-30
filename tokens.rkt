#lang racket

(define (make-tk str tokens is-match)
  (define (tk-all i)
    (cond
      [(= i (string-length str)) (- i 1)]
      [(is-match (string-ref str i)) (tk-all (+ i 1))]
      [else (- i 1)]
    )
  )
  (let* ([indx (+ (tk-all 0) 1)]
         [next-tk (substring str 0 indx)]
         [new-str (substring str indx)])
    (tokenize new-str (cons next-tk tokens))
  )
)

(define (is-tk-number ch)
  (or (equal? #\. ch) (char-numeric? ch))
)

(define (tk-number str tokens)
  (make-tk str tokens is-tk-number)
)

(define (tk-alpha str tokens)
  (make-tk str tokens char-alphabetic?)
)

(define (tk-symbol str tokens)
  (tokenize (substring str 1) (cons (make-string 1 (string-ref str 0)) tokens))
)

(define (tokenize str tokens)
  (if (> (string-length str) 0)
      (let ([ch (string-ref str 0)])
        (cond
          [(or (char-numeric? ch) (equal? ch #\.)) (tk-number str tokens)]
          [(char-alphabetic? ch) (tk-alpha str tokens)]
          [else (tk-symbol str tokens)]
        )
      )
      (reverse tokens)
  )
)

(provide tokens)
(define (tokens str)
  (tokenize str '())
)