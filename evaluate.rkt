#lang racket

(require "util.rkt")

(define (char-is-number-part? ch)
  (or (equal? ch #\.) (char-numeric? ch))
)

(define (is-number-tk s)
  (true-for-all? char-is-number-part? (string->list s))
)

(define (is-identifier-tk s)
  (true-for-all? char-alphabetic? (string->list s))
)

(define (is-symbolic-tk s)
  (not (or (is-number-tk s) (is-identifier-tk s)))
)

(define (binary-op-on-str x y op)
  (real->decimal-string (op (string->number x) (string->number y)))
)

(define (perform-multiplication tokens)
  (write-string (format "perform-mult~%"))
  (if (>= (length tokens) 3)
      (let* ([acc (first tokens)]
             [op (first (rest tokens))]
             [tk-aft-op (rest (rest (rest tokens)))]
             [operand (first (rest (rest tokens)))])
        (printf "mult op: ~a~%" op)
        (let ([result (cond
                        [(equal? op "*") (binary-op-on-str acc operand *)]
                        [(equal? op "/") (binary-op-on-str acc operand /)]
                        [(equal? op "%") (binary-op-on-str acc operand modulo)]
                        [else '()]
                      )])
          (if
           (not (null? result))
           (perform-multiplication (cons result tk-aft-op))
           tokens
          )
        )
      )
      tokens
  )
)

(define (perform-addition-impl tokens)
  (write-string (format "perform-add-impl ~a~%" tokens))
  (if (>= (length tokens) 3)
      (let* ([acc (first tokens)]
             [op (first (rest tokens))]
             [new-tks (perform-multiplication (rest (rest tokens)))]
             [tk-aft-op (rest new-tks)]
             [operand (first new-tks)])
        (let ([result (cond
                        [(equal? op "+") (binary-op-on-str acc operand +)]
                        [(equal? op "-") (binary-op-on-str acc operand -)]
                        [else '()]
                      )])
          (if
           (not (null? result))
           (perform-addition (cons result tk-aft-op))
           tokens
          )
        )
      )
      (begin
        (write-string (format "not enough tokens!~%"))
        tokens)
  )
)

(define (perform-addition tokens)
  (perform-addition-impl (perform-multiplication tokens))
)

(provide evaluate)
(define (evaluate tokens)
  (define (iter tks)
    (write-string (format "iter~%"))
    (if (>= (length tks) 3)
        (iter (perform-addition tks))
        tks
    )
  )
  (let ([result (iter tokens)])
    (if (equal? (length result) 1)
        (first result)
        "ERR"
    )
  )     
)