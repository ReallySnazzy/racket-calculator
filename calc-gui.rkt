#lang racket

(require racket/gui/base)
(require table-panel)

(define calc-window
  (instantiate dialog% ("Calc"))
)

(define vertical-view
  (instantiate vertical-panel%
    (calc-window)
    (vert-margin 2)
    (horiz-margin 2)
    (spacing 4)
  )
)

(define calc-window-txt "")

(define text-box
  (new text-field%
       (label "")
       (parent vertical-view)
       (enabled #f)
  )
)

(define buttons
  (list
   (list "PI" "e" "C" "<=")
   (list "(" ")" "!" "/")
   (list "7" "8" "9" "*")
   (list "4" "5" "6" "-")
   (list "1" "2" "3" "+")
   (list "%" "0" "." "=")
  )
)

(define button-row-ct (length buttons))

(define button-col-ct (length (list-ref buttons 0)))

(define table-view
  (instantiate table-panel%
    (vertical-view)
    (alignment '(center center))
    (dimensions (list button-row-ct button-col-ct))
  )
)

(define (append-calc-text txt)
  (set! calc-window-txt (string-append calc-window-txt txt))
  (send text-box set-value calc-window-txt)
)

(define (clear-calc-text)
  (set! calc-window-txt "")
  (send text-box set-value calc-window-txt)
)

(define (backspace-calc-text)
  (set! calc-window-txt
        (substring
         calc-window-txt
         0
         (- (string-length calc-window-txt) 1)
        )
  )
  (send text-box set-value calc-window-txt)
)

(define (make-calc-button-callback txt calc-cb)
  (lambda (btn ctl-evt)
    (cond
      [(equal? txt "<=") (backspace-calc-text)]
      [(equal? txt "C") (clear-calc-text)]
      [(equal? txt "=") (calc-callback-and-disp calc-cb)]
      [else (append-calc-text txt)]
    )
  )
)

(define (calc-callback-and-disp calc-callback)
  (let ([result (calc-callback calc-window-txt)])
    (set! calc-window-txt result)
    (send text-box set-value result)
  )
)

(provide show-calc)
(define (show-calc calc-callback)
  (for ([btn-row buttons])
    (for ([button-txt btn-row])
      (new button%
           [parent table-view]
           [label (format "~a" button-txt)]
           [callback (make-calc-button-callback button-txt calc-callback)]
      )
    )
  )

  (send calc-window show #t)
)
