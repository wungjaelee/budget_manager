#lang racket

;; contracts
(define (transaction? t)
  (and (list? t)
       (= (length t) 5)))

(provide
 (contract-out
  [process-transaction-history (-> input-port? (listof transaction?))]))

;; load global variables
; write your local path to US Bank transaction history csv file
(define temp-path "/Users/wungjaelee/Everything/가계부/Sep1-Dec20.csv")

(define in (open-input-file temp-path))

;; process transaction history
(define (process-transaction-history f)
  (define (process-transaction-history f accum-list)
    (let ([line (read-line f)])
      (cond [(eof-object? line) (reverse accum-list)]
            [else
             (process-transaction-history f
                                          (cons (line->transaction line)
                                                accum-list))])))
  (read-line f) ;; ignore header line
  (process-transaction-history f '()))

(define (remove-doublequote line)
  (string-replace line "\"" ""))

(define (remove-all-pattern str pats)
  (cond [(null? pats) str]
        [else (remove-all-pattern (string-replace str (first pats) "")
                                  (rest pats))]))

;; remove unnecessary information in Name
(define (extract-info name)
  (remove-all-pattern name
                      '("DEBIT PURCHASE " "-VISA ")))

(define (line->transaction line)
  (list-update 
   (list-update (string-split (remove-doublequote line) ",")
                4
                string->number)
   2
   extract-info))


;; accessors to transaction data
(define (date t) (list-ref t 0))

(define (name t) (list-ref t 2))

(define (memo t) (list-ref t 3))

(define (amount t) (list-ref t 4))


;; use transaction history to build more specific lists
(define (spending th)
  (filter (lambda (t) (< (amount t) 0)) th))

(define (income th)
  (filter (lambda (t) (> (amount t) 0)) th))


(define th (process-transaction-history in))
th
(spending th)
(income th)

;TO-DO
;1. Categorize each transaction into food, clothing, etc.
;2. Write function for getting monthly transaction
;3. Write functions for getting total income/spending of given transaction history
;4. Write function for monthly report (total spending, total income, categorized spending, maybe word of suggestion)
