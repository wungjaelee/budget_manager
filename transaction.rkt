#lang racket
(require "constants.rkt")
(require "utils.rkt")

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
(define (remove-unnecessary-info name)
  (remove-all-pattern name
                      '("DEBIT PURCHASE " "-VISA ")))

(define (line->transaction line)
  (compose (list
            (lambda (line) (string-split (remove-doublequote line) ","))
            (lambda (lst) (remove-nths lst 1 3)) ;removing transaction-type, memo column
            (lambda (lst) (list-update lst 2 string->number)) ;updating amount column to be number
            (lambda (lst) (list-update lst 1 remove-unnecessary-info))) ;removing unnecessary info from name column
           line))

;(define (line->transaction line)
;  (let ([t-info (string-split (remove-doublequote line) ",")])
;    (
;  (list-update 
;   (list-update (string-split (remove-doublequote line) ",")
;                4
;                string->number)
;   2
;   extract-info))


;;; accessors to transaction data

;; basic accessors
(define (date t) (list-ref t 0))

(define (name t) (list-ref t 1))

(define (amount t) (list-ref t 2))

;; more accessors
(define (month t)
  (string->number
   (list-ref (string-split (date t) "/") 0)))

(define (names th)
  (map (lambda (t) (name t)) th))



;; use transaction history to build more specific lists
(define (spending-th th)
  (filter (lambda (t) (< (amount t) 0)) th))

(define (total-spending th)
  (apply + (map (lambda (t) (amount t)) (spending-th th))))

(define (income-th th)
  (filter (lambda (t) (> (amount t) 0)) th))

(define (total-income th)
  (apply + (map (lambda (t) (amount t)) (income-th th))))

(define (monthly-th th m)
  (filter (lambda (t) (= (month t) m)) th))

(define (cat? t-name cat-constant)
  (ormap (lambda (keyword)
           (string-contains? t-name keyword))
         cat-constant))

;; in need of constant modification
(define (category t-name)
  (cond [(cat? t-name FOOD-DRINK) "FOOD-DRINK"]
        [(cat? t-name LEISURE) "LEISURE"]
        [(cat? t-name TRANSPORTATION) "TRANSPORTATION"]
        [(cat? t-name ACADEMICS) "ACADEMICS"]
        [else "uncategorized"]))

(define (categorize th)
  (map (lambda (t)
         (list-update t 2
                      (lambda (t-name) (cons (category t-name) t-name))))
       th))
  
(define (monthly-report th m)
  (let ([m-th (monthly-th th m)])
    (printf "Monthly report for ~a~n" m)
    (printf "Total spending: ~a~n" (total-spending m-th))
    (printf "Total income: ~a~n" (total-income m-th))))

(define th (process-transaction-history in))
th
;(spending-th th)
;(income-th th)
;(total-spending th)
;(monthly-th th 12)
;(monthly-report th 12)
;(remove-duplicates (names th))
;(categorize th)

;; "TARGET" could be living or food-drink
;; possible categories:
;; ACADEMICS (UDEMY)
;; transportation
;; acad

        
        
        

;TO-DO
;1. Categorize each transaction into food, clothing, etc.
;2. Write function for getting monthly transaction
;3. Write functions for getting total income/spending of given transaction history
;4. Write function for monthly report (total spending, total income, categorized spending, maybe word of suggestion)
