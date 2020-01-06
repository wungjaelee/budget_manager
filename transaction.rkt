#lang racket
(require racket/pretty)
(require "constants.rkt")
(require "utils.rkt")

;; contracts
(define (transaction? t)
  (and (list? t)
       (= (length t) 5)))

(provide
 (contract-out
  [process-transaction-history (-> input-port? (listof transaction?))]
  [name (-> list? string?)]))

;; load global variables
; write your local path to US Bank transaction history csv file
(define temp-path "/Users/wungjaelee/Everything/가계부/dec1-jan05.csv")

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
            ;removing transaction-type, memo column
            (lambda (lst) (remove-nths lst 1 3))
             ;updating amount column to be number
            (lambda (lst) (list-update lst 2 string->number))
             ;removing unnecessary info from name column
            (lambda (lst) (list-update lst 1 remove-unnecessary-info)))
           line))


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

(define (sum-amount th)
  (apply + (map (lambda (t) (amount t)) th)))

(define (monthly-th th m)
  (filter (lambda (t) (= (month t) m)) th))

(define (cat? t-name cat-constant)
  (ormap (lambda (keyword)
           (string-contains? t-name keyword))
         cat-constant))


;; in need of constant modification
;; (if you modify constants.rkt, you need to modify this as well)
  (define (category t)
    (let ([t-name (name t)])
      (cond [(cat? t-name FOOD-DRINK) "FOOD-DRINK"]
            [(cat? t-name LEISURE) "LEISURE"]
            [(cat? t-name TRANSPORTATION) "TRANSPORTATION"]
            [(cat? t-name ACADEMICS) "ACADEMICS"]
            [(cat? t-name VENMO)
             (if (< (amount t) -50)
                 "VENMO"
                 "FOOD-DRINK")]
            [(cat? t-name TUITION) "TUITION"]
            [(cat? t-name MONTHLY-PAYMENT) "MONTHLY-PAYMENT"]
            [(cat? t-name GROCERIES) "GROCERIES"]
            [(cat? t-name SPORTS) "SPORTS"]
            [(cat? t-name CLOTHING) "CLOTHING"]
            [else "UNDETERMINED"])))

(define (build-categories th)
  (let ([ht (make-hash)])
    (for-each (lambda (t)
                (let ([cat (category t)])
                  (hash-update! ht cat (lambda (cat-ts) (cons t cat-ts)) '())))
              th)
    ht))


(define (print-spending-per-category cat-spending)
  (printf "~n######SPENDING PER CATEGORY#######~n")
  (for-each (lambda (cat)
                (printf "Money spent on ~a: ~a~n"
                        cat
                        (sum-amount (hash-ref cat-spending cat))))
              (hash-keys cat-spending))
  (printf "##################################~n~n"))


(define (monthly-report th m)
  (let* ([m-th (monthly-th th m)]
         [spending (spending-th m-th)]
         [income (income-th m-th)]
         [cat-spending (build-categories spending)])
    (printf "Monthly report for ~a~n" m)
    (printf "Total spending: ~a~n" (sum-amount spending))
    (printf "Spending without monthly payment: ~a~n"
            (- (sum-amount spending) (sum-amount (hash-ref cat-spending "MONTHLY-PAYMENT"))))
    (printf "Total income: ~a~n" (sum-amount income))
    (print-spending-per-category cat-spending)
    (pretty-print cat-spending)

    (printf "~n#####INCOME#####~n")
    (pretty-print income)
    (printf "################~n")

    (printf "~n#####UNDETERMINED SPENDING#####~n")
    (pretty-print (hash-ref cat-spending "UNDETERMINED"))
    (printf "###############################~n")))

(define (search th keyword)
  (filter (lambda (t)
            (string-contains? (string-upcase (name t))
                              (string-upcase keyword)))
          th))

(define th (process-transaction-history in))
;th
;(build-categories th) 
;(spending-th th)
;(income-th th)
;(total-spending th)
;(monthly-th th 12)
;(monthly-report th 9)
;(monthly-report th 10)
;(monthly-report th 11)
(monthly-report th 12)


;TO-DO
;1. detect abnormal behavior. For example, venmo spending > 100
;2. average spending per category

;CHALLENGES
;1. Graphic UI (with react?)
;2. Graphs
;3. Database instead of constants
;4. AI feature. Word of suggestion based on spending, abnormal behavior, preset goals, etc.