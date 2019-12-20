#lang racket

; section 1: the contract definitions
(define msg-in-bound "index n within bound [0,~a] expected")

(define (mk-index-contract lst msg)
  (define (ctr n)
    (and (natural-number/c n) (< n (length lst))))
  (flat-named-contract (format msg (sub1 (length lst))) ctr))

; section 2: the exports
(provide
 (contract-out
  [remove-nth
   (->i ([lst list?]
         [n (lst) (mk-index-contract lst msg-in-bound)])
        [result (lst)
                (and/c list?
                       (lambda (res)
                         (= (length res) (sub1 (length lst)))))])]
  
  ;; this is a weak contract, since obj might not match function argument type
  [compose (-> (listof procedure?) any/c any)]

  ;; also weak contract
  ;; 1. does not check whether given indices are in-bound
  ;; 2. does not check whether resulting list has n fewer elements than the original list
  [remove-nths (->* (list? natural-number/c) () #:rest (listof natural-number/c)
                    list?)]))

; section 3: the function definitions
(define (remove-nth lst n)
  (define (remove-nth-helper lst n accum-lst)
    (cond [(null? lst) (reverse accum-lst)]
          [(= n 0) (remove-nth-helper (cdr lst) (sub1 n) accum-lst)]
          [else (remove-nth-helper (cdr lst) (sub1 n) (cons (car lst) accum-lst))]))
  (remove-nth-helper lst n '()))

(define (remove-nths lst index . indices)
  (define (helper lst indices-to-remove accum-lst curr-idx)
    (cond [(null? lst) (reverse accum-lst)]
          [(null? indices-to-remove) (append (reverse accum-lst) lst)]
          [(= (car indices-to-remove) curr-idx)
           (helper (cdr lst) (cdr indices-to-remove) accum-lst (add1 curr-idx))]
          [else (helper (cdr lst) indices-to-remove (cons (car lst) accum-lst) (add1 curr-idx))]))
  (helper lst (sort (cons index indices) <) '() 0))

(define (compose fs obj)
  (cond [(null? fs) obj]
        [else (compose (cdr fs) ((car fs) obj))]))