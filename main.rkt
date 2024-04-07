#lang racket

;; Structures
(struct account (number name balance) #:transparent)
(struct purchase (account-number timestamp merchant amount) #:transparent)
(struct payment (account-number timestamp type amount) #:transparent)

;; File Reading
(define (read-accounts filename)
  (call-with-input-file filename
    (lambda (in)
      (for/list ([line (in-lines in)])
        (define parts (string-split line))
        (account (string->number (car parts))
                 (string-trim (cadr parts) #rx"\"")
                 (string->number (caddr parts)))))))

(define (read-transactions filename)
  (call-with-input-file filename
    (lambda (in)
      (for/list ([line (in-lines in)])
        (define parts (string-split line))
        (cond
          [(string=? (car parts) "Purchase")
           (purchase (string->number (cadr parts))
                     (string->number (caddr parts))
                     (string-trim (cadddr parts) #rx"\"")
                     (string->number (car (last-pair parts))))]
          [else
           (payment (string->number (cadr parts))
                    (string->number (caddr parts))
                    (cadddr parts)
                    (string->number (car (last-pair parts))))])))))

;; Transaction Formatting
(define (format-transaction transaction)
  (match transaction
    [(purchase account-number timestamp merchant amount)
     (let ((merchant-string (string-trim (substring (string-trim merchant #rx"\"") 0 (min 40 (string-length (string-trim merchant #rx"\"")))) #rx" ")))
       (format "~a Purchase ~a ~a"
               timestamp
               merchant-string
               (number->string amount)))]
    [(payment account-number timestamp type amount)
     (format "~a Payment ~a ~a" timestamp type (number->string amount))]))

;; Helper Functions
(define (filter-account-transactions account transactions)
  (filter (lambda (t) (equal? (if (purchase? t) (purchase-account-number t) (payment-account-number t)) (account-number account))) transactions))

(define (calculate-total-purchases transactions)
  (foldl (lambda (p total) (+ total (or (purchase-amount p) 0))) 0 (filter purchase? transactions)))

(define (calculate-total-payments transactions)
  (foldl (lambda (p total) (+ total (or (payment-amount p) 0))) 0 (filter payment? transactions)))

(define (sort-transactions transactions)
  (sort transactions
        (lambda (a b)
          (< (if (purchase? a) (purchase-timestamp a) (payment-timestamp a))
             (if (purchase? b) (purchase-timestamp b) (payment-timestamp b))))))

;; Statement Generation
(define (generate-statement account transactions)
  (define account-transactions (filter-account-transactions account transactions))
  (define total-purchases (calculate-total-purchases account-transactions))
  (define total-payments (calculate-total-payments account-transactions))
  (define sorted-transactions (sort-transactions account-transactions))
  (list
   (format "STATEMENT OF ACCOUNT\n~a ~a Starting Balance: ~a\n"
           (account-number account)
           (account-name account)
           (or (account-balance account) 0))
   (string-join (map format-transaction sorted-transactions) "\n")
   (format "\nTotal Purchases: ~a" total-purchases)
   (format "Total Payments: ~a" total-payments)
   (format "Ending Balance: ~a" (+ (or (account-balance account) 0) total-purchases (- total-payments)))
   "\n********************************************************"))

(define (generate-statements accounts transactions)
  (map (lambda (account) (string-join (generate-statement account transactions) "\n")) accounts))

;; Main Function
(define (main)
  (define accounts (read-accounts "ACCOUNTS.TXT"))
  (define transactions (read-transactions "TRANSACTIONS.TXT"))
  (define statements (generate-statements accounts transactions))
  (with-handlers ([exn:fail? (lambda (e) (display (format "Error: ~a\n" (exn-message e))))])
    (if (file-exists? "STATEMENTS.TXT")
        (display "STATEMENTS.TXT already exists. Skipping file creation.")
        (call-with-output-file "STATEMENTS.TXT"
          (lambda (out)
            (for ([statement (in-list statements)])
              (displayln statement out)))))))

(main)
