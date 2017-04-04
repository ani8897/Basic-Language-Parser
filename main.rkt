#lang racket
(provide gnode ident num pred-p single-digit-p single-alphabet-p
         seq alt epsilon-p whitespace-p number-p variable-p term-p
         identifier-p expression-p assignment-p zero-or-more
         one-or-more single-space-p collector combine-cc
         combine-cs combine-sc combine-ss)

;;;;;;;;;;;;;;;;;; Structures ;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

;;;;;;;;;;;;;;;;;;;;; Atomic functions ;;;;;;;;;;;;;;;;;;;

(define (pred-p p)
  (lambda (str)
    (if (and (not (null? (string->list str))) (p (string-ref str 0)))
        (cons (string-ref str 0) (substring str 1))
        'fail )))

(define single-digit-p
  (lambda (str)
    (define (is-digit c)
      (define number (char->integer c))
      (if (and (> number 47) (< number 58)) #t #f))
    ((pred-p is-digit) str)))


(define single-alphabet-p
  (lambda (str)
    (define (is-alphabet c)
      (define number (char->integer c))
      (if (and (> number 96) (< number 123)) #t #f))
    ((pred-p is-alphabet) str)))

(define (seq p1 p2 f)
  (lambda (str)
    (if (eq? (p1 str) 'fail) 'fail
        (let*[(pair (p1 str))
              (new-str (cdr pair))
              (to-merge (car pair))
              (new-pair (p2 new-str))]
          (if (eq? new-pair 'fail) 'fail
              (cons (f to-merge (car new-pair)) (cdr new-pair)))))))

(define (alt p1 p2)
  (lambda (str)
    (define pair-1 (p1 str))
    (define pair-2 (p2 str))
    (cond
      [(not (eq? pair-1 'fail)) pair-1]
      [(not (eq? pair-2 'fail)) pair-2]
      [else 'fail])))

(define epsilon-p
  (lambda (str)
    (cons "" str)))

(define (zero-or-more p f)
  (lambda (str)
    (define pair (p str))
    (if (eq? pair 'fail) (epsilon-p str)
        (let* [(z-pair ((zero-or-more p f) (cdr pair)))]
          (cons (f (car pair) (car z-pair)) (cdr z-pair))))))

(define (one-or-more p f)
  (lambda (str)
    (define pair (p str))
    (if (eq? pair 'fail) 'fail
        ((zero-or-more p f) str))))

;;;;;;;;;;;;;;;;;;;;;;;; Removing Whitespaces ;;;;;;;;;;;;;;;;;;

(define single-space-p
  (lambda(str)
    (define (is-space c) (if (eq? c #\space) #t #f))
    ((pred-p is-space) str)))

(define whitespace-p
  (lambda(str)
    (define pair ((zero-or-more single-space-p combine-cs) str))
    (epsilon-p (cdr pair))))

;;;;;;;;;;;;;;;;;;;;;; Number Parser ;;;;;;;;;;;;;;;;;;;;;;;;

(define number-p
  (lambda (str)
    (define no-space-str (cdr (whitespace-p str)))
    (define str-number-pair ((one-or-more single-digit-p combine-cs) no-space-str))
    (if (eq? str-number-pair 'fail) 'fail
        (cons (num (string->number (car str-number-pair))) (cdr str-number-pair)))))

;;;;;;;;;;;;;;;;;;;;; Identifier Parser ;;;;;;;;;;;;;;;;;;;;;;;;

(define identifier-p
  (lambda (str)
    (define no-space-str (cdr (whitespace-p str)))
    (if (eq? (single-alphabet-p no-space-str) 'fail) 'fail
        (collector (epsilon-p no-space-str)))))

(define collector
  (lambda (pair)
    (define collected (car pair))
    (define str (cdr pair))
    (if (not (eq? (single-digit-p str) 'fail))
        (let*[(z-pair ((zero-or-more single-digit-p combine-cs) str))
              (car-n-pair (combine-ss collected (car z-pair)))
              (cdr-n-pair (cdr z-pair))
              (n-pair (cons car-n-pair cdr-n-pair))]
          (collector n-pair))
        (if (not (eq? (single-alphabet-p str) 'fail))
            (let*[(z-pair ((zero-or-more single-alphabet-p combine-cs) str))
              (car-n-pair (combine-ss collected (car z-pair)))
              (cdr-n-pair (cdr z-pair))
              (n-pair (cons car-n-pair cdr-n-pair))]
          (collector n-pair))
            (cons (ident collected) str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Variable Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define variable-p
  (lambda (str)
    (if (eq? (identifier-p str) 'fail) 'fail
        (let*([pair (identifier-p str)]
              [rem-str (cdr (whitespace-p(cdr pair)))]
              [check-open-brack (pred-p (lambda(c) (if (eq? c #\[ ) #t #f)))]
              [check-closed-brack (pred-p (lambda(c) (if (eq? c #\] ) #t #f)))]
              [brac-free (check-open-brack rem-str)])
          (if (eq? brac-free 'fail) pair
              (let*([brac-free-rem-str (cdr (whitespace-p (cdr brac-free)))]
                    [expression-parse (expression-p brac-free-rem-str)])
                (if (or (eq? expression-parse 'fail)
                        (eq? (check-closed-brack (cdr (whitespace-p (cdr expression-parse)))) 'fail)) 'fail
                    (cons (gnode 'ARRAY (list (car pair) (car expression-parse)))
                          (cdr (check-closed-brack (cdr (whitespace-p (cdr expression-parse)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Term Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define term-p
  (lambda (str)
    (if (not (eq? (number-p str) 'fail)) (number-p str)
        (if (not (eq? (variable-p str) 'fail)) (variable-p str)
            (let*([check-open-brack (pred-p (lambda(c) (if (eq? c #\( ) #t #f)))]
                  [check-closed-brack (pred-p (lambda(c) (if (eq? c #\) ) #t #f)))]
                  [brac-free (check-open-brack (cdr (whitespace-p str)))])
              (if (eq? brac-free 'fail) 'fail
                  (let*([expression-parse (expression-p (cdr (whitespace-p (cdr brac-free))))])
                    (if (or (eq? expression-parse 'fail)
                            (eq? (check-closed-brack (cdr (whitespace-p (cdr expression-parse)))) 'fail)) 'fail
                        (cons (car expression-parse)
                              (cdr (check-closed-brack (cdr (whitespace-p (cdr expression-parse))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Expression Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression-p
  (lambda (str)
    (let*([t-pair (term-p str)])
      (if (eq? t-pair 'fail) 'fail
          (let* ([rem-str (cdr (whitespace-p (cdr t-pair)))]
                 [check-plus (pred-p (lambda(c) (if (eq? c #\+ ) #t #f)))]
                 [plus-free (check-plus rem-str)])
            (if (eq? plus-free 'fail) t-pair
                (let* ([plus-free-rem-str (cdr (whitespace-p (cdr plus-free)))]
                       [expression-parse (expression-p plus-free-rem-str)])
                  (if (eq? expression-parse 'fail) 'fail
                      (cons (gnode 'PLUS (list (car t-pair) (car expression-parse)))
                            (cdr expression-parse))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Assignment Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assignment-p
  (lambda (str)
    (if (eq? (variable-p str) 'fail) 'fail
        (let*([pair (variable-p str)]
              [var-part (car pair)]
              [check-equal (pred-p (lambda(c) (if (eq? c #\= ) #t #f)))]
              [rem-str (cdr (whitespace-p (cdr pair)))]
              [equal-free (check-equal rem-str)])
          (if (eq? equal-free 'fail) 'fail
              (let*([expression-parse (expression-p (cdr (whitespace-p (cdr equal-free))))])
                (if (eq? expression-parse 'fail) 'fail
                    (cons (gnode 'ASSIGN (list var-part (car expression-parse)))
                          (cdr expression-parse)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;Helper;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (combine-cc char1 char2)
  (list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))