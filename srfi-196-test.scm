;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:

;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base))
(import (scheme write))
(import (srfi 1))
(import (srfi 196))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define *tests-failed* 0)
      (define-syntax check
        (syntax-rules (=>)
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report)
        (if (zero? *tests-failed*)
            (begin
             (display "All tests passed.")
             (newline))
            (begin
             (display "TESTS FAILED: ")
             (display *tests-failed*)
             (newline)))))))

(cond-expand
  ((library (srfi 158))
   (import (only (srfi 158) generator->list)))
  (else
   (begin
    (define (generator->list g)
      (let ((v (g)))
        (if (eof-object? v)
            '()
            (cons v (generator->list g))))))))

;;;; Utility

(define (identity x) x)

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define always (constantly #t))
(define never (constantly #f))

(define (%range-empty? r) (zero? (range-length r)))

;;;; Test ranges

(define test-num-range (numeric-range 10 30))

(define test-num-seq (iota 20 10))

(define test-empty-range (numeric-range 0 0))

;; Produces the range {#f, #t}.
(define test-bool-range
  (range 2 (lambda (n) (not (zero? n)))))

;;;; Conversion

;;;; Test these first, as range->list is used extensively in later tests.

(define (check-conversion)
  (print-header "Running conversion tests...")

  (check (range->list test-empty-range) => '())
  (check (range->list test-bool-range)  => '(#f #t))
  (check (range->list test-num-range)   => test-num-seq)

  (check (generator->list (range->generator test-num-range))
   => test-num-seq)

  (check (vector->list (range->vector test-num-range)) => test-num-seq))

(define (check-constructors)
  (print-header "Running constructor tests...")

  (check (range? (numeric-range 1 -5 -1))   => #t)
  (check (range? (numeric-range 1.3 5.3 1)) => #t)

  (check (range-length (numeric-range 0 9 4)) => 3))

;;;; Accessors

(define (check-accessors)
  (print-header "Running accessor tests...")

  (check (range-ref test-num-range 0)  => 10)
  (check (range-ref test-bool-range 1) => #t))

;;;; Iteration

(define (check-iteration)
  (print-header "Running iteration tests...")

  ;; Check lengths of ranges returned by range-split-at.
  (let ((n 10))
    (check (let-values (((ra rb) (range-split-at test-num-range n)))
             (list (range-length ra) (range-length rb)))
     => (list n (- (range-length test-num-range) n))))

  ;; Joining the two ranges returned by range-split-at gives the
  ;; original range.
  (check (let-values (((ra rb) (range-split-at test-bool-range 1)))
           (append (range->list ra) (range->list rb)))
   => (range->list test-bool-range))

  (check (range->list
          (subrange test-bool-range 0 (range-length test-bool-range)))
   => (range->list test-bool-range))
  (let ((a 5) (b 10))
    (check (= (range-length (subrange test-num-range a b)) (- b a))
     => #t)
    (check (equal?
            (range->list (subrange test-num-range a b))
            (range->list
             (range-take (range-drop test-num-range a) (- b a))))
     => #t)
    (check (equal?
            (range->list (subrange test-num-range 0 b))
            (range->list (range-take test-num-range b)))
     => #t)
    (check (equal?
            (range->list
             (subrange test-num-range a (range-length test-num-range)))
            (range->list (range-drop test-num-range a)))
     => #t))

  ;; range-take r n returns a range of length n.
  (check (range-length (range-take test-num-range 10)) => 10)
  (check (range-length
          (range-take test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take test-num-range 5))
   => (take test-num-seq 5))

  ;; range-take-right r n returns a range of length n.
  (check (range-length (range-take-right test-num-range 10)) => 10)
  (check (range-length
          (range-take-right test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take-right test-num-range 5))
   => (drop test-num-seq 15))

  ;; range-drop r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop test-num-range 15)) => (drop test-num-seq 15))

  ;; range-drop-right r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop-right test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop-right test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop-right test-num-range 15))
   => (take test-num-seq 5))

  (check (range-count always test-num-range) => (range-length test-num-range))
  (check (range-count never test-num-range)  => 0)
  (check (range-count even? test-num-range)  => (count even? test-num-seq))

  (check (range-any even? test-num-range) => #t)
  (check (range-any never test-num-range) => #f)

  (check (range-every number? test-num-range) => #t)
  (check (range-every even? test-num-range)   => #f)

  ;; (range-map->list f r) = (map f (range->list r))
  (let ((f not))
    (check (equal? (range-map->list f test-bool-range)
                   (map f (range->list test-bool-range)))
     => #t))

  (check (let ((v #f))
           (range-for-each (lambda (x) (set! v x)) test-bool-range)
           v)
   => #t)

  (check (equal? (range-filter->list always test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-filter->list never test-bool-range)) => #t)

  ;; (range-filter->list pred r) = (filter pred (range->list r))
  (let ((pred even?))
    (check (equal? (range-filter->list pred test-num-range)
                   (filter pred test-num-seq))
     => #t))

  (check (equal? (range-remove->list never test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-remove->list always test-bool-range)) => #t)

  ;; (range-remove->list pred r) = (remove pred (range->list r))
  (let ((pred even?))
    (check (equal? (range-remove->list pred test-num-range)
                   (remove pred test-num-seq))
     => #t))

  ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold (lambda (_ b) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold proc nil r) = (fold proc nil (range->list r))
  (let ((proc +) (nil 0))  ; sum over range
    (check (equal? (range-fold proc nil test-num-range)
                   (fold proc nil test-num-seq))
     => #t))

  ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold-right (lambda (_ b) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
  (let ((proc +) (nil 0))  ; sum over range
    (check (equal? (range-fold-right proc nil test-num-range)
                   (fold-right proc nil test-num-seq))
     => #t))

  (check (eqv? (range-start (range-reverse test-bool-range))
               (range-end test-bool-range))
   => #t)

  (check (eqv? (range-end (range-reverse test-bool-range))
               (range-start test-bool-range))
   => #t)

  (check (equal? (range->list (range-reverse test-num-range))
                 (reverse test-num-seq))
   => #t))

;;;; Searching

(define (check-searching)
  (print-header "Running search tests...")

  (check (range-index always test-num-range) => 0)
  (check (range-index never test-num-range)  => #f)
  (check (range-index values test-bool-range) => 1)

  (check (eqv? (range-index-right always test-num-range)
               (- (range-length test-num-range) 1))
   => #t)
  (check (range-index-right never test-num-range)  => #f)
  (check (range-index-right values test-bool-range) => 1)

  ;; range-index and range-index-right produce the same index if pred
  ;; is only satisfied by the element at that index.
  (let ((fifteen? (lambda (n) (= n 15))))
    (check (= (range-index fifteen? test-num-range)
              (range-index-right fifteen? test-num-range)
              (list-index fifteen? test-num-seq))
     => #t))

  ;; (range-take-while always r) = r
  (check (equal? (range->list (range-take-while always test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; (range-take-while never r) = [empty range]
  (check (%range-empty? (range-take-while never test-bool-range)) => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-take-while pred test-num-range))
     => (take-while pred test-num-seq)))

  ;; (range-drop-while always r) = [empty range]
  (check (%range-empty? (range-drop-while always test-bool-range)) => #t)

  ;; (range-drop-while never r) = r
  (check (equal? (range->list (range-drop-while never test-bool-range))
                 (range->list test-bool-range))
   => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-drop-while pred test-num-range))
     => (drop-while pred test-num-seq)))

  ;; Given a (non-existent) range-append function,
  ;;
  ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (equal?
            (append (range->list (range-take-while pred test-num-range))
                    (range->list (range-drop-while pred test-num-range)))
            test-num-seq)
     => #t))

  ;; (range-take-while-right always r) = r
  (check (equal? (range->list (range-take-while-right always test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; (range-take-while-right never r) = [empty range]
  (check (%range-empty? (range-take-while-right never test-bool-range)) => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-take-while-right pred test-num-range))
     => (iota 15 15)))

  ;; (range-drop-while-right always r) = [empty range]
  (check (%range-empty? (range-drop-while-right always test-bool-range)) => #t)

  ;; (range-drop-while-right never r) = r
  (check (equal? (range->list (range-drop-while-right never test-bool-range))
                 (range->list test-bool-range))
   => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-drop-while-right pred test-num-range))
     => (take test-num-seq 5)))

  ;; Given a (non-existent) range-append function,
  ;;
  ;; (range-append (range-drop-while-right p r)
  ;;               (range-take-while-right p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (equal?
            (append (range->list (range-drop-while-right pred test-num-range))
                    (range->list (range-take-while-right pred test-num-range)))
            test-num-seq)
     => #t)))

(define (check-all)
  (check-conversion)
  (check-constructors)
  (check-accessors)
  (check-iteration)
  (check-searching)

  (newline)
  (check-report))

(check-all)
