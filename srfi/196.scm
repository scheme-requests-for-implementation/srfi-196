;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

(define unspecified (if #f #f))

(define-record-type <range>
  (raw-range start-index length indexer)
  range?
  (start-index range-start-index)
  (length range-length)
  (indexer range-indexer))

;; Returns an empty range which is otherwise identical to r.
(define (%empty-range-from r)
  (raw-range (range-start-index r) 0 (range-indexer r)))

(define (%range-valid-index? r index)
  (and (exact-natural? index)
       (< index (range-length r))))

;; As the previous check, but bound is assumed to be exclusive.
(define (%range-valid-bound? r bound)
  (and (exact-natural? bound)
       (<= bound (range-length r))))

;;;; Constructors

;; The primary range constructor does some extra consistency checking.
(define (range length indexer)
  (assume (exact-natural? length))
  (assume (procedure? indexer))
  (raw-range 0 length indexer))

(define numeric-range
  (case-lambda
    ((start end) (numeric-range start end 1))
    ((start end step)
     (let ((len (exact (ceiling (/ (- end start) step)))))
       (assume (or (= start end)
                   ((if (< start end) < >) (+ start (* (- len 1) step))
                                           end))
               "numeric-range: computed length is invalid")
       (raw-range 0 (exact len) (lambda (n) (+ start (* n step))))))))

;;;; Accessors

(define (range-ref r index)
  (assume (%range-valid-index? r index) "range-ref: invalid index")
  ((range-indexer r) (+ index (range-start-index r))))

;; A portable implementation can't rely on inlining, but it
;; can rely on macros.
(define-syntax %range-ref-no-check
  (syntax-rules ()
    ((_ r index)
     ((range-indexer r) (+ index (range-start-index r))))))

(define (range-start r) (%range-ref-no-check r (range-start-index r)))

(define (range-end r) (%range-ref-no-check r (- (range-length r) 1)))

;;;; Iteration

(define (range-split-at r index)
  (assume (range? r))
  (assume (%range-valid-bound? r index))
  (cond ((= index 0) (values (%empty-range-from r) r))
        ((= index (range-length r)) (values r (%empty-range-from r)))
        (else
         (values (raw-range (range-start-index r) index (range-indexer r))
                 (raw-range index
                            (- (range-length r) index)
                            (range-indexer r))))))

(define (subrange r start end)
  (assume (range? r))
  (assume (%range-valid-index? r start) "subrange: invalid start index")
  (assume (%range-valid-bound? r end) "subrange: invalid end index")
  (if (and (zero? start) (= end (range-length r)))
      r
      (raw-range (+ (range-start-index r) start)
                 (- end start)
                 (range-indexer r))))

(define (range-take r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-take: invalid count")
  (if (zero? count)
      (%empty-range-from r)
      (raw-range (range-start-index r) count (range-indexer r))))

(define (range-take-right r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count)
          "range-take-right: invalid count")
  (if (zero? count)
      (%empty-range-from r)
      (raw-range (+ (range-start-index r) (- (range-length r) count))
                 count
                 (range-indexer r))))

(define (range-drop r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (+ (range-start-index r) count)
                 (- (range-length r) count)
                 (range-indexer r))))

(define (range-drop-right r count)
  (assume (range? r))
  (assume (%range-valid-bound? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (range-start-index r)
                 (- (range-length r) count)
                 (range-indexer r))))

(define (range-count pred r)
  (assume (procedure? pred))
  (range-fold (lambda (x c) (if (pred x) (+ c 1) c)) 0 r))

(define (range-any pred r)
  (assume (procedure? pred))
  (range-fold (lambda (x last) (or (pred x) last)) #f r))

(define (range-every pred r)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (range-fold (lambda (x _) (or (pred x) (return #f))) #t r))))

(define (range-map->list proc r)
  (assume (procedure? proc))
  (range-fold-right (lambda (elem xs) (cons (proc elem) xs))
                    '()
                    r))

(define (range-for-each proc r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0))
      (if (>= i len)
          unspecified
          (begin
           (proc (%range-ref-no-check r i))
           (lp (+ i 1)))))))

(define (range-fold proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0) (acc nil))
      (if (>= i len)
          acc
          (lp (+ i 1) (proc (%range-ref-no-check r i) acc))))))

(define (range-fold-right proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let rec ((i 0))
      (if (>= i len)
          nil
          (proc (%range-ref-no-check r i) (rec (+ i 1)))))))

(define (range-filter->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (x xs)
                      (if (pred x) (cons x xs) xs))
                    '()
                    r))

(define (range-remove->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (x xs)
                      (if (pred x) xs (cons x xs)))
                    '()
                    r))

(define (range-reverse r)
  (assume (range? r))
  (raw-range (range-start-index r)
             (range-length r)
             (lambda (n)
               ((range-indexer r) (- (range-length r) 1 n)))))

;;;; Searching

(define (range-index pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0))
      (cond ((>= i len) #f)
            ((pred (%range-ref-no-check r i)) i)
            (else (lp (+ i 1)))))))

(define (range-index-right pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (let lp ((i (- (range-length r) 1)))
    (cond ((< i 0) #f)
          ((pred (%range-ref-no-check r i)) i)
          (else (lp (- i 1))))))

(define (range-take-while pred r)
  (let ((count (range-index (lambda (x) (not (pred x))) r)))
    (if count (range-take r count) r)))

(define (range-take-while-right pred r)
  (let ((idx (range-index-right (lambda (x) (not (pred x))) r)))
    (if idx (range-take-right r (- (range-length r) 1 idx)) r)))

(define (range-drop-while pred r)
  (let ((count (range-index (lambda (x) (not (pred x))) r)))
    (if count (range-drop r count) (%empty-range-from r))))

(define (range-drop-while-right pred r)
  (let ((idx (range-index-right (lambda (x) (not (pred x))) r)))
    (if idx
        (range-drop-right r (- (range-length r) 1 idx))
        (%empty-range-from r))))

;;;; Conversion

(define (range->list r)
  (assume (range? r))
  (range-fold-right cons '() r))

(define (range->vector r)
  (assume (range? r))
  (let* ((len (range-length r))
         (vec (make-vector len)))
    (let lp ((i 0))
      (if (= i len)
          vec
          (begin
           (vector-set! vec i (%range-ref-no-check r i))
           (lp (+ i 1)))))))

(define (range->generator r)
  (assume (range? r))
  (let ((i 0) (len (range-length r)))
    (lambda ()
      (if (>= i len)
          (eof-object)
          (begin
           (let ((v (%range-ref-no-check r i)))
             (set! i (+ i 1))
             v))))))
