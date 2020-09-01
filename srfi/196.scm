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
     (assume (not (zero? step)) "numeric-range: zero-valued step")
     (let ((len (exact (ceiling (max 0 (/ (- end start) step))))))
       ;; Try to ensure that we can compute a correct range from the
       ;; given parameters, i.e. one not plagued by roundoff errors.
       (assume (cond ((and (positive? step) (< start end))
                      (and (> (+ start step) start)
                           (< (+ start (* (- len 1) step)) end)))
                     ((and (negative? step) (> start end))
                      (and (< (+ start step) start)
                           (> (+ start (* (- len 1) step)) end)))
                     (else #t))
               "numeric-range: invalid parameters")
       (raw-range 0 len (lambda (n) (+ start (* n step))))))))

;;;; Accessors

(define (range-ref r index)
  (assume (range? r))
  (assume (%range-valid-index? r index) "range-ref: invalid index")
  ((range-indexer r) (+ index (range-start-index r))))

;; A portable implementation can't rely on inlining, but it
;; can rely on macros.
(define-syntax %range-ref-no-check
  (syntax-rules ()
    ((_ r index)
     ((range-indexer r) (+ index (range-start-index r))))))

(define (range-first r) (%range-ref-no-check r (range-start-index r)))

(define (range-last r) (%range-ref-no-check r (- (range-length r) 1)))

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
  (assume (not (negative? (- end start))) "subrange: invalid subrange")
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

(define (range-count pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-1 (lambda (c x) (if (pred x) (+ c 1) c)) 0 r)
      (apply range-fold                 ; variadic path
             (lambda (c . xs)
               (if (apply pred xs) (+ c 1) c))
             0
             r
             rs)))

(define (range-any pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-1 (lambda (last x) (or (pred x) last)) #f r)
      (apply range-fold                 ; variadic path
             (lambda (last . xs) (or (apply pred xs) last))
             #f
             r
             rs)))

(define (range-every pred r . rs)
  (assume (procedure? pred))
  (assume (range? r))
  (call-with-current-continuation
   (lambda (return)
     (if (null? rs)                     ; one-range fast path
         (%range-fold-1 (lambda (_ x) (or (pred x) (return #f))) #t r)
         (apply range-fold              ; variadic path
                (lambda (_ . xs) (or (apply pred xs) (return #f)))
                #t
                r
                rs)))))

(define (range-map proc . rs)
  (assume (pair? rs))
  (vector->range (apply range-map->vector proc rs)))

(define (range-map->list proc r . rs)
  (assume (procedure? proc))
  (if (null? rs)                        ; one-range fast path
      (%range-fold-right-1 (lambda (res x) (cons (proc x) res)) '() r)
      (apply range-fold-right           ; variadic path
             (lambda (res . xs) (cons (apply proc xs) res))
             '()
             r
             rs)))

(define (range-map->vector proc r . rs)
  (assume (procedure? proc))
  (assume (range? r))
  (if (null? rs)                        ; one-range fast path
      (vector-unfold (lambda (i) (proc (%range-ref-no-check r i)))
		     (range-length r))
      (let ((rs* (cons r rs)))          ; variadic path
        (vector-unfold (lambda (i)
                         (apply proc (map (lambda (r)
                                            (%range-ref-no-check r i))
                                          rs*)))
                       (reduce max 0 (map range-length rs*))))))

(define (range-for-each proc r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0))
      (if (>= i len)
          (if #f #f)
          (begin
           (proc (%range-ref-no-check r i))
           (lp (+ i 1)))))))

(define (%range-fold-1 proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0) (acc nil))
      (if (= i len)
          acc
          (lp (+ i 1) (proc acc (%range-ref-no-check r i)))))))

(define range-fold
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     (assume (procedure? proc))
     (assume (pair? rs))
     (let ((len (reduce max 0 (map range-length rs))))
       (let lp ((i 0) (acc nil))
         (if (= i len)
             acc
             (lp (+ i 1)
                 (apply proc acc (map (lambda (r)
                                        (%range-ref-no-check r i))
                                      rs)))))))))

(define (%range-fold-right-1 proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let rec ((i 0))
      (if (= i len)
          nil
          (proc (rec (+ i 1)) (%range-ref-no-check r i))))))

(define range-fold-right
  (case-lambda
    ((proc nil r)                       ; one-range fast path
     (%range-fold-right-1 proc nil r))
    ((proc nil . rs)                    ; variadic path
     (assume (procedure? proc))
     (assume (pair? rs))
     (let ((len (reduce max 0 (map range-length rs))))
       (let rec ((i 0))
         (if (= i len)
             nil
             (apply proc
                    (rec (+ i 1))
                    (map (lambda (r) (%range-ref-no-check r i)) rs))))))))

(define (range-filter pred r)
  (vector->range (range-filter->vector pred r)))

(define (range-filter->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) (cons x xs) xs))
                    '()
                    r))

(define (range-filter->vector pred r)
  (list->vector (range-filter->list pred r)))

(define (range-remove pred r)
  (vector->range (range-remove->vector pred r)))

(define (range-remove->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (xs x)
                      (if (pred x) xs (cons x xs)))
                    '()
                    r))

(define (range-remove->vector pred r)
  (list->vector (range-remove->list pred r)))

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
  (range-fold-right xcons '() r))

(define (range->vector r)
  (assume (range? r))
  (vector-unfold (lambda (i) (%range-ref-no-check r i))
                 (range-length r)))

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

(define (vector->range vec)
  (assume (vector? vec))
  (raw-range 0 (vector-length vec) (lambda (i) (vector-ref vec i))))
