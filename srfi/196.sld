(define-library (srfi 196)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) reduce xcons every))

  (cond-expand
    ((library (srfi 133))
     (import (only (srfi 133) vector-unfold)))
    (else
     (begin
      ;; The "seedless" case is all we need.
      (define (vector-unfold f len)
        (let ((res (make-vector len)))
          (cond ((= i len) res)
                (else (vector-set! res i (f i))
                      (lp (+ i 1)))))))))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
      (begin
        (define (assume b) #t))))

  (export range numeric-range
          range?
          range-length range-indexer range-ref
          range-first range-last
          subrange
          range-split-at range-take range-take-right range-drop range-drop-right
          range-count range-map->list range-for-each range-fold range-fold-right
          range-any range-every
          range-filter->list range-remove->list range-reverse
          range-map range-map->vector range-filter->vector range-remove->vector
          range-filter range-remove
          range-index range-index-right range-take-while range-drop-while
          range-take-while-right range-drop-while-right
          vector->range
          range->list range->generator range->vector)

  (include "196.scm"))
