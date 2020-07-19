(define-library (srfi 196)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 128))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
      (begin
        (define (assume b) #t))))

  (cond-expand
    ((library (srfi 162))
     (import (srfi 162)))
    (else
     (begin
      (define real-comparator
        (make-comparator real? = < number-hash)))))

  (export range numeric-range
          range? range-contains? range-includes? range-empty?
          range-element-comparator range-length range-indexer range-ref
          range-start range-end
          range-split-at range-take range-take-right range-drop range-drop-right
          range-count range-map->list range-for-each range-fold range-fold-right
          range-any range-every
          range-filter->list range-remove->list range-reverse
          range-index range-index-right range-take-while range-drop-while
          range-take-while-right range-drop-while-right
          range->list range->generator range->vector)

  (include "196.scm"))
