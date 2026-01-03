#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1))

(define (parse-line line)
  (let ((trimmed (string-trim-both line)))
    (cond
     ((string-null? trimmed) #f)
     ((string-contains trimmed "-")
      (let ((parts (string-split trimmed #\-)))
        (match parts
          ((a b) `(range ,(string->number a) ,(string->number b)))
          (_ #f))))
     (else
      (let ((n (string->number trimmed)))
        (if n `(value ,n) #f))))))

(define (in-any-range? n ranges)
  "Check if n is within any range (inclusive)"
  (any (lambda (range)
         (match range
           ((start end) (and (>= n start) (<= n end)))))
       ranges))

(define (process-file filename)
  "Store ranges as pairs, check values against them"
  (let ((ranges '())
        (values-list '()))

    ;; Pass 1: Collect ranges and values
    (call-with-input-file filename
      (lambda (port)
        (let loop ()
          (let ((line (get-line port)))
            (unless (eof-object? line)
              (let ((parsed (parse-line line)))
                (when parsed
                  (match parsed
                    (('range start end)
                     (set! ranges (cons (list start end) ranges)))
                    (('value n)
                     (set! values-list (cons n values-list)))
                    (_ (void)))))
              (loop))))))

    (format #t "Loaded ~a ranges\n" (length ranges))
    (format #t "Checking ~a values...\n\n" (length values-list))

    ;; Pass 2: Check each value against all ranges
    (let ((counter 0))
      (for-each (lambda (n)
                  (if (in-any-range? n ranges)
                      (begin
                        (set! counter (+ counter 1))
                        (format #t "Value ~a: TRUE (counter = ~a)\n" n counter))
                      (format #t "Value ~a: FALSE\n" n)))
                (reverse values-list))

      counter)))

(define (main args)
  (if (< (length args) 2)
      (format #t "Usage: ~a <file>\n" (car args))
      (let* ((filename (cadr args))
             (counter (process-file filename)))
        (format #t "\nFinal counter: ~a\n" counter))))
