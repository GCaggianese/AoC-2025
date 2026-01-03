#!/usr/bin/guile \
-e main -s
!#

;;; SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
;;;
;;; SPDX-License-Identifier: Apache-2.0

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

(define (merge-ranges ranges)
  "Merge overlapping/adjacent ranges"
  (if (null? ranges)
      '()
      (let* ((sorted (sort ranges (lambda (a b) (< (car a) (car b)))))
             (first (car sorted))
             (rest (cdr sorted)))
        (let loop ((current first) (remaining rest) (merged '()))
          (if (null? remaining)
              (reverse (cons current merged))
              (let* ((next (car remaining))
                     (start1 (car current))
                     (end1 (cadr current))
                     (start2 (car next))
                     (end2 (cadr next)))
                (if (<= start2 (+ end1 1))  ; overlapping or adjacent
                    (loop (list start1 (max end1 end2))
                          (cdr remaining)
                          merged)
                    (loop next
                          (cdr remaining)
                          (cons current merged)))))))))

(define (count-numbers-in-ranges ranges)
  "Count total unique numbers covered by ranges"
  (fold (lambda (range acc)
          (match range
            ((start end) (+ acc (- end start) 1))))
        0
        ranges))

(define (process-file filename)
  "Parse file and solve both parts"
  (let ((ranges '())
        (values-list '()))

    ;; Parse file
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

    ;; Part 1
    (let ((counter 0))
      (for-each (lambda (n)
                  (if (in-any-range? n ranges)
                      (begin
                        (set! counter (+ counter 1))
                        (format #t "Value ~a: TRUE (counter = ~a)\n" n counter))
                      (format #t "Value ~a: FALSE\n" n)))
                (reverse values-list))

      (format #t "\n=== Part 1 ===\n")
      (format #t "Final counter: ~a\n\n" counter)

      ;; Part 2
      (let ((merged (merge-ranges ranges)))
        (format #t "=== Part 2 ===\n")
        (format #t "Original ranges: ~a\n" (length ranges))
        (format #t "Merged ranges: ~a\n" (length merged))
        (format #t "Merged intervals: ~a\n" merged)
        (format #t "Total unique numbers: ~a\n"
                (count-numbers-in-ranges merged))))))

(define (main args)
  (if (< (length args) 2)
      (format #t "Usage: ~a <file>\n" (car args))
      (process-file (cadr args))))
