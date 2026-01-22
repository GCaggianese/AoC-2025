;;; SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
;;;
;;; SPDX-License-Identifier: Apache-2.0

;; Utilities
(define (filter pred lst)
  (let loop ((l lst) (acc '()))
    (cond ((null? l) (reverse acc))
          ((pred (car l)) (loop (cdr l) (cons (car l) acc)))
          (else (loop (cdr l) acc)))))

(define (string-index str char . rest)
  (let ((start (if (null? rest) 0 (car rest))))
    (let loop ((i start))
      (cond ((>= i (string-length str)) #f)
            ((char=? (string-ref str i) char) i)
            (else (loop (+ i 1)))))))

(define (string->number-or-self s)
  (let ((n (string->number s))) (if n n s)))

(define (split-and-convert line delim)
  (map string->number-or-self
       (let loop ((start 0) (pos (string-index line delim)) (out '()))
         (if pos
             (loop (+ pos 1) (string-index line delim (+ pos 1)) (cons (substring line start pos) out))
             (reverse (cons (substring line start (string-length line)) out))))))

(define (read-file-string-list path)
  (with-input-from-file path
    (lambda ()
      (let loop ((lines '()) (line (read-line)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines) (read-line)))))))

(define (parse-csv path)
  (map (lambda (ln) (split-and-convert ln #\,)) (read-file-string-list path)))

;; --- POLYGON BUILDER ---
(define (build-polygon-from-path points)
  (let* ((n (length points))
         (vec (list->vector points)))
    (let loop ((i 0) (segs '()))
      (if (= i n)
          (reverse segs)
          (let* ((p1 (vector-ref vec i))
                 (p2 (vector-ref vec (modulo (+ i 1) n)))
                 (x1 (car p1)) (y1 (cadr p1))
                 (x2 (car p2)) (y2 (cadr p2)))
            (cond
             ((= y1 y2) (loop (+ i 1) (cons (list 'h p1 p2) segs)))
             ((= x1 x2) (loop (+ i 1) (cons (list 'v p1 p2) segs)))
             (else (error "Diagonal segment detected!" p1 p2))))))))

;; --- GEOMETRY CHECKS ---

(define (on-segment? x y segments)
  (let loop ((segs segments))
    (if (null? segs)
        #f
        (let* ((seg (car segs))
               (type (car seg))
               (p1 (cadr seg))
               (p2 (caddr seg)))
          (if (eq? type 'h)
              (let ((sy (cadr p1))
                    (min-x (min (car p1) (car p2)))
                    (max-x (max (car p1) (car p2))))
                (if (and (= y sy) (>= x min-x) (<= x max-x))
                    #t
                    (loop (cdr segs))))
              (let ((sx (car p1))
                    (min-y (min (cadr p1) (cadr p2)))
                    (max-y (max (cadr p1) (cadr p2))))
                (if (and (= x sx) (>= y min-y) (<= y max-y))
                    #t
                    (loop (cdr segs)))))))))

(define (point-in-polygon? x y segments)
  (if (on-segment? x y segments)
      #t
      (let loop ((segs segments) (count 0))
        (if (null? segs)
            (odd? count)
            (let* ((seg (car segs))
                   (type (car seg))
                   (p1 (cadr seg))
                   (p2 (caddr seg)))
              (if (eq? type 'v)
                  (let ((sx (car p1))
                        (sy1 (min (cadr p1) (cadr p2)))
                        (sy2 (max (cadr p1) (cadr p2))))
                    (if (and (> sx x) (>= y sy1) (< y sy2))
                        (loop (cdr segs) (+ count 1))
                        (loop (cdr segs) count)))
                  (loop (cdr segs) count)))))))

(define (rectangle-valid? x1 y1 x2 y2 segments)
  (let ((min-x (min x1 x2))
        (max-x (max x1 x2))
        (min-y (min y1 y2))
        (max-y (max y1 y2)))
    (and (point-in-polygon? min-x min-y segments)
         (point-in-polygon? max-x min-y segments)
         (point-in-polygon? min-x max-y segments)
         (point-in-polygon? max-x max-y segments)
         (let segment-check ((segs segments))
           (if (null? segs)
               #t
               (let* ((seg (car segs))
                      (type (car seg))
                      (p1 (cadr seg))
                      (p2 (caddr seg)))
                 (if (eq? type 'h)
                     (let ((sy (cadr p1))
                           (sx1 (min (car p1) (car p2)))
                           (sx2 (max (car p1) (car p2))))
                       (if (and (> sy min-y) (< sy max-y)
                                (< sx1 max-x) (> sx2 min-x))
                           #f
                           (segment-check (cdr segs))))
                     (let ((sx (car p1))
                           (sy1 (min (cadr p1) (cadr p2)))
                           (sy2 (max (cadr p1) (cadr p2))))
                       (if (and (> sx min-x) (< sx max-x)
                                (< sy1 max-y) (> sy2 min-y))
                           #f
                           (segment-check (cdr segs)))))))))))

(define (rectangle-area x1 y1 x2 y2)
  (* (+ 1 (abs (- x1 x2))) (+ 1 (abs (- y1 y2)))))

;; --- PART 1: Simple maximum rectangle (any two red tiles) ---
(define (find-max-rectangle-simple points)
  (let ((vec (list->vector points))
        (len (length points)))
    (let loop-i ((i 0) (max-area 0))
      (if (>= i len)
          max-area
          (let ((p1 (vector-ref vec i)))
            (let loop-j ((j (+ i 1)) (cur-max max-area))
              (if (>= j len)
                  (loop-i (+ i 1) cur-max)
                  (let* ((p2 (vector-ref vec j))
                         (x1 (car p1)) (y1 (cadr p1))
                         (x2 (car p2)) (y2 (cadr p2))
                         (area (rectangle-area x1 y1 x2 y2)))
                    (loop-j (+ j 1) (max area cur-max))))))))))

;; --- PART 2: Maximum rectangle with polygon constraints ---
(define (find-max-rectangle-constrained points segments)
  (let ((vec (list->vector points))
        (len (length points)))
    (let loop-i ((i 0) (max-area 0))
      (if (>= i len)
          max-area
          (let ((p1 (vector-ref vec i)))
            (let loop-j ((j (+ i 1)) (cur-max max-area))
              (if (>= j len)
                  (loop-i (+ i 1) cur-max)
                  (let* ((p2 (vector-ref vec j))
                         (x1 (car p1)) (y1 (cadr p1))
                         (x2 (car p2)) (y2 (cadr p2))
                         (area (rectangle-area x1 y1 x2 y2)))
                    (if (and (> area cur-max)
                             (rectangle-valid? x1 y1 x2 y2 segments))
                        (loop-j (+ j 1) area)
                        (loop-j (+ j 1) cur-max))))))))))

(define (main . args)
  (define input-file (if (null? args) "input.txt" (car args)))
  (define parsed-csv (parse-csv input-file))
  (define polygon (build-polygon-from-path parsed-csv))

  (display "Points: ") (display (length parsed-csv)) (newline)
  (display "Polygon segments: ") (display (length polygon)) (newline)
  (newline)

  ;; Part 1
  (display "Part 1: Finding maximum rectangle (any two red tiles)...") (newline)
  (let ((max-area-1 (find-max-rectangle-simple parsed-csv)))
    (display "Part 1 Answer: ") (display max-area-1) (newline))
  (newline)

  ;; Part 2
  (display "Part 2: Finding maximum rectangle (with polygon constraints)...") (newline)
  (let ((max-area-2 (find-max-rectangle-constrained parsed-csv polygon)))
    (display "Part 2 Answer: ") (display max-area-2) (newline)))

(main)
