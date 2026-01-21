(define (string-index str char . rest)
  (let ((start (if (null? rest) 0 (car rest))))
    (let loop ((i start))
      (cond ((>= i (string-length str)) #f)
            ((char=? (string-ref str i) char) i)
            (else (loop (+ i 1)))))))

(define (string->number-or-self s)
  (let ((n (string->number s)))
    (if n n s)))

(define (split-and-convert line delim)
  (map string->number-or-self
       (let loop ((start 0) (pos (string-index line delim)) (out '()))
         (if pos
             (loop (+ pos 1)
                   (string-index line delim (+ pos 1))
                   (cons (substring line start pos) out))
             (reverse (cons (substring line start (string-length line)) out))))))

(define (parse-csv path)
  (map (lambda (ln) (split-and-convert ln #\,))
       (read-file-string-list path)))

;; (define parsed-csv (parse-csv "D9/input_test.txt" ))

(define (square x)
  (* x x))

(define (vectorial-distance v1 v2)
  (sqrt (+
         (square
          (- (car v1) (car v2)))
         (square
          (- (cadr v1) (cadr v2)))) ))

(define (argmax2d matrix)
  (let ((maxval -inf.0) (mi 0) (mj 0))
    (do ((i 0 (+ i 1))) ((= i (length matrix)) (list mi mj))
      (do ((j 0 (+ j 1))) ((= j (length (list-ref matrix i))))
        (let ((v (list-ref (list-ref matrix i) j)))
          (when (> v maxval)
            (set! maxval v)
            (set! mi i)
            (set! mj j)))))))

(define (main . args)

  (define parsed-csv (parse-csv "input.txt"))

  (define dists
    (map (lambda (v1)
           (map (lambda (v2) (vectorial-distance v1 v2))
                parsed-csv))
         parsed-csv))

  (define (rectangle-area v1 v2)
    (* (+ 1 (abs (- (car v1) (car v2))))
       (+ 1 (abs (- (cadr v1) (cadr v2))))))

  (define areas
    (map (lambda (v1)
           (map (lambda (v2) (rectangle-area v1 v2))
                parsed-csv))
         parsed-csv))

  (define idxs (argmax2d areas))
  (define i (car idxs))
  (define j (cadr idxs))
  (define v1 (list-ref parsed-csv i))
  (define v2 (list-ref parsed-csv j))

  (display (rectangle-area v1 v2))
  (newline)
  (force-output)
  (exit 0))

(main)
