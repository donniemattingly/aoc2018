#lang racket

(require racket/file)
(require math)

(struct claim (id lOff tOff w h))

(define (parse-id id-str)
  (string-replace id-str "#" ""))

(define (parse-offsets off-str)
  (map string->number (string-split (string-replace off-str ":" "") ",")))

(define (parse-size size-str)
  (map string->number (string-split size-str "x")))

(define (parse-row row)
  (let ([srow (string-split row)])
    (let (
          [id (parse-id (list-ref srow 0))] 
          [offsets (parse-offsets (list-ref srow 2))] 
          [size (parse-size (list-ref srow 3))])
      (claim id (car offsets) (cdr offsets) (car size) (cdr size)))))

(define (input)
  (file->lines "../inputs/input-3.1.txt"))

(define (parsed-input)
  (map parse-row (input)))

(define (max-claim-field-by claims max-fn) 
  (apply max (map max-fn claims)))

(define (max-loffset claims)
  (apply max (map claim-lOff claims)))

(define (part-one input)
  0 )

