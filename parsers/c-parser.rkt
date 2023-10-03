#lang racket

;; TODO:
;; make inline code (i.e. code inside /* */ work properly
;;
(provide c->html)

(define (wrap-p text)
  (string-append "<p>" (list->string text) "</p>"))

(define (wrap-code text)
  (string-append "<code>" (list->string text) "</code>"))


;; simple lr(1) parser builds the string one character at at time
;; long comments (i.e. text inside "/*...*/" are turned into html
;; text surrounded by the <p> tag. otherwise the html text emitted
;; will be surrouned by <code> tags
;; 
(define (peek-expecting toks expected)
  (if (eq? expected (car (cdr toks)))
      true
      (begin 
        false)))

(define (parse-block-text input-toks output-string)
  (if (equal? #t (null? input-toks))
      (list '() output-string)
      (begin 
        (if (and (eq? #\* (car input-toks)) (peek-expecting input-toks #\/))
            (list (cdr (cdr input-toks)) output-string)
            (parse-block-text (cdr input-toks)
                              (append output-string (list(car input-toks))))))))
              
(define (parse-block-code input-toks output-string)
  (if (equal? #t (null? input-toks))
      (list '() output-string)
      (begin 
        (if (and (eq? #\/ (car input-toks)) (peek-expecting input-toks #\*))
            (list input-toks output-string)
            (parse-block-code (cdr input-toks)
                              (append output-string (list(car input-toks))))))))

(define (parse input-toks output-string)
  (if (equal? #t (null? input-toks))
      output-string
      (begin
        (if (and (eq? #\/ (car input-toks)) (peek-expecting input-toks #\*))
            (let* ((res (parse-block-text (cdr (cdr input-toks)) '()))
                   (out-string (wrap-p (second res))))
              (parse (first res) (append output-string (string->list out-string))))
            (let* ((res (parse-block-code input-toks '()))
                   (out-string (wrap-code (second res))))
              (parse (first res) (append output-string (string->list out-string))))))))

(define (c->html input)
  (let* ((toks (string->list (file->string input)))
         (output (parse toks)))
    output))
