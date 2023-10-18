#lang racket

(provide c->html)

(define (eat-until toks t res)
  (if (eq? (car toks) t)
      (list (cdr toks) res)
      (eat-until (cdr toks) t (append res (list (first toks))))))

(define (make-link-inner input-toks res-text)
  (let* ((toks_temp-str (eat-until input-toks #\) '()))
         (temp-str (list->string (second toks_temp-str)))
         (toks (first toks_temp-str))
         (txt-link (string-split temp-str "]"))
         (lnk (string-replace (string-replace (car (cdr txt-link)) ")" "") "(" ""))
         (txt (string-replace (string-replace (car txt-link) "]" "") "[" ""))
         (html-link (string-append "<a href=\"" lnk "\">" txt "</a>")))
    (list html-link toks)))

;; links are formatted in comments as [text](link)
(define (make-links input-toks res-text)
  (if (equal? #t (null? input-toks))
     res-text
      (begin
        (if (eq? (car input-toks) #\[)
            (begin
              (let* ((linked-text_toks (make-link-inner input-toks res-text))
                     (toks (second linked-text_toks))
                     (linked-text (string->list (first linked-text_toks)))
                    (appended (append res-text linked-text)))
                (make-links toks appended)))
            (make-links (cdr input-toks) (append res-text (list (car input-toks))))))))

(define (replace-newlines text)
  (string-replace text "\n" "</p><p>"))

(define (replace-brackets text)
  (string-replace (string-replace text "<" "&lt;") ">" "&gt;"))

(define (wrap-p text)
  (string-append "<p>" (replace-newlines (list->string text)) "</p>"))

(define (wrap-code text)
  (string-append "<pre><code>" (replace-brackets (list->string text)) "</code></pre>"))

(define (wrap-title text)
  (string-append "<h1>" text "</h1>"))

(define (extract-title text)
  (let* ((split-string (string-split text "\n"))
         (title (string-replace (first split-string) "/" "")))
    (list (wrap-title title) (cdr split-string))))

;; simple lr(1) parser builds the string one character at at time
;; long comments (i.e. text inside "/*...*/" are turned into html
;; text surrounded by the <p> tag. otherwise the html text emitted
;; will be surrouned by <code> tags
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
            ;; todo: replace newlines with <p></p>
            (let* ((res (parse-block-text (cdr (cdr input-toks)) '()))
                   (out-string (wrap-p (second res))))
              (parse (first res) (append output-string (make-links (string->list out-string) '()))))
            ;; todo: replace brackets with &gt; &lt;
            (let* ((res (parse-block-code input-toks '()))
                   (out-string (wrap-code (second res))))
              (parse (first res) (append output-string (string->list out-string))))))))

;; todo (make-title)
(define (c->html input)
  (let* ((title-rest (extract-title (file->string input)))
         (toks (string->list (string-join (second title-rest) "\n")))
         (output (append (string->list (first title-rest)) (parse toks '()))))
    (list->string  output)))
