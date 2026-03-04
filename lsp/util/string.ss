;;; -*- Gerbil -*-
;;; Shared string utilities used across the LSP server
(export #t)

;;; Split text into lines on newline characters
(def (string-split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))

;;; Check if haystack contains needle as a substring
(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring haystack i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

;;; Case-insensitive substring search
(def (string-contains-ci haystack needle)
  (let ((h (string-downcase haystack))
        (n (string-downcase needle)))
    (let ((hlen (string-length h))
          (nlen (string-length n)))
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? n (substring h i (+ i nlen))) #t)
          (else (loop (+ i 1))))))))

;;; Check if a string ends with a suffix
(def (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; Join strings with a separator
(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; Join lines with newline separator
(def (string-join-newline lines)
  (string-join lines "\n"))

;;; Take at most N elements from a list
(def (take-at-most lst n)
  (if (or (null? lst) (<= n 0))
    '()
    (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

;;; Classify each character position in text as:
;;;   0 = code, 1 = string, 2 = comment
;;; Handles: "...", ; line comments, #| block comments |#,
;;;          escape sequences in strings, #\ character literals
;;; Returns a u8vector of same length as text.
(def (classify-text-regions text)
  (let* ((len (string-length text))
         (regions (make-u8vector len 0)))
    (let loop ((i 0))
      (when (< i len)
        (let ((c (string-ref text i)))
          (cond
            ;; String literal
            ((char=? c #\")
             (u8vector-set! regions i 1)
             (let str-loop ((j (+ i 1)))
               (if (>= j len)
                 (loop j)  ;; unterminated string
                 (let ((sc (string-ref text j)))
                   (u8vector-set! regions j 1)
                   (cond
                     ((char=? sc #\\)
                      ;; Skip escaped character
                      (when (< (+ j 1) len)
                        (u8vector-set! regions (+ j 1) 1))
                      (str-loop (+ j 2)))
                     ((char=? sc #\")
                      (loop (+ j 1)))
                     (else (str-loop (+ j 1))))))))
            ;; Line comment
            ((char=? c #\;)
             (let cmt-loop ((j i))
               (if (or (>= j len) (char=? (string-ref text j) #\newline))
                 (loop j)
                 (begin
                   (u8vector-set! regions j 2)
                   (cmt-loop (+ j 1))))))
            ;; Block comment #| ... |#
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (char=? (string-ref text (+ i 1)) #\|))
             (u8vector-set! regions i 2)
             (u8vector-set! regions (+ i 1) 2)
             (let blk-loop ((j (+ i 2)) (depth 1))
               (cond
                 ((>= j len) (loop j))  ;; unterminated
                 ((and (char=? (string-ref text j) #\|)
                       (< (+ j 1) len)
                       (char=? (string-ref text (+ j 1)) #\#))
                  (u8vector-set! regions j 2)
                  (u8vector-set! regions (+ j 1) 2)
                  (if (= depth 1)
                    (loop (+ j 2))
                    (blk-loop (+ j 2) (- depth 1))))
                 ((and (char=? (string-ref text j) #\#)
                       (< (+ j 1) len)
                       (char=? (string-ref text (+ j 1)) #\|))
                  (u8vector-set! regions j 2)
                  (u8vector-set! regions (+ j 1) 2)
                  (blk-loop (+ j 2) (+ depth 1)))
                 (else
                  (u8vector-set! regions j 2)
                  (blk-loop (+ j 1) depth)))))
            ;; Character literal #\x — skip the char after #\
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (char=? (string-ref text (+ i 1)) #\\))
             (if (< (+ i 2) len)
               (loop (+ i 3))  ;; skip #\x
               (loop (+ i 2))))
            ;; Normal code character
            (else (loop (+ i 1)))))))
    regions))

;;; Check if an offset falls in a string or comment region
(def (in-string-or-comment? regions offset)
  (and (< offset (u8vector-length regions))
       (> (u8vector-ref regions offset) 0)))
