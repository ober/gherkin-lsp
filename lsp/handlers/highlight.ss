;;; -*- Gerbil -*-
;;; Document highlight handler — textDocument/documentHighlight
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; DocumentHighlightKind
(def DocumentHighlightKind.Text  1)
(def DocumentHighlightKind.Read  2)
(def DocumentHighlightKind.Write 3)

;;; Handle textDocument/documentHighlight
;;; Finds the symbol at cursor and returns all occurrences in the document
(def (handle-document-highlight params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (let-values (((sym start-col end-col) (symbol-at-position text line col)))
          (if sym
            (let ((highlights (find-highlights text sym)))
              (list->vector highlights))
            [])))
      [])))

;;; Find all occurrences of a symbol name in text with word-boundary detection
(def (find-highlights text name)
  (let ((lines (string-split-lines text))
        (name-len (string-length name))
        (result '()))
    (let loop ((ls lines) (line-num 0))
      (if (null? ls)
        (reverse result)
        (begin
          (find-symbol-in-line (car ls) name name-len line-num
            (lambda (col)
              (set! result
                (cons (make-document-highlight line-num col (+ col name-len))
                      result))))
          (loop (cdr ls) (+ line-num 1)))))))

;;; Find occurrences of name in a line with word boundary checking
(def (find-symbol-in-line text name name-len line-num callback)
  (let ((text-len (string-length text)))
    (let loop ((i 0))
      (when (<= (+ i name-len) text-len)
        (when (and (string=? name (substring text i (+ i name-len)))
                   (or (= i 0) (not (symbol-char? (string-ref text (- i 1)))))
                   (or (= (+ i name-len) text-len)
                       (not (symbol-char? (string-ref text (+ i name-len))))))
          (callback i))
        (loop (+ i 1))))))

;;; Build a DocumentHighlight object
(def (make-document-highlight line start-col end-col)
  (hash ("range" (make-lsp-range line start-col line end-col))
        ("kind" DocumentHighlightKind.Text)))
