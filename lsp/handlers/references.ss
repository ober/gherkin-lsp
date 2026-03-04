;;; -*- Gerbil -*-
;;; Find references handler
(import ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/index)
(export #t)

;;; Handle textDocument/references
(def (handle-references params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name _start _end)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (let ((refs (find-all-references sym-name)))
            (list->vector refs))
          []))
      [])))

;;; Find all references to a symbol across the entire workspace
;;; Searches both open documents and indexed files
(def (find-all-references name)
  (let ((result '())
        (searched (make-hash-table)))
    ;; Search open documents first (they have the latest text)
    (for-each
      (lambda (uri)
        (hash-put! searched uri #t)
        (let ((doc (get-document uri)))
          (when doc
            (find-symbol-in-text name (document-text doc) uri
              (lambda (line col end-col)
                (set! result
                  (cons (make-lsp-location uri
                          (make-lsp-range line col line end-col))
                        result)))))))
      (all-document-uris))
    ;; Search indexed files that aren't open
    (for-each
      (lambda (uri)
        (unless (hash-key? searched uri)
          (let ((text (get-file-text uri)))
            (when text
              (find-symbol-in-text name text uri
                (lambda (line col end-col)
                  (set! result
                    (cons (make-lsp-location uri
                            (make-lsp-range line col line end-col))
                          result))))))))
      (all-indexed-uris))
    result))

;;; Find all occurrences of a symbol name in text
(def (find-symbol-in-text name text uri callback)
  (let ((name-len (string-length name))
        (text-len (string-length text)))
    (let line-loop ((i 0) (line-num 0) (line-start 0))
      (cond
        ((>= i text-len)
         ;; Process last line
         (scan-line-for-symbol name name-len text line-start i line-num callback))
        ((char=? (string-ref text i) #\newline)
         (scan-line-for-symbol name name-len text line-start i line-num callback)
         (line-loop (+ i 1) (+ line-num 1) (+ i 1)))
        (else
         (line-loop (+ i 1) line-num line-start))))))

;;; Scan a single line for symbol occurrences
(def (scan-line-for-symbol name name-len text line-start line-end line-num callback)
  (let ((line-text (substring text line-start line-end)))
    (let loop ((col 0))
      (when (<= (+ col name-len) (string-length line-text))
        (when (and (string=? name (substring line-text col (+ col name-len)))
                   (or (= col 0)
                       (not (symbol-char? (string-ref line-text (- col 1)))))
                   (or (= (+ col name-len) (string-length line-text))
                       (not (symbol-char?
                              (string-ref line-text (+ col name-len))))))
          (callback line-num col (+ col name-len)))
        (loop (+ col 1))))))
