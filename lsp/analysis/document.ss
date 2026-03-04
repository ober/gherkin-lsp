;;; -*- Gerbil -*-
;;; Document state management — track open document text and metadata
(import ../util/position)
(export #t)

;;; Document record
(defstruct document (uri version text language-id) transparent: #t)

;;; Create a document from didOpen params
(def (make-document-from-open params)
  (let ((td (hash-ref params "textDocument" (hash))))
    (make-document
      (hash-ref td "uri" "")
      (hash-ref td "version" 0)
      (hash-ref td "text" "")
      (hash-ref td "languageId" "gerbil"))))

;;; Apply full content changes to a document
;;; For TextDocumentSyncKind.Full, the entire text is replaced
(def (document-apply-full-change doc new-text version)
  (make-document
    (document-uri doc)
    version
    new-text
    (document-language-id doc)))

;;; Get the number of lines in a document
(def (document-line-count doc)
  (let ((text (document-text doc)))
    (let loop ((i 0) (count 1))
      (cond
        ((>= i (string-length text)) count)
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ count 1)))
        (else (loop (+ i 1) count))))))

;;; Get a specific line from the document (0-based)
(def (document-line-at doc line-number)
  (let ((text (document-text doc)))
    (let loop ((i 0) (cur-line 0) (start 0))
      (cond
        ((>= i (string-length text))
         (if (= cur-line line-number)
           (substring text start i)
           ""))
        ((char=? (string-ref text i) #\newline)
         (if (= cur-line line-number)
           (substring text start i)
           (loop (+ i 1) (+ cur-line 1) (+ i 1))))
        (else
         (loop (+ i 1) cur-line start))))))

;;; Split document text into lines
(def (document-lines doc)
  (let ((text (document-text doc)))
    (let loop ((i 0) (start 0) (lines '()))
      (cond
        ((>= i (string-length text))
         (reverse (cons (substring text start i) lines)))
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
        (else
         (loop (+ i 1) start lines))))))

;;; Apply an incremental (range) change to a document.
;;; The range specifies start/end line/col; text is the replacement.
(def (document-apply-incremental-change doc range-change version)
  (let* ((text (document-text doc))
         (range (hash-ref range-change "range" #f))
         (new-text (hash-ref range-change "text" "")))
    (if (not range)
      ;; No range means full replacement
      (document-apply-full-change doc new-text version)
      (let* ((start (hash-ref range "start" (hash)))
             (end (hash-ref range "end" (hash)))
             (start-line (hash-ref start "line" 0))
             (start-char (hash-ref start "character" 0))
             (end-line (hash-ref end "line" 0))
             (end-char (hash-ref end "character" 0))
             (start-offset (line-col->offset text start-line start-char))
             (end-offset (line-col->offset text end-line end-char))
             (result (string-append
                       (substring text 0 start-offset)
                       new-text
                       (substring text end-offset (string-length text)))))
        (make-document (document-uri doc) version result
                       (document-language-id doc))))))
