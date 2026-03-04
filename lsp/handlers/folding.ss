;;; -*- Gerbil -*-
;;; Folding range handler — textDocument/foldingRange
(import ../util/log
        ../util/position
        ../state
        ../analysis/document
        ../analysis/parser)
(export #t)

;;; FoldingRangeKind constants
(def FoldingRangeKind.Comment  "comment")
(def FoldingRangeKind.Imports  "imports")
(def FoldingRangeKind.Region   "region")

;;; Handle textDocument/foldingRange
;;; Returns FoldingRange[] for multi-line forms
(def (handle-folding-range params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (forms (parse-source text))
             (ranges (collect-folding-ranges forms)))
        (list->vector ranges))
      [])))

;;; Collect folding ranges from located forms
;;; Only includes forms that span multiple lines
(def (collect-folding-ranges forms)
  (let loop ((fs forms) (result '()))
    (if (null? fs)
      (reverse result)
      (let* ((lf (car fs))
             (start-line (located-form-line lf))
             (end-line (located-form-end-line lf)))
        (if (> end-line start-line)
          (let ((kind (form-folding-kind (located-form-form lf))))
            (loop (cdr fs)
                  (cons (make-folding-range start-line end-line kind) result)))
          (loop (cdr fs) result))))))

;;; Determine the folding kind for a form
(def (form-folding-kind form)
  (if (and (pair? form) (eq? (car form) 'import))
    FoldingRangeKind.Imports
    #f))

;;; Build a FoldingRange object
(def (make-folding-range start-line end-line kind)
  (let ((range (hash ("startLine" start-line)
                      ("endLine" end-line))))
    (when kind
      (hash-put! range "kind" kind))
    range))
