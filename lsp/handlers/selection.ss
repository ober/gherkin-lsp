;;; -*- Gerbil -*-
;;; Selection range handler — textDocument/selectionRange
(import ../util/log
        ../util/position
        ../state
        ../analysis/document
        ../analysis/parser)
(export #t)

;;; Handle textDocument/selectionRange
;;; For each position, returns a nested chain of SelectionRange from
;;; innermost (symbol) to outermost (top-level form)
(def (handle-selection-range params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (positions (hash-ref params "positions" []))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (forms (parse-source text)))
        (list->vector
          (map (lambda (pos)
                 (let ((line (hash-ref pos "line" 0))
                       (col (hash-ref pos "character" 0)))
                   (build-selection-range text forms line col)))
               (vector->list positions))))
      [])))

;;; Build a nested SelectionRange for a position
;;; Finds all enclosing forms and builds from outermost to innermost
(def (build-selection-range text forms line col)
  (let ((enclosing (find-enclosing-forms forms line col)))
    (if (null? enclosing)
      ;; Fallback: use the symbol at cursor position
      (let-values (((sym start end) (symbol-at-position text line col)))
        (if sym
          (hash ("range" (make-lsp-range line start line end)))
          (hash ("range" (make-lsp-range line col line col)))))
      ;; Build nested chain from outermost to innermost
      (let ((chain (build-chain enclosing)))
        ;; Try to add a symbol-level range at the innermost level
        (let-values (((sym start end) (symbol-at-position text line col)))
          (if (and sym (selection-range-parent chain))
            ;; Only add symbol range if it's smaller than the innermost form range
            (hash ("range" (make-lsp-range line start line end))
                  ("parent" chain))
            chain))))))

;;; Find all located-forms that contain the given position
;;; Returns a list sorted from outermost to innermost
(def (find-enclosing-forms forms line col)
  (let loop ((fs forms) (result '()))
    (if (null? fs)
      (reverse result)
      (let ((lf (car fs)))
        (if (form-contains? lf line col)
          (loop (cdr fs) (cons lf result))
          (loop (cdr fs) result))))))

;;; Check if a located-form contains the given position
(def (form-contains? lf line col)
  (let ((sl (located-form-line lf))
        (sc (located-form-col lf))
        (el (located-form-end-line lf))
        (ec (located-form-end-col lf)))
    (and (or (> line sl) (and (= line sl) (>= col sc)))
         (or (< line el) (and (= line el) (<= col ec))))))

;;; Build a nested SelectionRange chain from a list of located-forms
;;; (outermost first)
(def (build-chain forms)
  (if (null? forms)
    #f
    (let loop ((fs (reverse forms)) (parent #f))
      (if (null? fs)
        parent
        (let* ((lf (car fs))
               (range (make-lsp-range (located-form-line lf)
                                       (located-form-col lf)
                                       (located-form-end-line lf)
                                       (located-form-end-col lf)))
               (node (if parent
                       (hash ("range" range) ("parent" parent))
                       (hash ("range" range)))))
          (loop (cdr fs) node))))))

;;; Get the parent from a SelectionRange hash, or #f
(def (selection-range-parent sr)
  (hash-ref sr "parent" #f))
