;;; -*- Gerbil -*-
;;; Call hierarchy handlers — prepareCallHierarchy, incomingCalls, outgoingCalls
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index
        ../analysis/module)
(export #t)

;;; Handle textDocument/prepareCallHierarchy
;;; Find the function definition at the cursor position
(def (handle-prepare-call-hierarchy params)
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
            ;; Find the definition for this symbol
            (let ((file-syms (get-file-symbols uri)))
              (let ((found (find-sym-by-name sym file-syms)))
                (if (and found
                         (or (= (sym-info-kind found) SymbolKind.Function)
                             (= (sym-info-kind found) SymbolKind.Method)))
                  (vector
                    (make-call-hierarchy-item found uri))
                  (vector))))
            (vector))))
      (vector))))

;;; Handle callHierarchy/incomingCalls
;;; Find all callers of the given function
(def (handle-incoming-calls params)
  (let* ((item (hash-ref params "item" (hash)))
         (name (hash-ref item "name" ""))
         (uri (hash-ref item "uri" "")))
    (let ((refs (find-references-by-name name))
          (result '()))
      (for-each
        (lambda (ref)
          (let* ((ref-uri (list-ref ref 0))
                 (ref-line (list-ref ref 1))
                 (ref-col (list-ref ref 2))
                 (ref-end-col (list-ref ref 3)))
            ;; Find the enclosing function at this location
            (let ((enclosing (find-enclosing-function ref-uri ref-line)))
              (when (and enclosing
                         ;; Don't include self-references from the definition itself
                         (not (and (string=? ref-uri uri)
                                   (string=? (sym-info-name enclosing) name))))
                (set! result
                  (cons (hash
                          ("from" (make-call-hierarchy-item enclosing ref-uri))
                          ("fromRanges"
                           (vector
                             (make-lsp-range ref-line ref-col ref-line ref-end-col))))
                        result))))))
        refs)
      ;; Deduplicate by caller name+uri
      (list->vector (deduplicate-calls result)))))

;;; Handle callHierarchy/outgoingCalls
;;; Find all functions called by the given function
(def (handle-outgoing-calls params)
  (let* ((item (hash-ref params "item" (hash)))
         (name (hash-ref item "name" ""))
         (uri (hash-ref item "uri" "")))
    (let ((doc (or (get-document uri) #f))
          (text (or (and (get-document uri) (document-text (get-document uri)))
                    (get-file-text uri))))
      (if text
        (let* ((forms (parse-source text))
               (file-syms (get-file-symbols uri))
               ;; Find the definition form for this function
               (def-form (find-definition-form name forms))
               (called-names (if def-form
                               (extract-called-symbols (located-form-form def-form))
                               '()))
               (result '()))
          ;; For each called symbol, try to find its definition
          (for-each
            (lambda (called-name)
              (let ((defs (find-definitions-by-name called-name)))
                (when (pair? defs)
                  (let* ((def-entry (car defs))
                         (def-uri (car def-entry))
                         (def-sym (cdr def-entry)))
                    (when (or (= (sym-info-kind def-sym) SymbolKind.Function)
                              (= (sym-info-kind def-sym) SymbolKind.Method))
                      (set! result
                        (cons (hash
                                ("to" (make-call-hierarchy-item def-sym def-uri))
                                ("fromRanges" (vector (make-lsp-range 0 0 0 0))))
                              result)))))))
            called-names)
          (list->vector result))
        (vector)))))

;;; Create a CallHierarchyItem from a sym-info
(def (make-call-hierarchy-item sym uri)
  (let ((line (sym-info-line sym))
        (col (sym-info-col sym))
        (end-line (sym-info-end-line sym))
        (end-col (sym-info-end-col sym))
        (name (sym-info-name sym))
        (kind (sym-info-kind sym)))
    (hash ("name" name)
          ("kind" kind)
          ("uri" uri)
          ("range" (make-lsp-range line col end-line end-col))
          ("selectionRange" (make-lsp-range line col line (+ col (string-length name)))))))

;;; Find the enclosing function definition at a given line
(def (find-enclosing-function uri line)
  (let ((syms (get-file-symbols uri)))
    (let loop ((ss syms) (best #f))
      (if (null? ss) best
        (let ((s (car ss)))
          (if (and (or (= (sym-info-kind s) SymbolKind.Function)
                       (= (sym-info-kind s) SymbolKind.Method))
                   (<= (sym-info-line s) line)
                   (>= (sym-info-end-line s) line))
            ;; Pick the most specific (innermost) enclosing function
            (if (or (not best)
                    (> (sym-info-line s) (sym-info-line best)))
              (loop (cdr ss) s)
              (loop (cdr ss) best))
            (loop (cdr ss) best)))))))

;;; Find the located-form for a named definition
(def (find-definition-form name forms)
  (let loop ((fs forms))
    (if (null? fs) #f
      (let* ((lf (car fs))
             (form (located-form-form lf)))
        (if (and (pair? form)
                 (definition-form? form)
                 (def-form-name form)
                 (string=? (def-form-name form) name))
          lf
          (loop (cdr fs)))))))

;;; Get the name defined by a definition form
(def (def-form-name form)
  (if (< (length form) 2) #f
    (let ((target (cadr form)))
      (cond
        ((symbol? target) (symbol->string target))
        ((and (pair? target) (symbol? (car target)))
         (symbol->string (car target)))
        (else #f)))))

;;; Extract all called symbol names from a form body
(def (extract-called-symbols form)
  (let ((result '())
        (seen (make-hash-table)))
    (define (walk f)
      (when (pair? f)
        (let ((head (car f)))
          (when (symbol? head)
            (let ((name (symbol->string head)))
              (unless (hash-key? seen name)
                (hash-put! seen name #t)
                (set! result (cons name result)))))
          (for-each
            (lambda (sub) (when (pair? sub) (walk sub)))
            (cdr f)))))
    (when (pair? form)
      (when (>= (length form) 3)
        ;; Walk the body (skip def head and arg list)
        (for-each walk (cddr form))))
    result))

;;; Deduplicate incoming calls by caller name+uri
(def (deduplicate-calls calls)
  (let ((seen (make-hash-table))
        (result '()))
    (for-each
      (lambda (call)
        (let* ((from (hash-ref call "from" (hash)))
               (key (string-append (hash-ref from "uri" "")
                                   ":" (hash-ref from "name" ""))))
          (unless (hash-key? seen key)
            (hash-put! seen key #t)
            (set! result (cons call result)))))
      calls)
    (reverse result)))
