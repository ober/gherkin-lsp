;;; -*- Gerbil -*-
;;; Code action handler — textDocument/codeAction
;;; Provides organize imports and add missing import actions
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/completion-data)
(export #t)

;;; CodeActionKind constants
(def CodeActionKind.QuickFix            "quickfix")
(def CodeActionKind.Source              "source")
(def CodeActionKind.SourceOrganizeImports "source.organizeImports")
(def CodeActionKind.RefactorExtract     "refactor.extract")
(def CodeActionKind.RefactorRewrite     "refactor.rewrite")

;;; Handle textDocument/codeAction
;;; Returns CodeAction[] with available actions (lazy — no edit, just data)
(def (handle-code-action params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (context (hash-ref params "context" (hash)))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc))
            (actions '())
            (start-line (hash-ref (hash-ref range "start" (hash)) "line" 0))
            (start-col (hash-ref (hash-ref range "start" (hash)) "character" 0))
            (end-line (hash-ref (hash-ref range "end" (hash)) "line" 0))
            (end-col (hash-ref (hash-ref range "end" (hash)) "character" 0)))
        ;; Always offer "organize imports" if there are imports
        (let ((organize (make-organize-imports-action uri text)))
          (when organize
            (set! actions (cons organize actions))))
        ;; Offer "add missing import" based on diagnostics
        (let ((diags (hash-ref context "diagnostics" [])))
          (let ((import-actions (make-add-import-actions uri text diags)))
            (set! actions (append actions import-actions)))
          ;; Offer "remove unused import" for unused-import diagnostics
          (let ((unused-actions (make-remove-unused-import-actions uri text diags)))
            (set! actions (append actions unused-actions))))
        ;; Offer "extract to def" when text is selected
        (when (or (not (= start-line end-line)) (not (= start-col end-col)))
          (set! actions (cons (make-extract-to-def-action uri start-line start-col
                                                          end-line end-col)
                              actions)))
        ;; Offer "wrap in form" when text is selected
        (when (or (not (= start-line end-line)) (not (= start-col end-col)))
          (for-each
            (lambda (wrapper)
              (set! actions (cons (make-wrap-in-form-action uri wrapper
                                   start-line start-col end-line end-col)
                                  actions)))
            '("when" "unless" "let" "begin" "try")))
        ;; Offer "add missing export" for defined-but-not-exported symbols
        (let ((export-actions (make-add-export-actions uri text start-line)))
          (set! actions (append actions export-actions)))
        ;; Offer "convert cond↔if" when cursor is on cond/if
        (let ((convert-action (make-convert-conditional-action uri text start-line)))
          (when convert-action
            (set! actions (cons convert-action actions))))
        (list->vector actions))
      [])))

;;; Handle codeAction/resolve
;;; Receives a code action with data, computes the edit, returns the full action
(def (handle-code-action-resolve params)
  (let ((data (hash-ref params "data" #f)))
    (if (not data)
      params  ;; No data — return as-is
      (let ((type (hash-ref data "type" ""))
            (uri (hash-ref data "uri" "")))
        (with-catch
          (lambda (e)
            (lsp-debug "code action resolve failed: ~a" e)
            params)
          (lambda ()
            (let ((doc (get-document uri)))
              (if (not doc)
                params
                (let ((text (document-text doc)))
                  (cond
                    ((string=? type "organize-imports")
                     (let ((edit (compute-organize-imports-edit uri text)))
                       (if edit
                         (begin (hash-put! params "edit" edit) params)
                         params)))
                    ((string=? type "add-import")
                     (let* ((module-name (hash-ref data "module" ""))
                            (edit (compute-add-import-edit uri text module-name)))
                       (hash-put! params "edit" edit)
                       params))
                    ((string=? type "remove-import")
                     (let* ((line (hash-ref data "line" 0))
                            (edit (make-remove-import-edit uri text
                                    (hash-ref data "importName" "") line)))
                       (if edit
                         (begin (hash-put! params "edit" edit) params)
                         params)))
                    ((string=? type "extract-to-def")
                     (let ((edit (compute-extract-to-def-edit
                                   uri text
                                   (hash-ref data "startLine" 0)
                                   (hash-ref data "startCol" 0)
                                   (hash-ref data "endLine" 0)
                                   (hash-ref data "endCol" 0))))
                       (hash-put! params "edit" edit)
                       params))
                    ((string=? type "wrap-in-form")
                     (let ((edit (compute-wrap-in-form-edit
                                   uri text
                                   (hash-ref data "wrapper" "begin")
                                   (hash-ref data "startLine" 0)
                                   (hash-ref data "startCol" 0)
                                   (hash-ref data "endLine" 0)
                                   (hash-ref data "endCol" 0))))
                       (hash-put! params "edit" edit)
                       params))
                    ((string=? type "add-export")
                     (let ((edit (compute-add-export-edit
                                   uri text
                                   (hash-ref data "symbol" ""))))
                       (hash-put! params "edit" edit)
                       params))
                    ((string=? type "convert-conditional")
                     (let ((edit (compute-convert-conditional-edit
                                   uri text
                                   (hash-ref data "line" 0)
                                   (hash-ref data "direction" ""))))
                       (if edit
                         (begin (hash-put! params "edit" edit) params)
                         params)))
                    (else params)))))))))))

;;; Create an "Organize Imports" code action (lazy — data only, no edit)
(def (make-organize-imports-action uri text)
  (let ((forms (parse-source text)))
    (let ((import-forms (find-import-forms forms)))
      (if (null? import-forms)
        #f
        (let ((all-specs (collect-all-import-specs import-forms)))
          (if (< (length all-specs) 2)
            #f
            (hash
              ("title" "Organize Imports")
              ("kind" CodeActionKind.SourceOrganizeImports)
              ("data" (hash ("type" "organize-imports")
                            ("uri" uri))))))))))

;;; Find all top-level import forms
(def (find-import-forms forms)
  (let loop ((fs forms) (result '()))
    (if (null? fs)
      (reverse result)
      (let ((lf (car fs)))
        (if (and (pair? (located-form-form lf))
                 (eq? (car (located-form-form lf)) 'import))
          (loop (cdr fs) (cons lf result))
          (loop (cdr fs) result))))))

;;; Collect all import specs from multiple import forms
(def (collect-all-import-specs import-forms)
  (let ((specs '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (pair? (cdr form))
            (for-each
              (lambda (spec) (set! specs (cons spec specs)))
              (cdr form)))))
      import-forms)
    (reverse specs)))

;;; Sort import specs alphabetically by their string representation
(def (sort-import-specs specs)
  (let ((pairs (map (lambda (s) (cons (import-spec->sort-key s) s)) specs)))
    (let ((sorted (sort pairs (lambda (a b) (string<? (car a) (car b))))))
      (map cdr sorted))))

;;; Get a sort key for an import spec
(def (import-spec->sort-key spec)
  (cond
    ((symbol? spec) (symbol->string spec))
    ((pair? spec) (format "~a" spec))
    (else "")))

;;; Format sorted imports as a single import form
(def (format-organized-imports specs)
  (if (null? specs)
    ""
    (let ((out (open-output-string)))
      (display "(import" out)
      (for-each
        (lambda (spec)
          (display "\n        " out)
          (write spec out))
        specs)
      (display ")" out)
      (get-output-string out))))

;;; Compute the workspace edit for organizing imports
(def (compute-organize-imports-edit uri text)
  (let ((forms (parse-source text)))
    (let ((import-forms (find-import-forms forms)))
      (if (null? import-forms)
        #f
        (let ((all-specs (collect-all-import-specs import-forms)))
          (if (< (length all-specs) 2)
            #f
            (let* ((sorted-specs (sort-import-specs all-specs))
                   (first-import (car import-forms))
                   (last-import (last-elem import-forms))
                   (start-line (located-form-line first-import))
                   (end-line (located-form-end-line last-import))
                   (end-col (located-form-end-col last-import))
                   (new-text (format-organized-imports sorted-specs)))
              (hash ("changes"
                     (hash (uri
                            (vector
                              (make-text-edit
                                (make-lsp-range start-line 0 end-line end-col)
                                new-text)))))))))))))

;;; Compute the workspace edit for adding an import
(def (compute-add-import-edit uri text module-name)
  (let* ((forms (parse-source text))
         (insert-pos (find-import-insert-position forms text)))
    (hash ("changes"
           (hash (uri
                  (vector
                    (make-text-edit
                      (make-lsp-range (car insert-pos) (cdr insert-pos)
                                      (car insert-pos) (cdr insert-pos))
                      (format "\n(import ~a)" module-name)))))))))

;;; Create "Add missing import" code actions from diagnostics
;;; Looks for unbound identifier errors and suggests imports
(def (make-add-import-actions uri text diags)
  (let ((actions '()))
    (for-each
      (lambda (diag)
        (let ((msg (if (vector? diag)
                     ""
                     (hash-ref diag "message" ""))))
          ;; Look for "unbound identifier" patterns
          (let ((sym-name (extract-unbound-symbol msg)))
            (when sym-name
              (let ((suggestions (suggest-import-for sym-name)))
                (for-each
                  (lambda (module-name)
                    (set! actions
                      (cons (make-add-import-action uri text sym-name module-name)
                            actions)))
                  suggestions))))))
      (if (vector? diags) (vector->list diags) '()))
    actions))

;;; Extract unbound symbol name from an error message
(def (extract-unbound-symbol msg)
  (cond
    ;; Pattern: "unbound identifier: foo"
    ((string-contains msg "unbound identifier")
     (let ((parts (string-split-on-colon msg)))
       (if (>= (length parts) 2)
         (string-trim-whitespace (list-ref-safe parts (- (length parts) 1)))
         #f)))
    ;; Pattern: "Unbound variable: foo"
    ((string-contains msg "Unbound variable")
     (let ((parts (string-split-on-colon msg)))
       (if (>= (length parts) 2)
         (string-trim-whitespace (list-ref-safe parts (- (length parts) 1)))
         #f)))
    (else #f)))

;;; Suggest module imports for a symbol
;;; Uses the stdlib symbols list from completion-data
(def (suggest-import-for sym-name)
  (let ((result '()))
    (for-each
      (lambda (entry)
        (when (string=? (car entry) sym-name)
          (set! result (cons (cadr entry) result))))
      *stdlib-symbols*)
    (reverse result)))

;;; Create a single "Add import" code action (lazy — data only)
(def (make-add-import-action uri text sym-name module-name)
  (hash
    ("title" (format "Add import ~a from ~a" sym-name module-name))
    ("kind" CodeActionKind.QuickFix)
    ("data" (hash ("type" "add-import")
                  ("uri" uri)
                  ("symbol" sym-name)
                  ("module" module-name)))))

;;; Find where to insert a new import statement
;;; Returns (line . col) — after the last existing import, or at line 0
(def (find-import-insert-position forms text)
  (let ((last-import-end 0))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (set! last-import-end (located-form-end-line lf)))))
      forms)
    (cons last-import-end 0)))

;;; Split string on colon
(def (string-split-on-colon str)
  (let loop ((i 0) (start 0) (parts '()))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) parts)))
      ((char=? (string-ref str i) #\:)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
      (else (loop (+ i 1) start parts)))))

;;; Trim leading/trailing whitespace from a string
(def (string-trim-whitespace str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (or (>= i len)
                          (not (char-whitespace-trim? (string-ref str i))))
                    i
                    (loop (+ i 1)))))
         (end (let loop ((i (- len 1)))
                (if (or (< i start)
                        (not (char-whitespace-trim? (string-ref str i))))
                  (+ i 1)
                  (loop (- i 1))))))
    (substring str start end)))

(def (char-whitespace-trim? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))

;;; Safe list-ref
(def (list-ref-safe lst n)
  (let loop ((l lst) (i 0))
    (cond
      ((null? l) "")
      ((= i n) (car l))
      (else (loop (cdr l) (+ i 1))))))

;;; Get last element of a list
(def (last-elem lst)
  (if (null? (cdr lst))
    (car lst)
    (last-elem (cdr lst))))

;;; Create "Remove unused import" code actions from diagnostics (lazy — data only)
(def (make-remove-unused-import-actions uri text diags)
  (let ((actions '()))
    (for-each
      (lambda (diag)
        (when (hash-table? diag)
          (let ((code (hash-ref diag "code" ""))
                (msg (hash-ref diag "message" "")))
            (when (string=? code "unused-import")
              (let ((import-name (extract-unused-import-name msg)))
                (when import-name
                  (let ((range (hash-ref diag "range" #f)))
                    (when range
                      (let* ((start (hash-ref range "start" (hash)))
                             (line (hash-ref start "line" 0)))
                        (set! actions
                          (cons (hash
                                  ("title" (format "Remove unused import: ~a" import-name))
                                  ("kind" CodeActionKind.QuickFix)
                                  ("diagnostics" (vector diag))
                                  ("data" (hash ("type" "remove-import")
                                                ("uri" uri)
                                                ("importName" import-name)
                                                ("line" line))))
                                actions)))))))))))
      (if (vector? diags) (vector->list diags) '()))
    actions))

;;; Extract the import name from an "Unused import: X" message
(def (extract-unused-import-name msg)
  (if (string-prefix? "Unused import: " msg)
    (substring msg 15 (string-length msg))
    #f))

;;; Create a workspace edit that removes an import spec from the file
(def (make-remove-import-edit uri text import-name line)
  (let* ((lines (string-split-lines text))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      ""))
         ;; Check if this line is a standalone import like (import :foo/bar)
         ;; or part of a multi-spec import
         (trimmed (string-trim-whitespace line-text)))
    ;; Simple case: the import spec is on its own line within a multi-line import
    ;; Remove the entire line
    (if (and (> line 0) (< line (length lines)))
      (hash ("changes"
             (hash (uri
                    (vector
                      (make-text-edit
                        (make-lsp-range line 0 (+ line 1) 0)
                        ""))))))
      #f)))

;;; String split into lines helper
(def (string-split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))

;;; ====================================================================
;;; Extract to def
;;; ====================================================================

;;; Create "Extract to def" code action (lazy)
(def (make-extract-to-def-action uri start-line start-col end-line end-col)
  (hash
    ("title" "Extract to def")
    ("kind" CodeActionKind.RefactorExtract)
    ("data" (hash ("type" "extract-to-def")
                  ("uri" uri)
                  ("startLine" start-line)
                  ("startCol" start-col)
                  ("endLine" end-line)
                  ("endCol" end-col)))))

;;; Compute workspace edit for extracting selection to a top-level def
(def (compute-extract-to-def-edit uri text start-line start-col end-line end-col)
  (let* ((lines (string-split-lines text))
         (selected (extract-text-range lines start-line start-col end-line end-col))
         (def-name "extracted")
         (def-text (format "(def (~a)\n  ~a)\n\n" def-name selected))
         ;; Insert the new def before the form containing the selection
         (insert-line (find-containing-form-start lines start-line)))
    (hash ("changes"
           (hash (uri
                  (vector
                    ;; Insert new def
                    (make-text-edit
                      (make-lsp-range insert-line 0 insert-line 0)
                      def-text)
                    ;; Replace selection with call
                    (make-text-edit
                      (make-lsp-range start-line start-col end-line end-col)
                      (format "(~a)" def-name)))))))))

;;; Extract text from a line range
(def (extract-text-range lines start-line start-col end-line end-col)
  (let ((num-lines (length lines)))
    (cond
      ((>= start-line num-lines) "")
      ((= start-line end-line)
       (let ((line (list-ref lines start-line)))
         (substring line
                    (min start-col (string-length line))
                    (min end-col (string-length line)))))
      (else
       (let ((out (open-output-string)))
         ;; First line from start-col
         (let ((first (list-ref lines start-line)))
           (display (substring first (min start-col (string-length first))
                               (string-length first)) out))
         ;; Middle lines (full)
         (let loop ((i (+ start-line 1)))
           (when (< i (min end-line num-lines))
             (display "\n" out)
             (display (list-ref lines i) out)
             (loop (+ i 1))))
         ;; Last line up to end-col
         (when (< end-line num-lines)
           (display "\n" out)
           (let ((last-line (list-ref lines end-line)))
             (display (substring last-line 0 (min end-col (string-length last-line))) out)))
         (get-output-string out))))))

;;; Find the start line of the top-level form containing the given line
(def (find-containing-form-start lines target-line)
  (let loop ((i target-line))
    (cond
      ((<= i 0) 0)
      ((let ((line (list-ref lines i)))
         (and (> (string-length line) 0)
              (char=? (string-ref line 0) #\()))
       i)
      (else (loop (- i 1))))))

;;; ====================================================================
;;; Wrap in form
;;; ====================================================================

;;; Create "Wrap in <form>" code action (lazy)
(def (make-wrap-in-form-action uri wrapper start-line start-col end-line end-col)
  (hash
    ("title" (format "Wrap in (~a ...)" wrapper))
    ("kind" CodeActionKind.RefactorRewrite)
    ("data" (hash ("type" "wrap-in-form")
                  ("uri" uri)
                  ("wrapper" wrapper)
                  ("startLine" start-line)
                  ("startCol" start-col)
                  ("endLine" end-line)
                  ("endCol" end-col)))))

;;; Compute workspace edit for wrapping selection in a form
(def (compute-wrap-in-form-edit uri text wrapper start-line start-col end-line end-col)
  (let* ((lines (string-split-lines text))
         (selected (extract-text-range lines start-line start-col end-line end-col))
         (wrapped (cond
                    ((string=? wrapper "let")
                     (format "(let (())\n  ~a)" selected))
                    (else
                     (format "(~a ~a)" wrapper selected)))))
    (hash ("changes"
           (hash (uri
                  (vector
                    (make-text-edit
                      (make-lsp-range start-line start-col end-line end-col)
                      wrapped))))))))

;;; ====================================================================
;;; Add missing export
;;; ====================================================================

;;; Create "Add export" code actions for symbols defined but not exported
(def (make-add-export-actions uri text cursor-line)
  (let ((forms (parse-source text))
        (actions '()))
    ;; Find all export forms and collect exported names
    (let ((exported (collect-exported-names forms))
          (defined (collect-defined-names forms)))
      ;; Find the form at the cursor line
      (for-each
        (lambda (def-entry)
          (let ((name (car def-entry))
                (line (cdr def-entry)))
            (when (and (= line cursor-line)
                       (not (hash-key? exported name))
                       ;; Skip if export #t is used
                       (not (hash-get exported "#t")))
              (set! actions
                (cons (hash
                        ("title" (format "Add ~a to exports" name))
                        ("kind" CodeActionKind.QuickFix)
                        ("data" (hash ("type" "add-export")
                                      ("uri" uri)
                                      ("symbol" name))))
                      actions)))))
        defined))
    actions))

;;; Collect exported symbol names from (export ...) forms
(def (collect-exported-names forms)
  (let ((ht (make-hash-table)))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'export))
            (for-each
              (lambda (spec)
                (cond
                  ((eq? spec #t) (hash-put! ht "#t" #t))
                  ((symbol? spec) (hash-put! ht (symbol->string spec) #t))))
              (cdr form)))))
      forms)
    ht))

;;; Collect defined symbol names: ((name . line) ...)
(def (collect-defined-names forms)
  (let ((result '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf))
              (line (located-form-line lf)))
          (when (pair? form)
            (let ((head (car form)))
              (when (memq head '(def define defn defstruct defclass defmethod
                                 defrule defrules defsyntax defconst defvalues))
                (let ((name-part (and (pair? (cdr form)) (cadr form))))
                  (cond
                    ((symbol? name-part)
                     (set! result (cons (cons (symbol->string name-part) line) result)))
                    ((and (pair? name-part) (symbol? (car name-part)))
                     (set! result (cons (cons (symbol->string (car name-part)) line)
                                        result))))))))))
      forms)
    (reverse result)))

;;; Compute workspace edit for adding a symbol to exports
(def (compute-add-export-edit uri text symbol-name)
  (let ((forms (parse-source text)))
    ;; Find the (export ...) form
    (let ((export-form (find-export-form forms)))
      (if export-form
        ;; Add to existing export form — append before closing paren
        (let* ((end-line (located-form-end-line export-form))
               (end-col (located-form-end-col export-form)))
          (hash ("changes"
                 (hash (uri
                        (vector
                          (make-text-edit
                            (make-lsp-range end-line (- end-col 1) end-line (- end-col 1))
                            (format " ~a" symbol-name))))))))
        ;; No export form — insert one after imports
        (let ((insert-line (find-export-insert-line forms)))
          (hash ("changes"
                 (hash (uri
                        (vector
                          (make-text-edit
                            (make-lsp-range insert-line 0 insert-line 0)
                            (format "(export ~a)\n" symbol-name))))))))))))

;;; Find the (export ...) form
(def (find-export-form forms)
  (let loop ((fs forms))
    (if (null? fs) #f
      (let ((lf (car fs)))
        (if (and (pair? (located-form-form lf))
                 (eq? (car (located-form-form lf)) 'export))
          lf
          (loop (cdr fs)))))))

;;; Find line after last import (for inserting export)
(def (find-export-insert-line forms)
  (let ((last-import-end 0))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (set! last-import-end (+ (located-form-end-line lf) 1)))))
      forms)
    last-import-end))

;;; ====================================================================
;;; Convert cond ↔ if
;;; ====================================================================

;;; Create convert conditional action if cursor is on a cond or if form
(def (make-convert-conditional-action uri text cursor-line)
  (let ((forms (parse-source text)))
    (let ((form-at (find-form-at-line forms cursor-line)))
      (if (not form-at) #f
        (let ((form (located-form-form form-at)))
          (cond
            ;; cond with exactly 2 clauses → convert to if
            ((and (pair? form) (eq? (car form) 'cond)
                  (= (length (cdr form)) 2))
             (hash
               ("title" "Convert cond to if")
               ("kind" CodeActionKind.RefactorRewrite)
               ("data" (hash ("type" "convert-conditional")
                             ("uri" uri)
                             ("line" cursor-line)
                             ("direction" "cond-to-if")))))
            ;; if → convert to cond
            ((and (pair? form) (eq? (car form) 'if)
                  (>= (length (cdr form)) 2))
             (hash
               ("title" "Convert if to cond")
               ("kind" CodeActionKind.RefactorRewrite)
               ("data" (hash ("type" "convert-conditional")
                             ("uri" uri)
                             ("line" cursor-line)
                             ("direction" "if-to-cond")))))
            (else #f)))))))

;;; Find the form at or containing the given line
(def (find-form-at-line forms line)
  (let loop ((fs forms))
    (if (null? fs) #f
      (let ((lf (car fs)))
        (if (and (>= line (located-form-line lf))
                 (<= line (located-form-end-line lf)))
          lf
          (loop (cdr fs)))))))

;;; Compute workspace edit for converting cond↔if
(def (compute-convert-conditional-edit uri text line direction)
  (let ((forms (parse-source text)))
    (let ((form-at (find-form-at-line forms line)))
      (if (not form-at) #f
        (let ((form (located-form-form form-at))
              (start-line (located-form-line form-at))
              (start-col (located-form-col form-at))
              (end-line (located-form-end-line form-at))
              (end-col (located-form-end-col form-at)))
          (cond
            ((string=? direction "cond-to-if")
             (let ((clauses (cdr form)))
               (if (= (length clauses) 2)
                 (let* ((clause1 (car clauses))
                        (clause2 (cadr clauses))
                        (test (if (pair? clause1) (car clause1) clause1))
                        (then-body (if (pair? clause1) (cdr clause1) '()))
                        (else-body (if (and (pair? clause2)
                                            (eq? (car clause2) 'else))
                                     (cdr clause2)
                                     (if (pair? clause2) (cdr clause2) '())))
                        (then-expr (if (= (length then-body) 1)
                                     (format "~a" (car then-body))
                                     (format "(begin ~a)" (format-list then-body))))
                        (else-expr (if (= (length else-body) 1)
                                     (format "~a" (car else-body))
                                     (format "(begin ~a)" (format-list else-body))))
                        (new-text (format "(if ~a\n  ~a\n  ~a)" test then-expr else-expr)))
                   (hash ("changes"
                          (hash (uri
                                 (vector
                                   (make-text-edit
                                     (make-lsp-range start-line start-col end-line end-col)
                                     new-text)))))))
                 #f)))
            ((string=? direction "if-to-cond")
             (let* ((parts (cdr form))
                    (test (car parts))
                    (then-expr (cadr parts))
                    (else-expr (if (pair? (cddr parts)) (caddr parts) #f))
                    (new-text (if else-expr
                                (format "(cond\n  (~a ~a)\n  (else ~a))"
                                        test then-expr else-expr)
                                (format "(cond\n  (~a ~a))"
                                        test then-expr))))
               (hash ("changes"
                      (hash (uri
                             (vector
                               (make-text-edit
                                 (make-lsp-range start-line start-col end-line end-col)
                                 new-text))))))))
            (else #f)))))))

;;; Format a list of expressions as a space-separated string
(def (format-list exprs)
  (let ((out (open-output-string)))
    (let loop ((es exprs) (first? #t))
      (when (pair? es)
        (unless first? (display " " out))
        (display (car es) out)
        (loop (cdr es) #f)))
    (get-output-string out)))

