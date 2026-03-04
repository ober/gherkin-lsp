;;; -*- Gerbil -*-
;;; Inlay hints handler — textDocument/inlayHint
;;; Shows parameter names at function call sites
(import ../compat/compat
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; InlayHintKind constants
(def InlayHintKind.Type      1)
(def InlayHintKind.Parameter 2)

;;; Handle textDocument/inlayHint
;;; Returns InlayHint[] for the visible range
(def (handle-inlay-hint params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (start (hash-ref range "start" (hash)))
         (end (hash-ref range "end" (hash)))
         (start-line (hash-ref start "line" 0))
         (end-line (hash-ref end "line" 0))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (forms (parse-source text))
             (hints (collect-inlay-hints forms text uri start-line end-line)))
        (list->vector hints))
      [])))

;;; Collect inlay hints from parsed forms within a line range
(def (collect-inlay-hints forms text uri start-line end-line)
  (let ((result '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf))
              (fl (located-form-line lf)))
          ;; Only process forms that overlap the visible range
          (when (and (<= fl end-line)
                     (>= (located-form-end-line lf) start-line))
            (let ((call-hints (extract-call-hints form text uri fl))
                  (type-hints (extract-let-type-hints form text fl)))
              (set! result (append result call-hints type-hints))))))
      forms)
    result))

;;; Extract inlay hints from a form (recursively walks subforms)
;;; Looks for function calls where we know the parameter names
(def (extract-call-hints form text uri context-line)
  (if (not (pair? form))
    '()
    (let ((result '()))
      ;; Check if this is a function call with arguments
      (when (and (symbol? (car form))
                 (pair? (cdr form)))
        (let* ((func-name (symbol->string (car form)))
               (args (cdr form))
               (param-names (lookup-param-names func-name uri)))
          (when (and param-names (pair? param-names))
            (set! result (append result
                           (generate-param-hints func-name args param-names
                                                  text context-line))))))
      ;; Recurse into subforms
      (for-each
        (lambda (sub)
          (when (pair? sub)
            (set! result (append result
                           (extract-call-hints sub text uri context-line)))))
        (if (pair? form) (cdr form) '()))
      result)))

;;; Look up parameter names for a function
;;; Returns a list of parameter name strings, or #f
(def (lookup-param-names func-name uri)
  ;; First check local symbols
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-func-with-params func-name local-syms)))
      (if found found
        ;; Check workspace symbols
        (let ((defs (find-definitions-by-name func-name)))
          (let loop ((ds defs))
            (if (null? ds) #f
              (let ((info (cdr (car ds))))
                (let ((params (extract-param-names-from-detail info)))
                  (if params params
                    (loop (cdr ds))))))))))))

;;; Find a function symbol and extract its parameter names
(def (find-func-with-params name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (let ((s (car ss)))
        (if (and (string=? name (sym-info-name s))
                 (= (sym-info-kind s) SymbolKind.Function))
          (extract-param-names-from-detail s)
          (loop (cdr ss)))))))

;;; Extract parameter names from a sym-info's detail string
;;; Detail is like "(func-name arg1 arg2 . rest)"
(def (extract-param-names-from-detail info)
  (let ((detail (sym-info-detail info)))
    (if (and detail (> (string-length detail) 0))
      (with-catch
        (lambda (e) #f)
        (lambda ()
          (let ((form (read (open-input-string detail))))
            (if (and (pair? form) (pair? (cdr form)))
              (let ((raw-params (cdr form)))
                (param-list->names raw-params))
              #f))))
      #f)))

;;; Convert a parameter list to a list of name strings
;;; Handles: (a b c), (a b . rest), ((a default) b), keyword: args
(def (param-list->names params)
  (cond
    ((null? params) '())
    ((symbol? params) (list (string-append (symbol->string params) "...")))
    ((pair? params)
     (let ((param (car params)))
       (cons
         (cond
           ((pair? param) (format "~a" (car param)))
           ((symbol? param)
            (let ((s (symbol->string param)))
              ;; Skip keyword parameters ending in :
              (if (and (> (string-length s) 0)
                       (char=? (string-ref s (- (string-length s) 1)) #\:))
                s
                s)))
           (else (format "~a" param)))
         (param-list->names (cdr params)))))
    (else '())))

;;; Generate InlayHint objects for parameter names at call sites
;;; We need to find the actual source positions of arguments in the text
(def (generate-param-hints func-name args param-names text context-line)
  (let ((hints '())
        (params param-names)
        (arg-index 0))
    ;; Skip creating hints for common well-known functions where hints are noise
    (if (member func-name '("cons" "car" "cdr" "list" "vector" "hash"
                             "+" "-" "*" "/" "=" "<" ">" "<=" ">="
                             "eq?" "equal?" "string=?" "not" "and" "or"
                             "display" "displayln" "newline" "write"
                             "set!" "hash-ref" "hash-put!"))
      '()
      ;; Only show hints if we have at least 2 params (single-arg is obvious)
      (if (< (length param-names) 2)
        '()
        (let* ((line-text (text-line-at text context-line))
               (arg-cols (find-arg-positions line-text)))
          (let loop ((as args) (ps params) (idx 0))
            (if (or (null? as) (null? ps))
              (reverse hints)
              (let* ((param-name (car ps))
                     ;; Skip keyword params (they're self-documenting)
                     (is-keyword? (and (> (string-length param-name) 0)
                                       (char=? (string-ref param-name
                                                 (- (string-length param-name) 1))
                                               #\:)))
                     ;; Use actual column if available, fall back to 0
                     (col (if (< idx (length arg-cols))
                            (list-ref arg-cols idx)
                            0)))
                (if is-keyword?
                  ;; Skip keyword and its value
                  (loop (if (pair? (cdr as)) (cddr as) '())
                        (if (pair? (cdr ps)) (cddr ps) '())
                        (+ idx 2))
                  (begin
                    (set! hints (cons (make-inlay-hint
                                        context-line
                                        col
                                        param-name
                                        InlayHintKind.Parameter)
                                      hints))
                    (loop (cdr as) (cdr ps) (+ idx 1))))))))))))

;;; Handle inlayHint/resolve — return the hint unchanged
(def (handle-inlay-hint-resolve params)
  params)

;;; Find the column positions of each argument in a function call on a line.
;;; Given a line like "(my-func arg1 (+ 1 2) arg3)" finds the start column
;;; of each argument after the function name.
;;; Returns a list of 0-based column positions, one per argument found.
(def (find-arg-positions line)
  (let ((len (string-length line)))
    (if (= len 0) '()
      ;; Find the opening paren
      (let loop-start ((i 0))
        (cond
          ((>= i len) '())
          ((char=? (string-ref line i) #\()
           ;; Skip past function name
           (let skip-func ((j (+ i 1)))
             (cond
               ((>= j len) '())
               ((char-whitespace? (string-ref line j))
                ;; Now find each argument start position
                (let loop-args ((k j) (positions '()) (depth 0) (in-string? #f) (escape? #f))
                  (cond
                    ((>= k len) (reverse positions))
                    ;; Handle escape in string
                    (escape?
                     (loop-args (+ k 1) positions depth in-string? #f))
                    ;; Handle string content
                    (in-string?
                     (let ((c (string-ref line k)))
                       (cond
                         ((char=? c #\\)
                          (loop-args (+ k 1) positions depth #t #t))
                         ((char=? c #\")
                          (loop-args (+ k 1) positions depth #f #f))
                         (else
                          (loop-args (+ k 1) positions depth #t #f)))))
                    (else
                     (let ((c (string-ref line k)))
                       (cond
                         ;; String start
                         ((char=? c #\")
                          (if (and (= depth 0)
                                   (or (null? positions)
                                       ;; previous char was whitespace = new arg
                                       (and (> k 0)
                                            (char-whitespace? (string-ref line (- k 1))))))
                            (loop-args (+ k 1) (cons k positions) 0 #t #f)
                            (loop-args (+ k 1) positions depth #t #f)))
                         ;; Open paren at depth 0 = start of nested arg
                         ((char=? c #\()
                          (if (and (= depth 0)
                                   (> k j)
                                   (or (null? positions)
                                       (and (> k 0)
                                            (char-whitespace? (string-ref line (- k 1))))))
                            (loop-args (+ k 1) (cons k positions) (+ depth 1) #f #f)
                            (loop-args (+ k 1) positions (+ depth 1) #f #f)))
                         ;; Close paren
                         ((char=? c #\))
                          (if (= depth 0)
                            (reverse positions)  ;; end of call
                            (loop-args (+ k 1) positions (- depth 1) #f #f)))
                         ;; Whitespace at depth 0 = potential arg boundary
                         ((and (char-whitespace? c) (= depth 0))
                          (loop-args (+ k 1) positions 0 #f #f))
                         ;; Non-whitespace at depth 0 after whitespace = new arg
                         ((and (= depth 0)
                               (> k 0)
                               (char-whitespace? (string-ref line (- k 1))))
                          (loop-args (+ k 1) (cons k positions) 0 #f #f))
                         (else
                          (loop-args (+ k 1) positions depth #f #f))))))))
               ;; Still in function name
               (else (skip-func (+ j 1))))))
          ;; Skip leading whitespace before paren
          (else (loop-start (+ i 1))))))))

;;; Create an InlayHint object
(def (make-inlay-hint line col label kind)
  (hash ("position" (make-lsp-position line col))
        ("label" (string-append label ":"))
        ("kind" kind)
        ("paddingRight" #t)))

;;; ====================================================================
;;; Let-binding type hints
;;; ====================================================================

;;; Known constructor → type mappings for heuristic type inference
(def *constructor-types*
  (let ((ht (make-hash-table)))
    (for-each
      (lambda (pair) (hash-put! ht (car pair) (cdr pair)))
      '(("make-hash-table" . "HashTable")
        ("hash" . "HashTable")
        ("open-input-string" . "Port")
        ("open-output-string" . "Port")
        ("open-input-file" . "Port")
        ("open-output-file" . "Port")
        ("open-input-u8vector" . "Port")
        ("open-output-u8vector" . "Port")
        ("current-input-port" . "Port")
        ("current-output-port" . "Port")
        ("current-error-port" . "Port")
        ("make-mutex" . "Mutex")
        ("make-condition-variable" . "CondVar")
        ("make-thread" . "Thread")
        ("make-parameter" . "Parameter")
        ("make-channel" . "Channel")
        ("make-u8vector" . "u8vector")
        ("make-vector" . "vector")
        ("make-string" . "string")
        ("string-append" . "string")
        ("string-copy" . "string")
        ("substring" . "string")
        ("number->string" . "string")
        ("symbol->string" . "string")
        ("format" . "string")
        ("get-output-string" . "string")
        ("string->number" . "number?")
        ("string-length" . "int")
        ("vector-length" . "int")
        ("length" . "int")
        ("char->integer" . "int")
        ("string->symbol" . "symbol")
        ("gensym" . "symbol")
        ("cons" . "pair")
        ("list" . "list")
        ("append" . "list")
        ("reverse" . "list")
        ("map" . "list")
        ("filter" . "list")
        ("sort" . "list")
        ("vector" . "vector")
        ("list->vector" . "vector")
        ("vector->list" . "list")
        ("read" . "datum")
        ("read-line" . "string?")
        ("read-char" . "char?")
        ("string-ref" . "char")
        ("integer->char" . "char")
        ("regexp" . "Regexp")
        ("pregexp" . "Regexp")))
    ht))

;;; Extract type hints from let/let*/letrec bindings
(def (extract-let-type-hints form text context-line)
  (if (not (pair? form))
    '()
    (let ((head (car form))
          (result '()))
      ;; Check for let/let*/letrec forms
      (when (and (symbol? head)
                 (memq head '(let let* letrec letrec*)))
        (let ((bindings (and (pair? (cdr form))
                             ;; let can have an optional name: (let name ((bindings)) body)
                             (if (symbol? (cadr form))
                               (and (pair? (cddr form)) (caddr form))
                               (cadr form)))))
          (when (and bindings (list? bindings))
            (for-each
              (lambda (binding)
                (when (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding)))
                  (let* ((var-name (symbol->string (car binding)))
                         (init-expr (cadr binding))
                         (inferred (infer-expression-type init-expr)))
                    (when inferred
                      ;; Find the position of the variable in text
                      ;; We place the hint after the variable name
                      (let ((hint-line context-line))
                        (set! result
                          (cons (hash
                                  ("position" (make-lsp-position hint-line 0))
                                  ("label" (string-append ": " inferred))
                                  ("kind" InlayHintKind.Type)
                                  ("paddingLeft" #t))
                                result)))))))
              bindings))))
      ;; Recurse into subforms
      (for-each
        (lambda (sub)
          (when (pair? sub)
            (set! result (append result
                           (extract-let-type-hints sub text context-line)))))
        (if (pair? form) (cdr form) '()))
      result)))

;;; Infer the type of an expression based on known constructors
(def (infer-expression-type expr)
  (cond
    ;; Direct constructor call: (make-hash-table), (open-input-string ...)
    ((and (pair? expr) (symbol? (car expr)))
     (let ((func-name (symbol->string (car expr))))
       ;; Check constructor map
       (let ((type (hash-get *constructor-types* func-name)))
         (if type type
           ;; Heuristic: make-FOO → FOO
           (if (and (> (string-length func-name) 5)
                    (string-prefix? "make-" func-name))
             (substring func-name 5 (string-length func-name))
             #f)))))
    ;; String literal
    ((string? expr) "string")
    ;; Number literal
    ((number? expr) "number")
    ;; Boolean literal
    ((boolean? expr) "bool")
    ;; Character literal
    ((char? expr) "char")
    ;; Vector literal
    ((vector? expr) "vector")
    (else #f)))
