;;; -*- Gerbil -*-
;;; Semantic tokens handler — textDocument/semanticTokens/full
;;; Provides token classification for syntax highlighting
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Semantic token type indices (must match the legend order)
(def SemanticTokenType.keyword   0)
(def SemanticTokenType.function  1)
(def SemanticTokenType.variable  2)
(def SemanticTokenType.parameter 3)
(def SemanticTokenType.type      4)
(def SemanticTokenType.macro     5)
(def SemanticTokenType.comment   6)
(def SemanticTokenType.string    7)
(def SemanticTokenType.number    8)
(def SemanticTokenType.operator  9)
(def SemanticTokenType.property  10)

;;; Semantic token modifier bit flags
(def SemanticTokenModifier.definition  1)
(def SemanticTokenModifier.readonly    2)

;;; Token type legend (order must match indices above)
(def *semantic-token-types*
  ["keyword" "function" "variable" "parameter" "type"
   "macro" "comment" "string" "number" "operator" "property"])

;;; Token modifier legend
(def *semantic-token-modifiers*
  ["definition" "readonly"])

;;; Gerbil special forms and keywords for classification
(def *gerbil-special-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("def" "define" "defn" "def*"
        "defstruct" "defclass" "defmethod" "defproto"
        "defrule" "defrules" "defsyntax" "defsyntax-call" "defsyntax-case"
        "defvalues" "defconst" "deferror-class"
        "deftable" "definterface" "implement"
        "lambda" "let" "let*" "letrec" "letrec*"
        "let-values" "let*-values" "letrec-values" "letrec*-values"
        "if" "cond" "case" "when" "unless"
        "and" "or" "not"
        "begin" "begin0" "begin-syntax" "begin-foreign" "begin-ffi"
        "begin-annotation"
        "do" "do-while" "while" "do-with-lock"
        "for" "for*" "for/collect" "for/fold"
        "set!" "set!-values"
        "values" "receive" "call-with-values" "call/values"
        "apply" "call/cc" "call-with-current-continuation"
        "with-catch" "with-exception-handler"
        "raise" "error"
        "import" "export"
        "require" "provide" "include"
        "quote" "quasiquote" "unquote" "unquote-splicing"
        "quote-syntax" "quasisyntax" "unsyntax" "unsyntax-splicing"
        "syntax" "syntax-rules" "syntax-case" "syntax/loc"
        "core-syntax-case" "core-ast-case" "core-match" "core-wrap"
        "ast-rules" "with-ast" "with-ast*" "datum->syntax"
        "match" "match*" "with" "one-of"
        "try" "catch" "finally"
        "assert" "assert!" "parameterize" "parameterize*"
        "dynamic-wind" "guard" "unwind-protect"
        "delay" "force" "declare" "using" "with-methods"
        "cond-expand" "case-lambda"
        ;; Additional keywords from gerbil-mode.el
        "rec" "alet" "alet*" "awhen" "let/cc" "let/esc"
        "define-alias" "cut"
        "sync" "wait"
        "type-of" "is"
        "spawn" "spawn*" "spawn/name" "spawn/group" "with-destroy"
        "until"
        "defmethod/alias" "with-methods" "with-class-methods"
        "with-class-method"
        "hash-eq" "hash-eqv" "let-hash"
        "chain" "continue" "yield" "coroutine"
        "<-" "<<" "->" "->>" "-->" "-->?"
        "with-result" "defcall-actor" "defapi" "deftyped"
        "defconst" "deferror-class" "do-while" "lambda%"
        "with-contract" "module" "interface"
        "with-interface" "with-struct" "with-class" "with-syntax"
        "with-syntax*" "syntax-parameterize"
        "defgeneric" "defmessage" "deftype" "definline" "definline*"
        "define-values" "define-syntaxes"
        "defproto" "extern" "defalias"
        "defrules*" "for-each" "map" "foldl" "foldr"
        "test-suite" "test-case" "with-result"
        ;; Test macros from :std/test
        "check" "checkf" "check-eq?" "check-not-eq?"
        "check-eqv?" "check-equal?" "check-not-equal?"
        "check-output" "check-predicate" "check-exception" "run-tests!"))
    ht))

;;; Gerbil macro-defining forms
(def *macro-def-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("defrule" "defrules" "defrules*" "defsyntax" "defsyntax-call" "defsyntax-case"))
    ht))

;;; Gerbil type-defining forms
(def *type-def-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("defstruct" "defclass" "deferror-class" "deftable" "definterface" "interface"))
    ht))

;;; Definition name context: keyword → (token-type . modifiers)
;;; After seeing one of these keywords, the NEXT symbol gets the indicated type+mods.
(def *def-name-forms*
  (let ((ht (make-hash-table)))
    ;; function + definition
    (for-each (lambda (s)
                (hash-put! ht s (cons SemanticTokenType.function
                                      SemanticTokenModifier.definition)))
      '("def" "defn" "def*" "define" "defvalues" "extern" "defalias"
        "definline" "definline*" "define-values" "define-syntaxes"
        "defcall-actor" "defmethod" "defgeneric" "defmessage"
        "deftype" "defproto"))
    ;; type + definition
    (for-each (lambda (s)
                (hash-put! ht s (cons SemanticTokenType.type
                                      SemanticTokenModifier.definition)))
      '("defstruct" "defclass" "deferror-class" "deftable"
        "definterface" "interface" "implement"))
    ;; macro + definition
    (for-each (lambda (s)
                (hash-put! ht s (cons SemanticTokenType.macro
                                      SemanticTokenModifier.definition)))
      '("defsyntax" "defrule" "defrules" "defrules*"
        "defsyntax-call" "defsyntax-case"))
    ;; variable + readonly (for defconst)
    (hash-put! ht "defconst"
               (cons SemanticTokenType.variable SemanticTokenModifier.readonly))
    ht))

;;; Previous token cache for delta support: uri → (result-id . encoded-data-vector)
(def *previous-tokens* (make-hash-table))
(def *result-id-counter* 0)

;;; Generate a fresh result ID
(def (next-result-id!)
  (set! *result-id-counter* (+ *result-id-counter* 1))
  (number->string *result-id-counter*))

;;; Handle textDocument/semanticTokens/full
(def (handle-semantic-tokens-full params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let* ((tokens (tokenize-source (document-text doc)))
             (data (encode-semantic-tokens tokens))
             (rid (next-result-id!)))
        ;; Cache for delta requests
        (hash-put! *previous-tokens* uri (cons rid data))
        (hash ("resultId" rid) ("data" data)))
      (hash ("data" [])))))

;;; Handle textDocument/semanticTokens/full/delta
;;; Returns edits relative to a previous token result, or a full result if previous unknown
(def (handle-semantic-tokens-delta params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (prev-rid (hash-ref params "previousResultId" ""))
         (doc (get-document uri)))
    (if doc
      (let* ((tokens (tokenize-source (document-text doc)))
             (new-data (encode-semantic-tokens tokens))
             (rid (next-result-id!))
             (cached (hash-get *previous-tokens* uri)))
        ;; Update cache
        (hash-put! *previous-tokens* uri (cons rid new-data))
        (if (and cached (string=? (car cached) prev-rid))
          ;; Compute delta edits
          (let ((edits (compute-token-edits (cdr cached) new-data)))
            (hash ("resultId" rid) ("edits" (list->vector edits))))
          ;; Previous unknown, return full result
          (hash ("resultId" rid) ("data" new-data))))
      (hash ("data" [])))))

;;; Compute semantic token edits between old and new data vectors
;;; Returns a list of edit objects: {"start": n, "deleteCount": m, "data": [...]}
(def (compute-token-edits old-data new-data)
  (let ((old-len (vector-length old-data))
        (new-len (vector-length new-data)))
    ;; Find first difference
    (let ((start (let loop ((i 0))
                   (cond
                     ((or (>= i old-len) (>= i new-len)) i)
                     ((= (vector-ref old-data i) (vector-ref new-data i))
                      (loop (+ i 1)))
                     (else i)))))
      ;; Find last difference (from the end)
      (let ((end-match (let loop ((oi (- old-len 1)) (ni (- new-len 1)) (count 0))
                         (cond
                           ((or (< oi start) (< ni start)) count)
                           ((= (vector-ref old-data oi) (vector-ref new-data ni))
                            (loop (- oi 1) (- ni 1) (+ count 1)))
                           (else count)))))
        (let ((delete-count (- old-len start end-match))
              (insert-end (- new-len end-match)))
          (if (and (= delete-count 0) (= insert-end start))
            [] ;; No changes
            (let ((insert-data
                   (let loop ((i start) (acc '()))
                     (if (>= i insert-end)
                       (list->vector (reverse acc))
                       (loop (+ i 1) (cons (vector-ref new-data i) acc))))))
              (list (hash ("start" start)
                          ("deleteCount" delete-count)
                          ("data" insert-data))))))))))

;;; Split a symbol containing a dot in the middle into (obj-part . prop-part).
;;; Returns #f if no mid-dot found.
(def (dot-accessor-split name)
  (let ((len (string-length name)))
    (if (< len 3) #f
      (let loop ((i 1))
        (cond
          ((>= i (- len 1)) #f)
          ((char=? (string-ref name i) #\.)
           (cons (substring name 0 i) (substring name (+ i 1) len)))
          (else (loop (+ i 1))))))))

;;; Tokenize source text into a list of (line col length type modifiers)
(def (tokenize-source text)
  (let ((tokens '())
        (def-ctx #f)   ;; #f or (token-type . modifiers) for definition name context
        (len (string-length text)))
    (let loop ((i 0) (line 0) (col 0))
      (if (>= i len)
        (reverse tokens)
        (let ((c (string-ref text i)))
          (cond
            ;; Newline
            ((char=? c #\newline)
             (loop (+ i 1) (+ line 1) 0))

            ;; Comment: ; to end of line
            ((char=? c #\;)
             (let ((end (find-end-of-line text i len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.comment 0)
                                  tokens))
               (set! def-ctx #f)
               (loop end line (+ col (- end i)))))

            ;; String literal
            ((char=? c #\")
             (let ((end (find-end-of-string text (+ i 1) len)))
               (let ((str-len (- end i)))
                 ;; Count lines within string
                 (let-values (((end-line end-col) (count-span text i end line col)))
                   (set! tokens (cons (list line col str-len
                                            SemanticTokenType.string 0)
                                      tokens))
                   (set! def-ctx #f)
                   (loop end end-line end-col)))))

            ;; Character literal: #\x
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (char=? (string-ref text (+ i 1)) #\\))
             (let ((end (find-end-of-char-literal text (+ i 2) len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.string 0)
                                  tokens))
               (set! def-ctx #f)
               (loop end line (+ col (- end i)))))

            ;; Boolean literal: #t or #f
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (let ((nc (string-ref text (+ i 1))))
                    (or (char=? nc #\t) (char=? nc #\f))))
             (let ((end (find-end-of-token text (+ i 1) len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.number 0)
                                  tokens))
               (set! def-ctx #f)
               (loop end line (+ col (- end i)))))

            ;; Number: starts with digit, or - followed by digit, or . followed by digit
            ((or (char-numeric? c)
                 (and (char=? c #\-)
                      (< (+ i 1) len)
                      (char-numeric? (string-ref text (+ i 1))))
                 (and (char=? c #\.)
                      (< (+ i 1) len)
                      (char-numeric? (string-ref text (+ i 1)))))
             ;; Check this is not part of a symbol
             (if (and (> i 0)
                      (symbol-char? (string-ref text (- i 1)))
                      (not (token-start? text i)))
               ;; Part of a symbol, skip
               (let ((end (find-end-of-token text i len)))
                 (loop end line (+ col (- end i))))
               (let ((end (find-end-of-token text i len)))
                 (let ((tok (substring text i end)))
                   (if (string->number tok)
                     (begin
                       (set! tokens (cons (list line col (- end i)
                                                SemanticTokenType.number 0)
                                          tokens))
                       (set! def-ctx #f)
                       (loop end line (+ col (- end i))))
                     ;; Not actually a number, classify as symbol
                     (begin
                       (set! tokens (cons (classify-symbol-token tok line col def-ctx)
                                          tokens))
                       ;; Update def-ctx based on this token
                       (set! def-ctx (next-def-ctx tok def-ctx))
                       (loop end line (+ col (- end i)))))))))

            ;; Symbol/identifier
            ((symbol-start-char? c)
             (let* ((end (find-end-of-token text i len))
                    (tok (substring text i end))
                    (dot-split (dot-accessor-split tok)))
               (if dot-split
                 ;; Dot-accessor: emit obj as variable, prop as property
                 (let ((obj-len (string-length (car dot-split)))
                       (prop-len (string-length (cdr dot-split))))
                   (set! tokens (cons (list line col obj-len
                                            SemanticTokenType.variable 0)
                                      tokens))
                   (set! tokens (cons (list line (+ col obj-len 1) prop-len
                                            SemanticTokenType.property 0)
                                      tokens))
                   (set! def-ctx #f))
                 ;; Regular symbol
                 (begin
                   (set! tokens (cons (classify-symbol-token tok line col def-ctx)
                                      tokens))
                   (set! def-ctx (next-def-ctx tok def-ctx))))
               (loop end line (+ col (- end i)))))

            ;; Quote characters: ' ` , ,@
            ((memv c '(#\' #\`))
             (loop (+ i 1) line (+ col 1)))

            ;; Skip parens, brackets, whitespace, etc.
            (else
             (loop (+ i 1) line (+ col 1)))))))))

;;; Compute the next def-ctx after emitting a token.
;;; If the token is a defining keyword, set context for next symbol.
;;; If def-ctx was set, consume it (return #f).
(def (next-def-ctx tok current-def-ctx)
  (if current-def-ctx
    ;; def-ctx was just consumed by the token we emitted — clear it
    #f
    ;; Check if this keyword should set def-ctx for next symbol
    (hash-ref *def-name-forms* tok #f)))

;;; Classify a symbol token into a semantic token type.
;;; def-ctx: #f or (token-type . modifiers) for definition name override.
(def (classify-symbol-token name line col def-ctx)
  (let ((name-len (string-length name)))
    (cond
      ;; Definition name override (e.g. after def, defstruct, etc.)
      ((and def-ctx (not (hash-key? *gerbil-special-forms* name)))
       (list line col name-len (car def-ctx) (cdr def-ctx)))
      ;; Keyword / special form
      ((hash-key? *gerbil-special-forms* name)
       (list line col name-len SemanticTokenType.keyword 0))
      ;; Macro-defining form names (also keywords)
      ((hash-key? *macro-def-forms* name)
       (list line col name-len SemanticTokenType.keyword 0))
      ;; Type-related: names ending with ::t or ? (type predicates)
      ((and (> name-len 3) (string-suffix? "::t" name))
       (list line col name-len SemanticTokenType.type 0))
      ;; Constructor: make-*
      ((and (> name-len 5) (string-prefix? "make-" name))
       (list line col name-len SemanticTokenType.function 0))
      ;; Keyword symbol ending with :
      ((and (> name-len 1)
            (char=? (string-ref name (- name-len 1)) #\:))
       (list line col name-len SemanticTokenType.parameter 0))
      ;; UPPERCASE names are typically constants
      ((all-uppercase? name)
       (list line col name-len SemanticTokenType.variable
             SemanticTokenModifier.readonly))
      ;; Default: variable
      (else
       (list line col name-len SemanticTokenType.variable 0)))))

;;; Encode tokens into LSP delta-encoded format
;;; Input: list of (line col length type modifiers), not necessarily sorted
;;; Output: flat vector of [deltaLine deltaStartChar length tokenType tokenModifiers ...]
(def (encode-semantic-tokens tokens)
  ;; Sort by line then column
  (let ((sorted (sort-tokens tokens)))
    (let loop ((ts sorted) (prev-line 0) (prev-col 0) (result '()))
      (if (null? ts)
        (list->vector (reverse result))
        (let* ((tok (car ts))
               (line (car tok))
               (col (cadr tok))
               (length (caddr tok))
               (type (cadddr tok))
               (modifiers (car (cddddr tok)))
               (delta-line (- line prev-line))
               (delta-col (if (= delta-line 0) (- col prev-col) col)))
          (loop (cdr ts)
                line col
                (cons* modifiers type length delta-col delta-line result)))))))

;;; Sort tokens by line then column
(def (sort-tokens tokens)
  (sort tokens
    (lambda (a b)
      (let ((la (car a)) (lb (car b)))
        (if (= la lb)
          (< (cadr a) (cadr b))
          (< la lb))))))

;;; cons* helper: cons multiple items onto a list
(def (cons* a b c d e rest)
  (cons a (cons b (cons c (cons d (cons e rest))))))

;;; Find end of line from position i
(def (find-end-of-line text i len)
  (let loop ((j i))
    (if (or (>= j len) (char=? (string-ref text j) #\newline))
      j
      (loop (+ j 1)))))

;;; Find end of string literal (past closing quote)
(def (find-end-of-string text i len)
  (let loop ((j i))
    (cond
      ((>= j len) j)
      ((char=? (string-ref text j) #\\)
       (loop (+ j 2))) ;; skip escaped char
      ((char=? (string-ref text j) #\")
       (+ j 1))
      (else (loop (+ j 1))))))

;;; Find end of character literal
(def (find-end-of-char-literal text i len)
  (if (>= i len) i
    ;; Handle named chars like #\space, #\newline
    (let ((end (find-end-of-token text i len)))
      (if (> end i) end (+ i 1)))))

;;; Find end of a token (symbol, number, etc.)
(def (find-end-of-token text i len)
  (let loop ((j i))
    (if (or (>= j len) (token-delimiter? (string-ref text j)))
      j
      (loop (+ j 1)))))

;;; Check if character is a token delimiter
(def (token-delimiter? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
      (char=? c #\return) (char=? c #\() (char=? c #\))
      (char=? c #\[) (char=? c #\]) (char=? c #\{) (char=? c #\})
      (char=? c #\") (char=? c #\;)))

;;; Check if a character can start a symbol
(def (symbol-start-char? c)
  (or (char-alphabetic? c)
      (memv c '(#\_ #\! #\? #\* #\+ #\/ #\< #\> #\= #\. #\: #\#
                #\% #\& #\^ #\~ #\-))))

;;; Check if position is a token start (preceded by delimiter or start of text)
(def (token-start? text i)
  (or (= i 0)
      (token-delimiter? (string-ref text (- i 1)))))

;;; Check if a string is all uppercase letters (for constant detection)
(def (all-uppercase? name)
  (let ((len (string-length name)))
    (and (> len 1)
         (let loop ((i 0) (has-alpha? #f))
           (cond
             ((>= i len) has-alpha?)
             ((char-upper-case? (string-ref name i))
              (loop (+ i 1) #t))
             ((or (char=? (string-ref name i) #\_)
                  (char=? (string-ref name i) #\-)
                  (char-numeric? (string-ref name i)))
              (loop (+ i 1) has-alpha?))
             (else #f))))))

;;; Handle textDocument/semanticTokens/range
;;; Only tokenize within the requested range for better performance
(def (handle-semantic-tokens-range params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (start (hash-ref range "start" (hash)))
         (end (hash-ref range "end" (hash)))
         (start-line (hash-ref start "line" 0))
         (end-line (hash-ref end "line" 0))
         (doc (get-document uri)))
    (if doc
      (let ((tokens (tokenize-source-range (document-text doc) start-line end-line)))
        (hash ("data" (encode-semantic-tokens tokens))))
      (hash ("data" [])))))

;;; Tokenize only within a line range (for range requests)
(def (tokenize-source-range text start-line end-line)
  (let ((all-tokens (tokenize-source text)))
    ;; Filter to only tokens within the requested range
    (filter
      (lambda (tok)
        (let ((line (car tok)))
          (and (>= line start-line) (<= line end-line))))
      all-tokens)))

;;; Count lines/cols in a span of text
(def (count-span text start end cur-line cur-col)
  (let loop ((i start) (line cur-line) (col cur-col))
    (cond
      ((>= i end) (values line col))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ line 1) 0))
      (else
       (loop (+ i 1) line (+ col 1))))))
