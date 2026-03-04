;;; -*- Gerbil -*-
;;; Completion candidate generation
(import ../types
        ../util/log
        ../util/string
        ../util/position
        ../state
        ./symbols
        ./parser
        ./module)
(export #t)

;;; Gerbil special forms and keywords
(def *gerbil-keywords*
  '("def" "define" "defn" "def*"
    "defstruct" "defclass" "defmethod" "defproto"
    "defrule" "defrules" "defsyntax" "defsyntax-case"
    "defvalues" "defconst"
    "deferror-class"
    "deftable" "definterface" "implement"
    "lambda" "let" "let*" "letrec" "letrec*"
    "let-values" "let*-values"
    "if" "cond" "case" "when" "unless"
    "and" "or" "not"
    "begin" "begin0"
    "do" "do-while" "do-with-lock"
    "for" "for*" "for/collect" "for/fold"
    "while"
    "set!" "set!-values"
    "values" "receive" "call-with-values"
    "apply" "call/cc" "call-with-current-continuation"
    "with-catch" "with-exception-handler"
    "raise" "error"
    "import" "export"
    "require" "provide"
    "include"
    "quote" "quasiquote" "unquote" "unquote-splicing"
    "syntax" "syntax-rules" "syntax-case"
    "match" "with" "one-of"
    "try" "catch" "finally"
    "assert"
    "parameterize"
    "dynamic-wind"
    "guard"
    "delay" "force"
    "declare"
    "using" "with-methods"))

;;; Classify the cursor context for completion filtering.
;;; Returns: 'string | 'comment | 'import | 'head | 'top-level | 'argument
(def (completion-context text line col)
  (let* ((regions (classify-text-regions text))
         (offset (line-col->offset text line col)))
    (cond
      ;; In string or comment — suppress completions
      ((and (< offset (u8vector-length regions))
            (= (u8vector-ref regions offset) 1))
       'string)
      ((and (< offset (u8vector-length regions))
            (= (u8vector-ref regions offset) 2))
       'comment)
      (else
       ;; Determine form context from the line text
       (let* ((line-text (text-line-at text line))
              (trimmed (let trim ((s line-text))
                         (if (and (> (string-length s) 0)
                                  (char-whitespace? (string-ref s 0)))
                           (trim (substring s 1 (string-length s)))
                           s))))
         (cond
           ;; Inside an import form
           ((and (> (string-length trimmed) 0)
                 (or (string-prefix? "(import" trimmed)
                     ;; Check if current line is indented under an import
                     (in-import-form? text line)))
            'import)
           ;; Head position: cursor right after opening paren
           ((head-position? line-text col)
            'head)
           ;; Top-level: column 0 or just after (
           ((= col 0) 'top-level)
           ;; Otherwise it's an argument position
           (else 'argument)))))))

;;; Check if cursor is in head position (right after an open paren)
(def (head-position? line-text col)
  (and (> col 0)
       (< col (string-length line-text))
       ;; Scan back to see if the previous non-space is (
       (let loop ((i (- col 1)))
         (cond
           ((< i 0) #f)
           ((char=? (string-ref line-text i) #\() #t)
           ((char-whitespace? (string-ref line-text i)) (loop (- i 1)))
           (else #f)))))

;;; Check if a line is inside an import form by scanning upward
(def (in-import-form? text line)
  (let loop ((l (- line 1)))
    (if (< l 0) #f
      (let ((lt (text-line-at text l)))
        (cond
          ;; Found import form start
          ((string-contains lt "(import")
           ;; Check it hasn't been closed before our line
           #t)
          ;; Non-indented line — we've gone past the form
          ((and (> (string-length lt) 0)
                (not (char-whitespace? (string-ref lt 0))))
           #f)
          (else (loop (- l 1))))))))

;;; Generate completion candidates for a position in a document
(def (completion-candidates uri text line col)
  (let ((prefix (get-completion-prefix text line col))
        (ctx (completion-context text line col))
        (result '())
        (seen (make-hash-table)))  ;; deduplicate by name

    ;; Helper to add an item, deduplicating by label
    (define (add-item! item)
      (let ((label (hash-ref item "label" "")))
        (unless (hash-key? seen label)
          (hash-put! seen label #t)
          (set! result (cons item result)))))

    (lsp-debug "completion prefix: ~s context: ~a" prefix ctx)

    ;; Suppress completions in strings and comments
    (when (memq ctx '(string comment))
      (set! result '())
      ;; Force early return by setting prefix to impossible value
      (set! prefix #f))

    ;; Local symbols from this file (sort-prefix "0" = highest priority)
    (let ((file-syms (get-file-symbols uri)))
      (for-each
        (lambda (s)
          (when (or (not prefix) (string-prefix? prefix (sym-info-name s)))
            (add-item! (sym-info->completion-item s sort-prefix: "0"))))
        file-syms))

    ;; Symbols from the file's imports (sort-prefix "1" = imported)
    (with-catch
      (lambda (e) (lsp-debug "import completion failed: ~a" e))
      (lambda ()
        (let* ((file-path (uri->file-path uri))
               (forms (parse-source-resilient text))
               (imported (get-imported-symbols forms file-path)))
          (for-each
            (lambda (s)
              (when (or (not prefix) (string-prefix? prefix (sym-info-name s)))
                (add-item! (sym-info->completion-item s detail-uri: "imported"
                                                       sort-prefix: "1"))))
            imported))))

    ;; Keywords (sort-prefix depends on context — rank higher in head position)
    (let ((kw-sort (if (eq? ctx 'head) "0" "3")))
      (for-each
        (lambda (kw)
          (when (or (not prefix) (string-prefix? prefix kw))
            (add-item! (hash ("label" kw)
                             ("kind" CompletionItemKind.Keyword)
                             ("detail" "keyword")
                             ("sortText" (string-append kw-sort kw))))))
        *gerbil-keywords*))

    ;; Symbols from all indexed files (sort-prefix "2" = workspace)
    (hash-for-each
      (lambda (other-uri syms)
        (unless (string=? other-uri uri)
          (for-each
            (lambda (s)
              (when (or (not prefix) (string-prefix? prefix (sym-info-name s)))
                (add-item! (sym-info->completion-item s detail-uri: other-uri
                                                       sort-prefix: "2"))))
            syms)))
      *symbol-index*)

    ;; Standard library symbols (sort-prefix "3" = stdlib fallback)
    (for-each
      (lambda (entry)
        (let ((name (car entry))
              (mod (cadr entry)))
          (when (or (not prefix) (string-prefix? prefix name))
            (add-item! (hash ("label" name)
                             ("kind" CompletionItemKind.Function)
                             ("detail" mod)
                             ("sortText" (string-append "3" name))
                             ("data" (hash ("module" mod)
                                           ("name" name))))))))
      *stdlib-symbols*)

    ;; Snippet completions
    (for-each add-item! (snippet-completions prefix))
    result))

;;; Convert a sym-info to a CompletionItem
;;; sort-prefix controls ranking: "0" = local, "1" = imported, "2" = workspace
(def (sym-info->completion-item s detail-uri: (detail-uri #f) sort-prefix: (sort-prefix "2"))
  (let ((kind (sym-kind->completion-kind (sym-info-kind s)))
        (detail (or (sym-info-detail s)
                    (and detail-uri (string-append "from " detail-uri))
                    "")))
    (hash ("label" (sym-info-name s))
          ("kind" kind)
          ("detail" detail)
          ("sortText" (string-append sort-prefix (sym-info-name s))))))

;;; Map SymbolKind to CompletionItemKind
(def (sym-kind->completion-kind sk)
  (cond
    ((= sk SymbolKind.Function) CompletionItemKind.Function)
    ((= sk SymbolKind.Variable) CompletionItemKind.Variable)
    ((= sk SymbolKind.Constant) CompletionItemKind.Variable)
    ((= sk SymbolKind.Struct)   CompletionItemKind.Struct)
    ((= sk SymbolKind.Class)    CompletionItemKind.Class)
    ((= sk SymbolKind.Method)   CompletionItemKind.Method)
    (else CompletionItemKind.Text)))

;;; Get the symbol prefix at the cursor for filtering
;;; Returns a string prefix or #f if no prefix
(def (get-completion-prefix text line col)
  (let ((line-text (get-line-text text line)))
    (if (or (string=? line-text "") (= col 0))
      #f
      (let loop ((i (min (- col 1) (- (string-length line-text) 1)))
                 (acc '()))
        (if (or (< i 0) (not (completion-char? (string-ref line-text i))))
          (if (null? acc) #f (list->string acc))
          (loop (- i 1) (cons (string-ref line-text i) acc)))))))

;;; Get a line from text by line number
(def (get-line-text text line-num)
  (let loop ((i 0) (cur 0) (start 0))
    (cond
      ((>= i (string-length text))
       (if (= cur line-num) (substring text start i) ""))
      ((char=? (string-ref text i) #\newline)
       (if (= cur line-num)
         (substring text start i)
         (loop (+ i 1) (+ cur 1) (+ i 1))))
      (else (loop (+ i 1) cur start)))))

;;; Standard library symbols with their source modules
;;; Curated list of commonly used exports for rich completions
(def *stdlib-symbols*
  '(;; :std/sugar
    ("when-let" ":std/sugar") ("if-let" ":std/sugar")
    ("with-destroy" ":std/sugar") ("ignore-errors" ":std/sugar")
    ("awhen" ":std/sugar") ("chain" ":std/sugar") ("is" ":std/sugar")
    ("with-id" ":std/sugar") ("let-hash" ":std/sugar")
    ("using-method" ":std/sugar") ("with-methods" ":std/sugar")
    ;; :std/iter
    ("for" ":std/iter") ("for*" ":std/iter")
    ("for/collect" ":std/iter") ("for/fold" ":std/iter")
    ("in-range" ":std/iter") ("in-iota" ":std/iter")
    ("in-naturals" ":std/iter") ("in-hash" ":std/iter")
    ("in-hash-keys" ":std/iter") ("in-hash-values" ":std/iter")
    ("in-input-lines" ":std/iter") ("in-input-chars" ":std/iter")
    ("yield" ":std/iter") ("iter" ":std/iter")
    ;; :std/text/json
    ("read-json" ":std/text/json") ("write-json" ":std/text/json")
    ("string->json-object" ":std/text/json")
    ("json-object->string" ":std/text/json")
    ("bytes->json-object" ":std/text/json")
    ("json-object->bytes" ":std/text/json")
    ("pretty-json" ":std/text/json")
    ;; :std/misc/ports
    ("read-file-string" ":std/misc/ports")
    ("read-all-as-string" ":std/misc/ports")
    ("read-file-lines" ":std/misc/ports")
    ("read-all-as-lines" ":std/misc/ports")
    ("write-file-string" ":std/misc/ports")
    ("write-file-lines" ":std/misc/ports")
    ("copy-port" ":std/misc/ports")
    ("writeln" ":std/misc/ports")
    ;; :std/misc/string
    ("string-trim-prefix" ":std/misc/string")
    ("string-trim-suffix" ":std/misc/string")
    ("string-trim-eol" ":std/misc/string")
    ("string-split-prefix" ":std/misc/string")
    ("string-split-suffix" ":std/misc/string")
    ("string-split-eol" ":std/misc/string")
    ("string-subst" ":std/misc/string")
    ("string-whitespace?" ":std/misc/string")
    ("str" ":std/misc/string")
    ;; :std/misc/process
    ("run-process" ":std/misc/process")
    ("run-process/batch" ":std/misc/process")
    ("invoke" ":std/misc/process")
    ("filter-with-process" ":std/misc/process")
    ;; :std/misc/path
    ("path-simplify" ":std/misc/path")
    ("path-extension-is?" ":std/misc/path")
    ("path-parent" ":std/misc/path")
    ("path-default-extension" ":std/misc/path")
    ("subpath" ":std/misc/path")
    ("absolute-path?" ":std/misc/path")
    ;; :std/error
    ("with-exception-stack-trace" ":std/error")
    ("deferror-class" ":std/error")
    ("raise-bad-argument" ":std/error")
    ("check-argument" ":std/error")
    ("dump-stack-trace!" ":std/error")
    ;; :std/sort
    ("sort" ":std/sort") ("sort!" ":std/sort")
    ("stable-sort" ":std/sort") ("stable-sort!" ":std/sort")
    ;; :std/format
    ("format" ":std/format") ("fprintf" ":std/format")
    ("printf" ":std/format")
    ;; v0.19 relocated modules
    ("list-sort" ":std/list/list") ("list-sort!" ":std/list/list")
    ("object-class" ":gerbil/runtime/mop")
    ;; v0.19 sync modules
    ("channel-put" ":std/sync/channel") ("channel-get" ":std/sync/channel")
    ("make-channel" ":std/sync/channel")
    ("make-completion" ":std/sync/completion")
    ("completion-post!" ":std/sync/completion")
    ("completion-wait!" ":std/sync/completion")
    ;; v0.19 string/path module
    ("path-simplify" ":std/string/path") ("path-parent" ":std/string/path")
    ;; v0.19 iterator forms
    ("in-range-inclusive" ":std/iter") ("in-integers" ":std/iter")
    ("in-number-series" ":std/iter")))

;;; Characters that can appear in completion prefixes
(def (completion-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\! #\? #\* #\+ #\/ #\< #\> #\= #\. #\: #\#
                #\% #\& #\^ #\~))))

;;; Snippet templates for common Gerbil forms
(def *snippet-templates*
  '(("defstruct" "(defstruct ${1:name} (${2:fields})\n  transparent: #t)" "Define a struct type")
    ("defclass" "(defclass ${1:name} (${2:fields})\n  constructor: :init!)" "Define a class type")
    ("def-fn" "(def (${1:name} ${2:args})\n  ${0:body})" "Define a function")
    ("let" "(let ((${1:var} ${2:val}))\n  ${0:body})" "Let binding")
    ("let*" "(let* ((${1:var} ${2:val}))\n  ${0:body})" "Sequential let binding")
    ("match" "(match ${1:expr}\n  (${2:pattern} ${0:body}))" "Pattern match")
    ("test-suite" "(test-suite \"${1:name}\"\n  (test-case \"${2:case}\"\n    (check ${0:expr} => expected)))" "Test suite")
    ("test-case" "(test-case \"${1:name}\"\n  (check ${0:expr} => expected))" "Test case")
    ("cond" "(cond\n  (${1:test} ${2:expr})\n  (else ${0:default}))" "Conditional")
    ("lambda" "(lambda (${1:args})\n  ${0:body})" "Lambda expression")
    ("import" "(import ${0:module})" "Import module")
    ("export" "(export #t)" "Export all")
    ("definterface" "(definterface ${1:Name}\n  (${2:method} (${3:args})))" "Define an interface (v0.19)")
    ("implement" "(implement ${1:ClassName} ${2:InterfaceName}\n  (def (${3:method} self ${4:args})\n    ${0:body}))" "Implement interface (v0.19)")
    ("defsyntax-case" "(defsyntax-case (${1:name} stx)\n  ((_ ${2:pattern})\n   ${0:body}))" "Define syntax-case macro (v0.19)")))

;;; Get snippet completions matching a prefix
(def (snippet-completions prefix)
  (let ((result '()))
    (for-each
      (lambda (entry)
        (let ((name (car entry))
              (template (cadr entry))
              (doc (caddr entry)))
          (when (or (not prefix) (string-prefix? prefix name))
            (set! result
              (cons (hash ("label" name)
                          ("kind" CompletionItemKind.Snippet)
                          ("detail" "snippet")
                          ("insertText" template)
                          ("insertTextFormat" InsertTextFormat.Snippet)
                          ("documentation"
                           (hash ("kind" MarkupKind.Markdown)
                                 ("value" doc))))
                    result)))))
      *snippet-templates*)
    result))
