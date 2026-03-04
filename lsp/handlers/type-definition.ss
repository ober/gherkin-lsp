;;; -*- Gerbil -*-
;;; Type definition handler — textDocument/typeDefinition
;;; Uses heuristics to navigate from constructors/predicates/accessors to their type
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/typeDefinition
;;; Given a symbol like make-point, point?, or point-x, navigate to the struct/class
(def (handle-type-definition params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name _start-col _end-col)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (find-type-definition sym-name uri)
          (void)))
      (void))))

;;; Find the type definition for a symbol using heuristics
(def (find-type-definition name uri)
  (let ((type-name (infer-type-name name)))
    (if type-name
      (find-type-symbol type-name uri)
      ;; Fallback: check if the symbol itself is a type
      (find-type-symbol name uri))))

;;; Infer the type name from a symbol name using common patterns
;;; make-Foo → Foo, Foo? → Foo, Foo-bar → Foo, Foo-bar-set! → Foo
(def (infer-type-name name)
  (cond
    ;; make-X constructor → X
    ((string-prefix? "make-" name)
     (substring name 5 (string-length name)))
    ;; X? predicate → X
    ((and (> (string-length name) 1)
          (char=? (string-ref name (- (string-length name) 1)) #\?))
     (substring name 0 (- (string-length name) 1)))
    ;; X-field-set! mutator → X
    ((string-suffix? "-set!" name)
     (let ((base (substring name 0 (- (string-length name) 5))))
       (extract-type-prefix base)))
    ;; X-field accessor → X (only if X is a known type)
    ((string-contains-char name #\-)
     (extract-type-prefix name))
    (else #f)))

;;; Extract the type prefix from "Type-field" patterns
;;; Tries progressively shorter prefixes: "Foo-bar-baz" → "Foo-bar", "Foo"
(def (extract-type-prefix name)
  (let loop ((i (- (string-length name) 1)))
    (cond
      ((<= i 0) #f)
      ((char=? (string-ref name i) #\-)
       (substring name 0 i))
      (else (loop (- i 1))))))

;;; Find a type symbol (struct or class) by name in the workspace
(def (find-type-symbol type-name uri)
  ;; Search local file first
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-type-in-syms type-name local-syms)))
      (if found
        (make-lsp-location uri
          (make-lsp-range (sym-info-line found) (sym-info-col found)
                          (sym-info-end-line found) (sym-info-end-col found)))
        ;; Search workspace index
        (let ((result (find-type-in-workspace type-name)))
          (if result
            (let ((def-uri (car result))
                  (info (cdr result)))
              (make-lsp-location def-uri
                (make-lsp-range (sym-info-line info) (sym-info-col info)
                                (sym-info-end-line info) (sym-info-end-col info))))
            (void)))))))

;;; Find a type symbol in a list of sym-infos
(def (find-type-in-syms name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (let ((s (car ss)))
        (if (and (string=? name (sym-info-name s))
                 (or (= (sym-info-kind s) SymbolKind.Struct)
                     (= (sym-info-kind s) SymbolKind.Class)))
          s
          (loop (cdr ss)))))))

;;; Find a type symbol across the workspace index
(def (find-type-in-workspace name)
  (let ((defs (find-definitions-by-name name)))
    (let loop ((ds defs))
      (if (null? ds) #f
        (let ((info (cdr (car ds))))
          (if (or (= (sym-info-kind info) SymbolKind.Struct)
                  (= (sym-info-kind info) SymbolKind.Class))
            (car ds)
            (loop (cdr ds))))))))

;;; Check if a string contains a character
(def (string-contains-char str ch)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) #t)
      (else (loop (+ i 1))))))
