;;; -*- Gerbil -*-
;;; Type hierarchy handlers — prepareTypeHierarchy, supertypes, subtypes
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index
        ../analysis/module)
(export #t)

;;; Handle textDocument/prepareTypeHierarchy
;;; Find the struct/class at cursor position
(def (handle-prepare-type-hierarchy params)
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
            (let ((file-syms (get-file-symbols uri)))
              (let ((found (find-sym-by-name sym file-syms)))
                (if (and found
                         (or (= (sym-info-kind found) SymbolKind.Struct)
                             (= (sym-info-kind found) SymbolKind.Class)))
                  (vector (make-type-hierarchy-item found uri))
                  ;; Also search workspace-wide
                  (let ((defs (find-definitions-by-name sym)))
                    (let ((type-defs
                           (filter
                             (lambda (d)
                               (let ((s (cdr d)))
                                 (or (= (sym-info-kind s) SymbolKind.Struct)
                                     (= (sym-info-kind s) SymbolKind.Class))))
                             defs)))
                      (if (pair? type-defs)
                        (vector (make-type-hierarchy-item
                                  (cdar type-defs) (caar type-defs)))
                        (vector)))))))
            (vector))))
      (vector))))

;;; Handle typeHierarchy/supertypes
;;; Find the parent type from the detail string
(def (handle-supertypes params)
  (let* ((item (hash-ref params "item" (hash)))
         (name (hash-ref item "name" ""))
         (uri (hash-ref item "uri" ""))
         (data (hash-ref item "data" (hash)))
         (detail (hash-ref data "detail" "")))
    ;; Parse parent from detail string: "struct X < Parent" or "class X < Parent"
    (let ((parent-name (extract-parent-from-detail detail)))
      (if parent-name
        ;; Find the parent definition
        (let ((defs (find-definitions-by-name parent-name)))
          (let ((type-defs
                 (filter
                   (lambda (d)
                     (let ((s (cdr d)))
                       (or (= (sym-info-kind s) SymbolKind.Struct)
                           (= (sym-info-kind s) SymbolKind.Class))))
                   defs)))
            (if (pair? type-defs)
              (vector (make-type-hierarchy-item (cdar type-defs) (caar type-defs)))
              (vector))))
        (vector)))))

;;; Handle typeHierarchy/subtypes
;;; Find all struct/class definitions whose parent matches the given type
(def (handle-subtypes params)
  (let* ((item (hash-ref params "item" (hash)))
         (name (hash-ref item "name" ""))
         (result '()))
    ;; Scan all indexed symbols for structs/classes with this parent
    (hash-for-each
      (lambda (sym-uri syms)
        (for-each
          (lambda (s)
            (when (or (= (sym-info-kind s) SymbolKind.Struct)
                      (= (sym-info-kind s) SymbolKind.Class))
              (let ((detail (or (sym-info-detail s) "")))
                (let ((parent (extract-parent-from-detail detail)))
                  (when (and parent (string=? parent name))
                    (set! result
                      (cons (make-type-hierarchy-item s sym-uri) result)))))))
          syms))
      *symbol-index*)
    (list->vector result)))

;;; Create a TypeHierarchyItem from a sym-info
(def (make-type-hierarchy-item sym uri)
  (let ((line (sym-info-line sym))
        (col (sym-info-col sym))
        (end-line (sym-info-end-line sym))
        (end-col (sym-info-end-col sym))
        (name (sym-info-name sym))
        (kind (sym-info-kind sym))
        (detail (or (sym-info-detail sym) "")))
    (hash ("name" name)
          ("kind" kind)
          ("uri" uri)
          ("range" (make-lsp-range line col end-line end-col))
          ("selectionRange" (make-lsp-range line col line (+ col (string-length name))))
          ("detail" detail)
          ("data" (hash ("detail" detail))))))

;;; Extract parent type name from detail string
;;; "struct point < base-point" → "base-point"
;;; "class dog < animal" → "animal"
(def (extract-parent-from-detail detail)
  (let ((idx (string-contains detail " < ")))
    (if idx
      (let ((after (substring detail (+ idx 3) (string-length detail))))
        (if (> (string-length after) 0) after #f))
      #f)))

;;; Helper: string-contains returning index (or #f)
(def (string-contains str needle)
  (let ((hlen (string-length str))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring str i (+ i nlen))) i)
        (else (loop (+ i 1)))))))
