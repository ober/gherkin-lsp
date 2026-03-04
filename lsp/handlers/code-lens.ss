;;; -*- Gerbil -*-
;;; Code lens handler — textDocument/codeLens
;;; Shows reference counts and test runners
(import ../compat/compat
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/codeLens
;;; Returns CodeLens[] with reference counts and test runners
(def (handle-code-lens params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let ((syms (get-file-symbols uri))
            (result '()))
        (for-each
          (lambda (s)
            (let ((kind (sym-info-kind s))
                  (name (sym-info-name s))
                  (line (sym-info-line s)))
              ;; Only show lenses for top-level definitions
              (when (or (= kind SymbolKind.Function)
                        (= kind SymbolKind.Variable)
                        (= kind SymbolKind.Struct)
                        (= kind SymbolKind.Class)
                        (= kind SymbolKind.Constant)
                        (= kind SymbolKind.Method))
                ;; Reference count lens
                (let ((refs (find-references-by-name name)))
                  (let ((ref-count (length refs)))
                    (set! result
                      (cons (make-code-lens
                              line 0
                              (format "~a reference~a" ref-count
                                      (if (= ref-count 1) "" "s"))
                              "gerbil-lsp.showReferences"
                              [uri line 0])
                            result)))))
              ;; Test suite lens
              (when (and (= kind SymbolKind.Variable)
                         (let ((detail (or (sym-info-detail s) "")))
                           (or (string-suffix? "-test" name)
                               (string-contains-detail detail "test-suite"))))
                (set! result
                  (cons (make-code-lens
                          line 0
                          "Run test"
                          "gerbil-lsp.runTest"
                          [name])
                        result)))))
          ;; Filter to only top-level definition kinds (skip params/locals)
          (filter top-level-definition? syms))
        (list->vector result))
      (vector))))

;;; Create a CodeLens object
(def (make-code-lens line col title command args)
  (hash ("range" (make-lsp-range line col line col))
        ("command"
         (hash ("title" title)
               ("command" command)
               ("arguments" (list->vector args))))))

;;; Check if a sym-info is a top-level definition (not a parameter or local)
(def (top-level-definition? s)
  (let ((detail (or (sym-info-detail s) "")))
    (not (or (string=? detail "parameter")
             (string=? detail "local")))))

;;; Check if detail string contains a substring
(def (string-contains-detail haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring haystack i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

;;; Check if a string ends with a suffix
(def (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (slen2 (string-length str)))
    (and (>= slen2 slen)
         (string=? suffix (substring str (- slen2 slen) slen2)))))
