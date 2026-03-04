#!chezscheme
;;; build-gherkin.ss — Compile gerbil-lsp .ss modules to .sls via Gherkin compiler
;;; Usage: scheme -q --libdirs src:<gherkin-path> --compile-imported-libraries < build-gherkin.ss

(import
  (except (chezscheme) void box box? unbox set-box!
          andmap ormap iota last-pair find
          1+ 1- fx/ fx1+ fx1-
          error error? raise with-exception-handler identifier?
          hash-table? make-hash-table)
  (compiler compile))

;; --- Configuration ---
(define output-dir "src/lsp")

;; --- Import map: Gerbil module → Chez library ---
;; *default-package* sets the package for unmapped relative imports
(define lsp-import-map
  '(;; Default package for unmapped relative imports
    (*default-package* . lsp)

    ;; Standard library mappings
    (:std/sugar        . (compat sugar))
    (:std/format       . (compat format))
    (:std/sort         . (compat sort))
    (:std/pregexp      . (compat pregexp))
    (:std/misc/string  . (compat misc))
    (:std/misc/list    . (compat misc))
    (:std/misc/path    . (compat misc))
    (:std/misc/hash    . (compat misc))
    (:std/misc/ports   . (lsp compat))
    (:std/misc/process . (compat process))
    (:std/iter         . #f)  ;; stripped — Gherkin compiles for-loops natively
    (:std/error        . (runtime error))
    (:std/text/json    . (compat json))
    (:std/cli/getopt   . (compat getopt))
    (:std/os/signal    . (compat signal))
    (:std/os/signal-handler . (compat signal-handler))
    (:std/os/fdio      . (compat fdio))
    (:std/srfi/1       . (compat misc))
    (:std/foreign      . #f)  ;; stripped — no Gambit FFI on Chez
    (:std/build-script . #f)  ;; stripped
    (:std/test         . #f)  ;; stripped
    ;; Gerbil runtime
    (:gerbil/core      . #f)  ;; stripped
    (:gerbil/runtime   . #f)  ;; stripped
    (:gerbil/runtime/init . #f)
    (:gerbil/runtime/loader . #f)
    (:gerbil/expander   . #f)
    (:gerbil/compiler   . #f)
    ;; Internal compat modules — map to handwritten .sls
    ("./compat/compat"    . (lsp compat))
    ("../compat/compat"   . (lsp compat))
    ("./compat/version"   . #f)  ;; stripped — not needed
    ("../compat/version"  . #f)
    ))

;; --- Base imports for all compiled modules ---
(define lsp-base-imports
  '((except (chezscheme) box box? unbox set-box!
            andmap ormap iota last-pair find
            1+ 1- fx/ fx1+ fx1-
            error error? raise with-exception-handler identifier?
            hash-table? make-hash-table
            sort sort! path-extension
            printf fprintf
            ;; Exclude Chez builtins that (compat gambit) replaces
            file-directory? file-exists? getenv close-port
            ;; Chez void takes 0 args; Gerbil's is variadic
            void
            ;; Gambit-compatible: handles /dev/fd/N and keyword args
            open-output-file open-input-file)
    (compat types)
    (except (runtime util)
            ;; These conflict with (compat gambit) exports
            string->bytes bytes->string
            ;; These conflict with (compat misc) exports
            string-split string-join find string-index
            ;; Unused internal helpers
            pgetq pgetv pget)
    (except (runtime table) string-hash)
    (runtime mop)
    (except (runtime error) with-catch with-exception-catcher)
    (runtime hash)
    ;; Gambit compat (u8vector, threading, file ops, etc.)
    (except (compat gambit) number->string make-mutex
            with-output-to-string)
    ;; Misc utilities (string-prefix?, path-expand, etc.)
    (compat misc)
    ;; LSP-specific compat (json, getopt, process, exceptions, etc.)
    (lsp compat)))

;; --- Import conflict resolution (from gherkin-shell) ---
(define (fix-import-conflicts lib-form)
  (let* ((lib-name (cadr lib-form))
         (export-clause (caddr lib-form))
         (import-clause (cadddr lib-form))
         (body (cddddr lib-form))
         (imports (cdr import-clause))
         (local-defs
           (let lp ((forms body) (names '()))
             (if (null? forms)
               names
               (lp (cdr forms)
                   (append (extract-def-names (car forms)) names)))))
         (all-earlier-names
           (let lp ((imps imports) (seen '()) (result '()))
             (if (null? imps)
               (reverse result)
               (let* ((imp (car imps))
                      (lib (get-import-lib-name imp))
                      (exports (if lib
                                 (or (begin (ensure-library-loaded lib)
                                       (guard (e (#t #f))
                                         (library-exports lib)))
                                     (read-sls-exports lib)
                                     '())
                                 '()))
                      (provided (cond
                                  ((and (pair? imp) (eq? (car imp) 'except))
                                   (filter (lambda (s) (not (memq s (cddr imp))))
                                           exports))
                                  ((and (pair? imp) (eq? (car imp) 'only))
                                   (cddr imp))
                                  (else exports))))
                 (lp (cdr imps)
                     (append provided seen)
                     (cons seen result)))))))
    (let ((fixed-imports
            (map (lambda (imp earlier-names)
                   (fix-one-import imp
                     (append local-defs earlier-names)))
                 imports all-earlier-names)))
      (let ((fixed-body (fix-assigned-exports
                          (cdr export-clause)
                          (list (cons 'import fixed-imports))
                          body)))
        `(library ,lib-name ,export-clause
          (import ,@fixed-imports) ,@fixed-body)))))

(define (fix-assigned-exports exports import-forms body)
  (let ((assigned-names
          (let lp ((tree body) (names '()))
            (cond
              ((not (pair? tree)) names)
              ((and (eq? (car tree) 'set!)
                    (pair? (cdr tree))
                    (symbol? (cadr tree))
                    (memq (cadr tree) exports)
                    (not (memq (cadr tree) names)))
               (cons (cadr tree) names))
              (else
               (lp (cdr tree) (lp (car tree) names)))))))
    (if (null? assigned-names)
      body
      (let ((new-body
              (let lp ((forms body) (result '()))
                (if (null? forms)
                  (reverse result)
                  (let ((form (car forms)))
                    (cond
                      ((and (pair? form)
                            (eq? (car form) 'define)
                            (let ((def-name (if (pair? (cadr form)) (caadr form) (cadr form))))
                              (and (symbol? def-name) (memq def-name assigned-names))))
                       (let* ((def-name (if (pair? (cadr form)) (caadr form) (cadr form)))
                              (init (if (pair? (cadr form))
                                      `(lambda ,(cdadr form) ,@(cddr form))
                                      (if (pair? (cddr form)) (caddr form) '(void))))
                              (cell-name (string->symbol
                                           (string-append (symbol->string def-name) "-cell"))))
                         (lp (cdr forms)
                             (append
                               (list
                                 `(define-syntax ,def-name
                                    (identifier-syntax
                                      (id (vector-ref ,cell-name 0))
                                      ((set! id v) (vector-set! ,cell-name 0 v))))
                                 `(define ,cell-name (vector ,init)))
                               result))))
                      (else
                       (lp (cdr forms) (cons form result)))))))))
        new-body))))

(define (extract-def-names form)
  (cond
    ((not (pair? form)) '())
    ((eq? (car form) 'define)
     (cond
       ((symbol? (cadr form)) (list (cadr form)))
       ((pair? (cadr form)) (list (caadr form)))
       (else '())))
    ((eq? (car form) 'define-syntax)
     (if (symbol? (cadr form)) (list (cadr form)) '()))
    ((eq? (car form) 'begin)
     (let lp ((forms (cdr form)) (names '()))
       (if (null? forms) names
           (lp (cdr forms) (append (extract-def-names (car forms)) names)))))
    (else '())))

(define (ensure-library-loaded lib-name)
  (guard (e (#t #f))
    (eval `(import ,lib-name) (interaction-environment))
    #t))

(define (read-sls-exports lib-name)
  (let ((path (lib-name->sls-path lib-name)))
    (if (and path (file-exists? path))
      (guard (e (#t #f))
        (call-with-input-file path
          (lambda (port)
            (let ((first (read port)))
              (let ((lib-form (if (and (pair? first) (eq? (car first) 'library))
                                first
                                (read port))))
                (if (and (pair? lib-form) (eq? (car lib-form) 'library))
                  (let ((export-clause (caddr lib-form)))
                    (if (and (pair? export-clause) (eq? (car export-clause) 'export))
                      (cdr export-clause)
                      #f))
                  #f))))))
      #f)))

(define (lib-name->sls-path lib-name)
  (cond
    ((and (pair? lib-name) (= (length lib-name) 2)
          (eq? (car lib-name) 'lsp))
     (string-append output-dir "/" (symbol->string (cadr lib-name)) ".sls"))
    ((and (pair? lib-name) (= (length lib-name) 2)
          (eq? (car lib-name) 'compat))
     (string-append "src/compat/" (symbol->string (cadr lib-name)) ".sls"))
    (else #f)))

(define (fix-one-import imp local-defs)
  (let ((lib-name (get-import-lib-name imp)))
    (if (not lib-name)
      imp
      (let* ((_load (ensure-library-loaded lib-name))
             (lib-exports (or (guard (e (#t #f)) (library-exports lib-name))
                              (read-sls-exports lib-name)
                              '()))
             (conflicts (filter (lambda (d) (memq d lib-exports))
                                local-defs)))
            (if (null? conflicts)
              imp
              (cond
                ((and (pair? imp) (eq? (car imp) 'except))
                 (let ((existing (cddr imp)))
                   `(except ,(cadr imp)
                      ,@existing
                      ,@(filter (lambda (d) (not (memq d existing)))
                                conflicts))))
                ((and (pair? imp) (eq? (car imp) 'only))
                 (let ((kept (filter (lambda (s) (not (memq s conflicts)))
                                     (cddr imp))))
                   `(only ,(cadr imp) ,@kept)))
                ((pair? imp)
                 `(except ,imp ,@conflicts))
                (else imp)))))))

(define (get-import-lib-name spec)
  (cond
    ((and (pair? spec)
          (memq (car spec) '(except only rename prefix)))
     (get-import-lib-name (cadr spec)))
    ((and (pair? spec) (symbol? (car spec)))
     spec)
    (else #f)))

;; --- Module compilation ---
;; Compile a module from source-dir/name.ss to output-dir/flat-name.sls
(define (compile-module source-path flat-name)
  (let* ((output-path (string-append output-dir "/" flat-name ".sls"))
         (lib-name `(lsp ,(string->symbol flat-name))))
    (display (string-append "  Compiling: " source-path " → " flat-name ".sls\n"))
    (guard (exn
             (#t (display (string-append "  ERROR: " source-path " failed: "))
                 (display (condition-message exn))
                 (when (irritants-condition? exn)
                   (display " — ")
                   (display (condition-irritants exn)))
                 (newline)
                 #f))
      (let* ((lib-form (gerbil-compile-to-library
                         source-path lib-name
                         lsp-import-map lsp-base-imports))
             (lib-form (fix-import-conflicts lib-form)))
        (call-with-output-file output-path
          (lambda (port)
            (display "#!chezscheme\n" port)
            (parameterize ([print-gensym #f])
              (pretty-print lib-form port)))
          'replace)
        (display (string-append "  OK: " output-path "\n"))
        #t))))

;; --- Main ---
(display "=== Gherkin LSP Builder ===\n\n")

;; Tier 1: Utilities (no dependencies on other lsp modules)
(display "--- Tier 1: Utilities ---\n")
(compile-module "lsp/util/log.ss" "util-log")
(compile-module "lsp/util/string.ss" "util-string")
(compile-module "lsp/util/position.ss" "util-position")

;; Tier 2: Core protocol (depends on utils)
(display "\n--- Tier 2: Core Protocol ---\n")
(compile-module "lsp/types.ss" "types")
(compile-module "lsp/jsonrpc.ss" "jsonrpc")
(compile-module "lsp/state.ss" "state")
(compile-module "lsp/capabilities.ss" "capabilities")
(compile-module "lsp/validation.ss" "validation")
(compile-module "lsp/transport.ss" "transport")
(compile-module "lsp/server.ss" "server")

;; Tier 3: Analysis modules
(display "\n--- Tier 3: Analysis ---\n")
(compile-module "lsp/analysis/document.ss" "analysis-document")
(compile-module "lsp/analysis/parser.ss" "analysis-parser")
(compile-module "lsp/analysis/symbols.ss" "analysis-symbols")
(compile-module "lsp/analysis/project-config.ss" "analysis-project-config")
(compile-module "lsp/analysis/cache.ss" "analysis-cache")
(compile-module "lsp/analysis/module.ss" "analysis-module")
(compile-module "lsp/analysis/index.ss" "analysis-index")
(compile-module "lsp/analysis/completion-data.ss" "analysis-completion-data")

;; Tier 4: Handlers (depend on core + analysis)
(display "\n--- Tier 4: Handlers ---\n")
(compile-module "lsp/handlers/lifecycle.ss" "handlers-lifecycle")
(compile-module "lsp/handlers/sync.ss" "handlers-sync")
(compile-module "lsp/handlers/diagnostics.ss" "handlers-diagnostics")
(compile-module "lsp/handlers/completion.ss" "handlers-completion")
(compile-module "lsp/handlers/hover.ss" "handlers-hover")
(compile-module "lsp/handlers/definition.ss" "handlers-definition")
(compile-module "lsp/handlers/references.ss" "handlers-references")
(compile-module "lsp/handlers/symbols.ss" "handlers-symbols")
(compile-module "lsp/handlers/rename.ss" "handlers-rename")
(compile-module "lsp/handlers/formatting.ss" "handlers-formatting")
(compile-module "lsp/handlers/signature.ss" "handlers-signature")
(compile-module "lsp/handlers/code-action.ss" "handlers-code-action")
(compile-module "lsp/handlers/configuration.ss" "handlers-configuration")
(compile-module "lsp/handlers/highlight.ss" "handlers-highlight")
(compile-module "lsp/handlers/folding.ss" "handlers-folding")
(compile-module "lsp/handlers/selection.ss" "handlers-selection")
(compile-module "lsp/handlers/links.ss" "handlers-links")
(compile-module "lsp/handlers/semantic-tokens.ss" "handlers-semantic-tokens")
(compile-module "lsp/handlers/inlay-hints.ss" "handlers-inlay-hints")
(compile-module "lsp/handlers/call-hierarchy.ss" "handlers-call-hierarchy")
(compile-module "lsp/handlers/type-definition.ss" "handlers-type-definition")
(compile-module "lsp/handlers/implementation.ss" "handlers-implementation")
(compile-module "lsp/handlers/type-hierarchy.ss" "handlers-type-hierarchy")
(compile-module "lsp/handlers/code-lens.ss" "handlers-code-lens")
(compile-module "lsp/handlers/on-type-formatting.ss" "handlers-on-type-formatting")
(compile-module "lsp/handlers/pull-diagnostics.ss" "handlers-pull-diagnostics")
(compile-module "lsp/handlers/execute-command.ss" "handlers-execute-command")
(compile-module "lsp/handlers/will-rename.ss" "handlers-will-rename")
(compile-module "lsp/handlers/apply-edit.ss" "handlers-apply-edit")

;; Tier 5: Entry points
(display "\n--- Tier 5: Entry points ---\n")
(compile-module "lsp/lib.ss" "lib")
(compile-module "lsp/main.ss" "main")

(display "\n=== Build complete ===\n")
