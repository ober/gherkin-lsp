#!chezscheme
;; Build a native gherkin-lsp binary.
;;
;; Usage: cd gherkin-lsp && make binary
;;
;; Produces: ./gherkin-lsp (single ELF binary with embedded boot files + program)

(import (chezscheme))

;; --- Helper: generate C header from binary file ---
(define (file->c-header input-path output-path array-name size-name)
  (let* ((port (open-file-input-port input-path))
         (data (get-bytevector-all port))
         (size (bytevector-length data)))
    (close-port port)
    (call-with-output-file output-path
      (lambda (out)
        (fprintf out "/* Auto-generated — do not edit */~n")
        (fprintf out "static const unsigned char ~a[] = {~n" array-name)
        (let loop ((i 0))
          (when (< i size)
            (when (= 0 (modulo i 16)) (fprintf out "  "))
            (fprintf out "0x~2,'0x" (bytevector-u8-ref data i))
            (when (< (+ i 1) size) (fprintf out ","))
            (when (= 15 (modulo i 16)) (fprintf out "~n"))
            (loop (+ i 1))))
        (fprintf out "~n};~n")
        (fprintf out "static const unsigned int ~a = ~a;~n" size-name size))
      'replace)
    (printf "  ~a: ~a bytes~n" output-path size)))

;; --- Locate Chez install directory ---
(define chez-dir
  (or (getenv "CHEZ_DIR")
      (let* ((mt (symbol->string (machine-type)))
             (home (getenv "HOME"))
             (lib-dir (format "~a/.local/lib" home))
             (csv-dir
               (let lp ((dirs (guard (e (#t '())) (directory-list lib-dir))))
                 (cond
                   ((null? dirs) #f)
                   ((and (> (string-length (car dirs)) 3)
                         (string=? "csv" (substring (car dirs) 0 3)))
                    (format "~a/~a/~a" lib-dir (car dirs) mt))
                   (else (lp (cdr dirs)))))))
        (and csv-dir
             (file-exists? (format "~a/main.o" csv-dir))
             csv-dir))))

(unless chez-dir
  (display "Error: Cannot find Chez install dir. Set CHEZ_DIR.\n")
  (exit 1))

;; --- Locate gherkin runtime ---
(define gherkin-dir
  (or (getenv "GHERKIN_DIR")
      (let ((home (getenv "HOME")))
        (format "~a/mine/gherkin/src" home))))

(unless (file-exists? (format "~a/compat/types.so" gherkin-dir))
  (printf "Error: Cannot find gherkin runtime at ~a~n" gherkin-dir)
  (exit 1))

(printf "Chez dir:    ~a~n" chez-dir)
(printf "Gherkin dir: ~a~n" gherkin-dir)

;; --- Step 1: Compile all modules + entry point ---
(printf "~n[1/6] Compiling all modules...~n")
(parameterize ([compile-imported-libraries #t])
  (compile-program "lsp.ss"))

;; --- Step 2: Bundle program .so ---
(printf "[2/6] Using compiled program...~n")
(system "cp lsp.so lsp-all.so")

;; --- Step 3: Make libs-only boot file ---
(printf "[3/6] Creating libs-only boot file...~n")
(apply make-boot-file "lsp.boot" '("scheme" "petite")
  (append
    ;; Gherkin runtime (dependency order)
    (list
      (format "~a/compat/types.so" gherkin-dir)
      (format "~a/compat/gambit-compat.so" gherkin-dir)
      (format "~a/runtime/util.so" gherkin-dir)
      (format "~a/runtime/table.so" gherkin-dir)
      (format "~a/runtime/c3.so" gherkin-dir)
      (format "~a/runtime/mop.so" gherkin-dir)
      (format "~a/runtime/error.so" gherkin-dir)
      (format "~a/runtime/hash.so" gherkin-dir)
      (format "~a/runtime/syntax.so" gherkin-dir)
      (format "~a/runtime/eval.so" gherkin-dir)
      (format "~a/reader/reader.so" gherkin-dir)
      (format "~a/compiler/compile.so" gherkin-dir)
      (format "~a/boot/gherkin.so" gherkin-dir))
    ;; Compat layer
    (map (lambda (m) (format "src/compat/~a.so" m))
      '("gambit" "misc" "sort" "sugar" "format"
        "json" "getopt" "process"))
    ;; LSP compat
    (list "src/lsp/compat.so")
    ;; LSP modules (dependency order)
    (map (lambda (m) (format "src/lsp/~a.so" m))
      '("util-log" "util-string" "util-position"
        "types" "jsonrpc" "state" "capabilities" "validation"
        "transport" "server"
        "analysis-document" "analysis-parser" "analysis-symbols"
        "analysis-project-config" "analysis-cache" "analysis-module"
        "analysis-index" "analysis-completion-data"
        "handlers-lifecycle" "handlers-sync" "handlers-diagnostics"
        "handlers-completion" "handlers-hover" "handlers-definition"
        "handlers-references" "handlers-symbols" "handlers-rename"
        "handlers-formatting" "handlers-signature" "handlers-code-action"
        "handlers-configuration" "handlers-highlight" "handlers-folding"
        "handlers-selection" "handlers-links" "handlers-semantic-tokens"
        "handlers-inlay-hints" "handlers-call-hierarchy"
        "handlers-type-definition" "handlers-implementation"
        "handlers-type-hierarchy" "handlers-code-lens"
        "handlers-on-type-formatting" "handlers-pull-diagnostics"
        "handlers-execute-command" "handlers-will-rename"
        "handlers-apply-edit"
        "lib" "main"))))

;; --- Step 4: Generate C headers with embedded data ---
(printf "[4/6] Embedding boot files + program as C headers...~n")
(file->c-header "lsp-all.so" "lsp_program.h"
                "lsp_program_data" "lsp_program_size")
(file->c-header (format "~a/petite.boot" chez-dir) "lsp_petite_boot.h"
                "petite_boot_data" "petite_boot_size")
(file->c-header (format "~a/scheme.boot" chez-dir) "lsp_scheme_boot.h"
                "scheme_boot_data" "scheme_boot_size")
(file->c-header "lsp.boot" "lsp_lsp_boot.h"
                "lsp_boot_data" "lsp_boot_size")

;; --- Step 5: Compile and link ---
(printf "[5/6] Compiling and linking...~n")
(let ((cmd (format "gcc -c -O2 -o lsp-main.o lsp-main.c -I~a -I. -Wall 2>&1" chez-dir)))
  (unless (= 0 (system cmd))
    (display "Error: lsp-main.c compilation failed\n")
    (exit 1)))
(let ((cmd (format "gcc -rdynamic -o gherkin-lsp lsp-main.o -L~a -lkernel -llz4 -lz -lm -ldl -lpthread -luuid -lncurses -Wl,-rpath,~a"
             chez-dir chez-dir)))
  (printf "  ~a~n" cmd)
  (unless (= 0 (system cmd))
    (display "Error: Link failed\n")
    (exit 1)))

;; --- Step 6: Clean up ---
(printf "[6/6] Cleaning up...~n")
(for-each (lambda (f)
            (when (file-exists? f) (delete-file f)))
  '("lsp-main.o" "lsp_program.h"
    "lsp_petite_boot.h" "lsp_scheme_boot.h" "lsp_lsp_boot.h"
    "lsp-all.so" "lsp.so" "lsp.wpo" "lsp.boot"))

(printf "~n========================================~n")
(printf "Build complete!~n~n")
(printf "  Binary: ./gherkin-lsp  (~a KB)~n"
  (quotient (file-length (open-file-input-port "gherkin-lsp")) 1024))
(printf "~nRun:~n")
(printf "  ./gherkin-lsp --stdio  # start LSP server~n")
(printf "  cp gherkin-lsp /tmp/ && /tmp/gherkin-lsp --stdio~n")
