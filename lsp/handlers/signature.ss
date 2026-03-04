;;; -*- Gerbil -*-
;;; Signature help handler — show function signatures
(import ../compat/compat
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/signatureHelp
(def (handle-signature-help params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let ((call-info (find-enclosing-call (document-text doc) line col)))
        (if call-info
          (let* ((func-name (car call-info))
                 (arg-index (cdr call-info))
                 (sig (find-signature func-name uri)))
            (if sig
              (make-signature-help
                (vector sig)
                0
                (max 0 arg-index))
              (void)))
          (void)))
      (void))))

;;; Find the enclosing function call at a position
;;; Returns (func-name . arg-index) or #f
(def (find-enclosing-call text line col)
  (let ((offset (line-col->offset* text line col)))
    (if (not offset)
      #f
      ;; Walk backwards to find the opening paren of the enclosing call
      (let loop ((i (- offset 1)) (depth 0) (arg-count 0))
        (cond
          ((< i 0) #f)
          ((char=? (string-ref text i) #\))
           (loop (- i 1) (+ depth 1) arg-count))
          ((char=? (string-ref text i) #\()
           (if (= depth 0)
             ;; Found the opening paren — now extract the function name
             (let ((func-name (extract-func-name text (+ i 1))))
               (if func-name
                 (cons func-name arg-count)
                 #f))
             (loop (- i 1) (- depth 1) arg-count)))
          ;; Count spaces at depth 0 as argument separators
          ((and (= depth 0) (char-whitespace? (string-ref text i)))
           ;; Check if previous char was non-whitespace (argument boundary)
           (if (and (> i 0)
                    (not (char-whitespace? (string-ref text (- i 1))))
                    (not (char=? (string-ref text (- i 1)) #\()))
             (loop (- i 1) depth (+ arg-count 1))
             (loop (- i 1) depth arg-count)))
          (else
           (loop (- i 1) depth arg-count)))))))

;;; Extract function name starting at position (after opening paren)
(def (extract-func-name text start)
  (let ((len (string-length text)))
    (let loop ((i start) (chars '()))
      (cond
        ((>= i len) #f)
        ((or (char-whitespace? (string-ref text i))
             (char=? (string-ref text i) #\))
             (char=? (string-ref text i) #\())
         (if (null? chars) #f
           (list->string (reverse chars))))
        (else
         (loop (+ i 1) (cons (string-ref text i) chars)))))))

;;; Convert line/col to offset (simple implementation)
(def (line-col->offset* text line col)
  (let loop ((i 0) (cur-line 0) (cur-col 0))
    (cond
      ((and (= cur-line line) (= cur-col col)) i)
      ((>= i (string-length text)) #f)
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ cur-line 1) 0))
      (else
       (loop (+ i 1) cur-line (+ cur-col 1))))))

;;; Find a function signature for display
(def (find-signature func-name uri)
  ;; Look up in local symbols first
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-func-sym func-name local-syms)))
      (if found
        (make-sig-from-sym found)
        ;; Search workspace
        (let ((defs (find-definitions-by-name func-name)))
          (if (pair? defs)
            (make-sig-from-sym (cdr (car defs)))
            #f))))))

;;; Find a function symbol by name
(def (find-func-sym name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (let ((s (car ss)))
        (if (and (string=? name (sym-info-name s))
                 (= (sym-info-kind s) SymbolKind.Function))
          s
          (loop (cdr ss)))))))

;;; Create a SignatureInformation from a sym-info
;;; Extracts individual parameter labels for active parameter highlighting
(def (make-sig-from-sym s)
  (let ((detail (sym-info-detail s)))
    (if detail
      (let ((params (extract-param-labels detail)))
        (make-signature-information detail
          documentation: (format "Defined as ~a" (sym-info-name s))
          parameters: params))
      (make-signature-information
        (format "(~a ...)" (sym-info-name s))
        documentation: (format "~a" (sym-info-name s))))))

;;; Extract parameter labels from a signature string like "(name arg1 arg2 . rest)"
;;; Returns a list of ParameterInformation objects
(def (extract-param-labels sig-str)
  (with-catch
    (lambda (e) '())
    (lambda ()
      (let ((form (read (open-input-string sig-str))))
        (if (and (pair? form) (pair? (cdr form)))
          (let ((args (cdr form)))
            (extract-params-from-arglist args))
          '())))))

;;; Convert an argument list to ParameterInformation objects
;;; Handles: (a b c), (a b . rest), ((a default) b), keyword: args
(def (extract-params-from-arglist args)
  (cond
    ((null? args) '())
    ((symbol? args)
     ;; Rest argument: . rest
     (list (make-parameter-information (format ". ~a" args))))
    ((pair? args)
     (let ((param (car args)))
       (cons
         (cond
           ;; Optional with default: (param default)
           ((pair? param)
            (make-parameter-information (format "~a" param)))
           ;; Keyword argument: name:
           ((and (symbol? param)
                 (let ((s (symbol->string param)))
                   (and (> (string-length s) 0)
                        (char=? (string-ref s (- (string-length s) 1)) #\:))))
            (make-parameter-information (symbol->string param)))
           ;; Normal parameter
           ((symbol? param)
            (make-parameter-information (symbol->string param)))
           ;; Anything else
           (else (make-parameter-information (format "~a" param))))
         (extract-params-from-arglist (cdr args)))))
    (else '())))

;;; Check if char is whitespace
(def (char-whitespace? c)
  (or (char=? c #\space) (char=? c #\tab)
      (char=? c #\newline) (char=? c #\return)))
