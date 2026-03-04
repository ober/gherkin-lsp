;;; -*- Gerbil -*-
;;; Compatibility shim for Gerbil v0.19
;;; Provides the same API surface as compat-v18 using v0.19 paths
;;; or local reimplementations for removed modules.

;; :std/cli/getopt still exists in v0.19
(import :std/cli/getopt)
(export format fprintf sort
        read-file-string read-all-as-string
        string-trim-eol
        call-with-getopt flag option)

;;; format — reimplementation for ~a and ~s directives
;;; :std/format is removed in v0.19; this covers the LSP's usage
(def (format fmt . args)
  (let ((out (open-output-string))
        (len (string-length fmt)))
    (let loop ((i 0) (args args))
      (cond
       ((>= i len)
        (get-output-string out))
       ((and (char=? (string-ref fmt i) #\~)
             (< (+ i 1) len))
        (let ((directive (string-ref fmt (+ i 1))))
          (case directive
            ((#\a)
             (display (car args) out)
             (loop (+ i 2) (cdr args)))
            ((#\s)
             (write (car args) out)
             (loop (+ i 2) (cdr args)))
            ((#\~)
             (write-char #\~ out)
             (loop (+ i 2) args))
            ((#\%)
             (newline out)
             (loop (+ i 2) args))
            (else
             (write-char #\~ out)
             (write-char directive out)
             (loop (+ i 2) args)))))
       (else
        (write-char (string-ref fmt i) out)
        (loop (+ i 1) args))))))

;;; fprintf — write formatted output to a port
(def (fprintf port fmt . args)
  (display (apply format fmt args) port))

;;; sort — v0.19 renames sort to list-sort with swapped arg order
(def (sort lst pred)
  (list-sort pred lst))

;;; read-file-string — :std/misc/ports is removed in v0.19
(def (read-file-string path . _rest)
  (call-with-input-file path
    (lambda (port)
      (read-line port #f))))

;;; read-all-as-string — reads all available output from a process port
(def (read-all-as-string port)
  (read-line port #f))

;;; string-trim-eol — :std/misc/string is removed in v0.19
(def (string-trim-eol str)
  (let loop ((end (string-length str)))
    (if (and (> end 0)
             (let ((ch (string-ref str (- end 1))))
               (or (char=? ch #\newline)
                   (char=? ch #\return))))
      (loop (- end 1))
      (if (= end (string-length str))
        str
        (substring str 0 end)))))
