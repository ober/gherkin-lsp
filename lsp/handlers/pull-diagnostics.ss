;;; -*- Gerbil -*-
;;; Pull diagnostics handler — textDocument/diagnostic
;;; Returns DocumentDiagnosticReport
(import ../util/log
        ../types
        ../state
        ../analysis/document
        ./diagnostics)
(export #t)

;;; Handle textDocument/diagnostic (pull model)
;;; Returns a DocumentDiagnosticReport with kind "full"
(def (handle-document-diagnostic params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (let ((doc (get-document uri)))
      (if doc
        (let ((diags (collect-diagnostics uri doc)))
          (hash ("kind" "full")
                ("items" (list->vector diags))))
        (hash ("kind" "full")
              ("items" (vector)))))))
