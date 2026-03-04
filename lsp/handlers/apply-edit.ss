;;; -*- Gerbil -*-
;;; workspace/applyEdit support — push multi-file edits to the client
;;; This is a utility module (not a handler — the server initiates this).
(import :std/sugar
        ../util/log
        ../server)
(export #t)

;;; Apply a workspace edit via the client.
;;; edit: a WorkspaceEdit hash ({"changes": {...}} or {"documentChanges": [...]})
;;; label: optional human-readable description of the edit
;;; Returns #t if the client applied the edit, #f otherwise.
(def (apply-workspace-edit! edit label: (label #f))
  (let ((params (hash ("edit" edit))))
    (when label
      (hash-put! params "label" label))
    (let ((response (send-request-sync! "workspace/applyEdit" params
                      timeout: 10)))
      (if (and response (hash-table? response))
        (let ((applied (hash-ref response "applied" #f)))
          (if applied
            (begin (lsp-debug "workspace/applyEdit applied") #t)
            (begin
              (lsp-warn "workspace/applyEdit rejected: ~a"
                (hash-ref response "failureReason" "unknown"))
              #f)))
        (begin
          (lsp-warn "workspace/applyEdit: no response from client")
          #f)))))
