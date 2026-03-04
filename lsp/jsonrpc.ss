;;; -*- Gerbil -*-
;;; JSON-RPC 2.0 message handling for LSP
(import :std/text/json
        ./util/log)
(export #t)

;;; JSON-RPC 2.0 error codes
(def PARSE-ERROR      -32700)
(def INVALID-REQUEST  -32600)
(def METHOD-NOT-FOUND -32601)
(def INVALID-PARAMS   -32602)
(def INTERNAL-ERROR   -32603)

;; LSP-specific error codes
(def SERVER-NOT-INITIALIZED -32002)
(def REQUEST-CANCELLED      -32800)

;;; Parse a JSON string into a JSON-RPC message.
;;; Returns a hash table with keys: "jsonrpc", "method", "params", "id"
;;; For requests: has "id" and "method"
;;; For notifications: has "method" but no "id"
;;; For responses: has "id" and "result" or "error"
(def (parse-jsonrpc json-string)
  (with-catch
    (lambda (e)
      (lsp-error "JSON parse error: ~a" e)
      #f)
    (lambda ()
      (string->json-object json-string))))

;;; Check if a parsed message is a request (has id and method)
(def (jsonrpc-request? msg)
  (and (hash-table? msg)
       (hash-key? msg "id")
       (hash-key? msg "method")))

;;; Check if a parsed message is a notification (has method, no id)
(def (jsonrpc-notification? msg)
  (and (hash-table? msg)
       (hash-key? msg "method")
       (not (hash-key? msg "id"))))

;;; Check if a parsed message is a response (has id, no method)
(def (jsonrpc-response? msg)
  (and (hash-table? msg)
       (hash-key? msg "id")
       (not (hash-key? msg "method"))))

;;; Get message fields
(def (jsonrpc-method msg)
  (hash-ref msg "method" #f))

(def (jsonrpc-id msg)
  (hash-ref msg "id" #f))

(def (jsonrpc-params msg)
  (hash-ref msg "params" (hash)))

;;; Build a JSON-RPC success response string
(def (make-response id result)
  (json-object->string
    (hash ("jsonrpc" "2.0")
          ("id" id)
          ("result" result))))

;;; Build a JSON-RPC error response string
(def (make-error-response id code message (data (void)))
  (let ((err (hash ("code" code) ("message" message))))
    (unless (void? data)
      (hash-put! err "data" data))
    (json-object->string
      (hash ("jsonrpc" "2.0")
            ("id" id)
            ("error" err)))))

;;; Build a JSON-RPC notification string (server → client)
(def (make-notification method params)
  (json-object->string
    (hash ("jsonrpc" "2.0")
          ("method" method)
          ("params" params))))
