;;; -*- Gerbil -*-
;;; LSP server main dispatch loop
(import ./compat/compat
        :std/text/json
        ./util/log
        ./transport
        ./jsonrpc
        ./state
        ./validation)
(export #t)

;;; Method handler registry: method-string → handler-fn
;;; Request handlers: (params) → result (hash table or void)
;;; Notification handlers: (params) → void
(def *request-handlers* (make-hash-table))
(def *notification-handlers* (make-hash-table))


;;; Output port for sending messages (stdout)
(def *output-port* #f)

;;; Mutex protecting write-message for concurrent access (main + diagnostics thread)
(def *output-mutex* (make-mutex 'output))

;;; Register a request handler (expects a response)
(def (register-request-handler! method handler)
  (hash-put! *request-handlers* method handler))

;;; Register a notification handler (no response expected)
(def (register-notification-handler! method handler)
  (hash-put! *notification-handlers* method handler))

;;; Send a notification from server to client (thread-safe)
(def (send-notification! method params)
  (when *output-port*
    (let ((msg (make-notification method params)))
      (mutex-lock! *output-mutex*)
      (with-catch
        (lambda (e)
          (mutex-unlock! *output-mutex*)
          (raise e))
        (lambda ()
          (write-message *output-port* msg)
          (mutex-unlock! *output-mutex*))))))

;;; LSP MessageType constants
(def MessageType.Error   1)
(def MessageType.Warning 2)
(def MessageType.Info    3)
(def MessageType.Log     4)

;;; Send a window/logMessage notification to the client
(def (send-log-message! type message)
  (send-notification! "window/logMessage"
    (hash ("type" type) ("message" message))))

;;; Send a $/progress notification
;;; kind: "begin", "report", or "end"
(def (send-progress! token kind
                     title: (title (void))
                     message: (message (void))
                     percentage: (percentage (void)))
  (let ((value (hash ("kind" kind))))
    (unless (void? title) (hash-put! value "title" title))
    (unless (void? message) (hash-put! value "message" message))
    (unless (void? percentage) (hash-put! value "percentage" percentage))
    (send-notification! "$/progress"
      (hash ("token" token) ("value" value)))))

;;; Pending request tracking: id → callback or condvar+result-cell
(def *pending-requests* (make-hash-table))
(def *pending-requests-mutex* (make-mutex 'pending-requests))

;;; Send a request from server to client (thread-safe)
;;; Optional callback: called with (result-or-error) when response arrives.
;;; Without callback, fire-and-forget.
(def *next-server-request-id* 0)
(def (send-request! method params callback: (callback #f))
  (when *output-port*
    (let* ((id (begin (set! *next-server-request-id*
                        (+ *next-server-request-id* 1))
                      *next-server-request-id*))
           (msg (json-object->string
                  (hash ("jsonrpc" "2.0")
                        ("id" id)
                        ("method" method)
                        ("params" params)))))
      ;; Register callback if provided
      (when callback
        (mutex-lock! *pending-requests-mutex*)
        (hash-put! *pending-requests* id callback)
        (mutex-unlock! *pending-requests-mutex*))
      (mutex-lock! *output-mutex*)
      (with-catch
        (lambda (e)
          (mutex-unlock! *output-mutex*)
          (raise e))
        (lambda ()
          (write-message *output-port* msg)
          (mutex-unlock! *output-mutex*)))
      id)))

;;; Send a request and wait synchronously for the response.
;;; Returns the result hash, or #f on timeout/error.
;;; Timeout in seconds (default: 5).
(def (send-request-sync! method params timeout: (timeout 5))
  (let ((mx (make-mutex 'sync-request))
        (cv (make-condition-variable 'sync-response))
        (result-cell (box #f)))
    (mutex-lock! mx)
    (send-request! method params
      callback: (lambda (response)
                  (set-box! result-cell response)
                  (mutex-lock! mx)
                  (condition-variable-signal! cv)
                  (mutex-unlock! mx)))
    ;; Wait for response with timeout
    (let ((deadline (+ (time->seconds (current-time)) timeout)))
      (mutex-unlock! mx cv (seconds->time deadline)))
    (unbox result-cell)))

;;; Dispatch a response to its pending callback
(def (dispatch-response! msg)
  (let ((id (jsonrpc-id msg)))
    (mutex-lock! *pending-requests-mutex*)
    (let ((callback (hash-ref *pending-requests* id #f)))
      (when callback
        (hash-remove! *pending-requests* id))
      (mutex-unlock! *pending-requests-mutex*)
      (when callback
        (let ((result (or (hash-ref msg "result" #f)
                          (hash-ref msg "error" #f))))
          (with-catch
            (lambda (e) (lsp-error "response callback error: ~a" e))
            (lambda () (callback result))))))))

;;; Main server loop — reads from stdin, dispatches, writes to stdout
;;; Resilient: only exits on EOF, recovers from malformed messages.
(def (start-server (input-port (current-input-port))
                   (output-port (current-output-port)))
  (set! *output-port* output-port)
  (lsp-info "gerbil-lsp server starting")
  (let loop ()
    (let ((result (read-message input-port)))
      (cond
        ((eq? result 'eof)
         (lsp-info "input stream closed, shutting down"))
        ((eq? result 'error)
         (lsp-warn "malformed message (no Content-Length), skipping")
         (loop))
        ((string? result)
         (handle-message result output-port)
         (loop))
        (else
         (lsp-error "unexpected read-message result: ~a" result)
         (loop))))))

;;; Handle a single JSON-RPC message
(def (handle-message json-str output-port)
  (let ((msg (parse-jsonrpc json-str)))
    (cond
      ((not msg)
       (lsp-error "failed to parse message")
       (write-message output-port
         (make-error-response (void) PARSE-ERROR "Parse error")))

      ;; Request — has id and method, needs response
      ((jsonrpc-request? msg)
       (handle-request msg output-port))

      ;; Notification — has method, no id, no response
      ((jsonrpc-notification? msg)
       (handle-notification msg))

      ;; Response — has id, no method — from client answering our request
      ((jsonrpc-response? msg)
       (dispatch-response! msg))

      (else
       (lsp-warn "unexpected message type: ~a" json-str)))))

;;; Handle a request — dispatch and send response
;;; Guards: reject if not initialized (except "initialize") or if shutdown requested
(def (handle-request msg output-port)
  (let ((id (jsonrpc-id msg))
        (method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "request: ~a (id=~a)" method id)
    (cond
      ;; After shutdown, reject all requests
      ((shutdown-requested?)
       (lsp-warn "request after shutdown: ~a" method)
       (write-message output-port
         (make-error-response id INVALID-REQUEST "Server is shutting down")))
      ;; Before initialization, only "initialize" is allowed
      ((and (not (server-initialized?))
            (not (string=? method "initialize")))
       (lsp-warn "request before initialize: ~a" method)
       (write-message output-port
         (make-error-response id SERVER-NOT-INITIALIZED "Server not initialized")))
      ;; Normal dispatch
      (else
       (let ((handler (hash-ref *request-handlers* method #f)))
         (if handler
           (with-catch
             (lambda (e)
               (lsp-error "handler error for ~a: ~a" method e)
               (write-message output-port
                 (make-error-response id INTERNAL-ERROR
                   (format "Internal error: ~a" e))))
             (lambda ()
               (let ((result (handler params)))
                 (validate-and-log! method result)
                 (write-message output-port
                   (make-response id result)))))
           (begin
             (lsp-warn "no handler for request: ~a" method)
             (write-message output-port
               (make-error-response id METHOD-NOT-FOUND
                 (string-append "Method not found: " method))))))))))

;;; Handle a notification — dispatch, no response
;;; After shutdown, only "exit" is processed; all others are ignored
(def (handle-notification msg)
  (let ((method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "notification: ~a" method)
    (if (and (shutdown-requested?) (not (string=? method "exit")))
      (lsp-debug "ignoring notification after shutdown: ~a" method)
      (let ((handler (hash-ref *notification-handlers* method #f)))
        (if handler
          (with-catch
            (lambda (e)
              (lsp-error "handler error for ~a: ~a" method e))
            (lambda ()
              (handler params)))
          (lsp-debug "no handler for notification: ~a" method))))))
