;;; eglot-gerbil.el --- Gerbil Scheme support for Eglot -*- lexical-binding: t -*-

;; Author: gerbil-lsp contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eglot "1.12"))
;; Keywords: languages, gerbil, scheme, lsp
;; URL: https://github.com/ober/gerbil-lsp

;;; Commentary:

;; This package extends Eglot with features that the gerbil-lsp language
;; server provides but Eglot does not natively support:
;;
;;   - Code lenses (reference counts, test runners above definitions)
;;   - Call hierarchy (incoming/outgoing calls navigation)
;;   - Type hierarchy (supertypes/subtypes for structs & classes)
;;   - Selection range expansion/shrinking (structural selection)
;;   - Custom commands (run test, show references, compile file, build project)
;;   - Semantic token faces tuned for Gerbil Scheme (incl. property for obj.method)
;;   - Workspace symbol search with completion UI
;;   - Organize imports code action shortcut
;;   - Refactoring: extract to def, wrap in form, add export, cond↔if
;;   - Diagnostic tag faces (unused = faded, deprecated = strikethrough)
;;   - Project configuration (.gerbil-lsp.json)
;;
;; Eglot already handles completion, hover, go-to-definition, references,
;; rename, formatting, signature help, code actions, diagnostics, inlay
;; hints, and semantic tokens.  This package adds only what's missing.
;;
;; Usage:
;;
;;   (add-to-list 'load-path "/path/to/gerbil-lsp/emacs")
;;   (require 'eglot-gerbil)
;;
;; Then open a .ss file in `gerbil-mode' and run M-x eglot.  Or enable
;; auto-start:
;;
;;   (setq eglot-gerbil-auto-start t)

;;; Code:

(require 'eglot)
(require 'project)
(require 'seq)

;; Compatibility shims for eglot < 1.16 vs >= 1.16
(with-no-warnings
  (defalias 'eglot-gerbil--uri-to-path
    (if (fboundp 'eglot-uri-to-path) #'eglot-uri-to-path #'eglot--uri-to-path))
  (defalias 'eglot-gerbil--path-to-uri
    (if (fboundp 'eglot-path-to-uri) #'eglot-path-to-uri #'eglot--path-to-uri)))

;; =====================================================================
;; Customization
;; =====================================================================

(defgroup eglot-gerbil nil
  "Gerbil Scheme support for Eglot."
  :group 'eglot
  :prefix "eglot-gerbil-")

(defcustom eglot-gerbil-server-path "gerbil-lsp"
  "Path to the gerbil-lsp executable.
Set to an absolute path if the binary is not on your PATH."
  :type 'string
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-log-level "info"
  "Log level for the gerbil-lsp server."
  :type '(choice (const "debug")
                 (const "info")
                 (const "warn")
                 (const "error"))
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-enable-inlay-hints t
  "Enable inlay hints (parameter name annotations at call sites)."
  :type 'boolean
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-enable-code-lenses t
  "Enable code lenses (reference counts, test runners above definitions)."
  :type 'boolean
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-enable-validation t
  "Enable LSP response validation (debug mode).
When non-nil, the server validates all outgoing responses against
LSP protocol schemas and logs warnings for violations.  Useful
during development to catch protocol conformance issues."
  :type 'boolean
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-auto-start nil
  "When non-nil, automatically start Eglot in `gerbil-mode' buffers."
  :type 'boolean
  :group 'eglot-gerbil)

(defcustom eglot-gerbil-code-lens-face
  '(:height 0.85 :foreground "dim gray")
  "Face for code lens text displayed above definitions."
  :type 'plist
  :group 'eglot-gerbil)

;; =====================================================================
;; Server registration
;; =====================================================================

(defun eglot-gerbil--server-command (&rest _args)
  "Return the command to start the gerbil-lsp server."
  (append (list eglot-gerbil-server-path
                "--stdio"
                "--log-level" eglot-gerbil-log-level)
          (when eglot-gerbil-enable-validation
            (list "--validate"))))

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(gerbil-mode . ,#'eglot-gerbil--server-command)))

;; =====================================================================
;; Semantic token faces
;; =====================================================================

;; The server provides 11 token types and 2 modifiers:
;;   Types:     keyword, function, variable, parameter, type,
;;              macro, comment, string, number, operator, property
;;   Modifiers: definition, readonly

(defface eglot-gerbil-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Gerbil keywords and special forms."
  :group 'eglot-gerbil)

(defface eglot-gerbil-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for Gerbil function names."
  :group 'eglot-gerbil)

(defface eglot-gerbil-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Gerbil variables."
  :group 'eglot-gerbil)

(defface eglot-gerbil-parameter-face
  '((t :inherit font-lock-variable-name-face :slant italic))
  "Face for Gerbil function parameters."
  :group 'eglot-gerbil)

(defface eglot-gerbil-type-face
  '((t :inherit font-lock-type-face))
  "Face for Gerbil types (structs, classes)."
  :group 'eglot-gerbil)

(defface eglot-gerbil-macro-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for Gerbil macros."
  :group 'eglot-gerbil)

(defface eglot-gerbil-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for Gerbil comments."
  :group 'eglot-gerbil)

(defface eglot-gerbil-string-face
  '((t :inherit font-lock-string-face))
  "Face for Gerbil string literals."
  :group 'eglot-gerbil)

(defface eglot-gerbil-number-face
  '((t :inherit font-lock-number-face))
  "Face for Gerbil numeric literals."
  :group 'eglot-gerbil)

(defface eglot-gerbil-operator-face
  '((t :inherit font-lock-operator-face))
  "Face for Gerbil operators."
  :group 'eglot-gerbil)

(defface eglot-gerbil-property-face
  '((t :inherit font-lock-property-use-face))
  "Face for Gerbil dot-accessor property names (obj.method)."
  :group 'eglot-gerbil)

(defface eglot-gerbil-definition-face
  '((t :inherit bold))
  "Face modifier for definitions."
  :group 'eglot-gerbil)

(defface eglot-gerbil-readonly-face
  '((t :inherit font-lock-constant-face))
  "Face modifier for readonly/constant values."
  :group 'eglot-gerbil)

(defvar eglot-gerbil-semantic-token-faces
  '(("keyword"   . eglot-gerbil-keyword-face)
    ("function"  . eglot-gerbil-function-face)
    ("variable"  . eglot-gerbil-variable-face)
    ("parameter" . eglot-gerbil-parameter-face)
    ("type"      . eglot-gerbil-type-face)
    ("macro"     . eglot-gerbil-macro-face)
    ("comment"   . eglot-gerbil-comment-face)
    ("string"    . eglot-gerbil-string-face)
    ("number"    . eglot-gerbil-number-face)
    ("operator"  . eglot-gerbil-operator-face)
    ("property"  . eglot-gerbil-property-face))
  "Mapping from gerbil-lsp semantic token types to Emacs faces.")

(defvar eglot-gerbil-semantic-token-modifier-faces
  '(("definition" . eglot-gerbil-definition-face)
    ("readonly"   . eglot-gerbil-readonly-face))
  "Mapping from gerbil-lsp semantic token modifiers to Emacs faces.")

;; =====================================================================
;; Code lenses
;; =====================================================================

(defvar-local eglot-gerbil--code-lens-overlays nil
  "Active code lens overlays in the current buffer.")

(defun eglot-gerbil--clear-code-lenses ()
  "Remove all code lens overlays from the current buffer."
  (mapc #'delete-overlay eglot-gerbil--code-lens-overlays)
  (setq eglot-gerbil--code-lens-overlays nil))

(defun eglot-gerbil--render-code-lens (lens)
  "Render a single code LENS as an overlay above its target line."
  (let* ((range (plist-get lens :range))
         (command (plist-get lens :command))
         (start (plist-get range :start))
         (line (plist-get start :line))
         (title (plist-get command :title))
         (cmd-name (plist-get command :command))
         (cmd-args (plist-get command :arguments)))
    (when (and title cmd-name)
      (save-excursion
        (goto-char (point-min))
        (forward-line line)
        (let* ((bol (line-beginning-position))
               (ov (make-overlay bol bol))
               (map (make-sparse-keymap))
               (action (lambda (&rest _)
                         (interactive)
                         (when (eglot-current-server)
                           (jsonrpc-request
                            (eglot--current-server-or-lose)
                            :workspace/executeCommand
                            `(:command ,cmd-name :arguments ,cmd-args)))))
               (text (propertize (concat title "\n")
                                 'face eglot-gerbil-code-lens-face
                                 'mouse-face 'highlight
                                 'cursor t
                                 'keymap map)))
          (define-key map [mouse-1] action)
          (define-key map (kbd "RET") action)
          (overlay-put ov 'before-string text)
          (overlay-put ov 'eglot-gerbil-code-lens t)
          (push ov eglot-gerbil--code-lens-overlays))))))

;;;###autoload
(defun eglot-gerbil-refresh-code-lenses ()
  "Request and display code lenses in the current buffer."
  (interactive)
  (eglot-gerbil--ensure-server)
  (eglot-gerbil--clear-code-lenses)
  (when eglot-gerbil-enable-code-lenses
    (let* ((server (eglot--current-server-or-lose))
           (params `(:textDocument ,(eglot--TextDocumentIdentifier)))
           (lenses (jsonrpc-request server :textDocument/codeLens params)))
      (when lenses
        (seq-doseq (lens lenses)
          (eglot-gerbil--render-code-lens lens))))))

(defun eglot-gerbil--maybe-refresh-lenses ()
  "Refresh code lenses if enabled and a server is active."
  (when (and eglot-gerbil-enable-code-lenses (eglot-current-server))
    (eglot-gerbil-refresh-code-lenses)))

;; =====================================================================
;; Call hierarchy
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-incoming-calls ()
  "Show functions that call the symbol at point."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareCallHierarchy params)))
    (if (and items (length> items 0))
        (let* ((item (aref items 0))
               (calls (jsonrpc-request server :callHierarchy/incomingCalls
                                       `(:item ,item))))
          (if (and calls (length> calls 0))
              (eglot-gerbil--display-hierarchy
               "Incoming Calls" calls
               (lambda (entry) (plist-get entry :from)))
            (message "No incoming calls found")))
      (message "No call hierarchy item at point"))))

;;;###autoload
(defun eglot-gerbil-outgoing-calls ()
  "Show functions called by the symbol at point."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareCallHierarchy params)))
    (if (and items (length> items 0))
        (let* ((item (aref items 0))
               (calls (jsonrpc-request server :callHierarchy/outgoingCalls
                                       `(:item ,item))))
          (if (and calls (length> calls 0))
              (eglot-gerbil--display-hierarchy
               "Outgoing Calls" calls
               (lambda (entry) (plist-get entry :to)))
            (message "No outgoing calls found")))
      (message "No call hierarchy item at point"))))

;; =====================================================================
;; Type hierarchy
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-supertypes ()
  "Show supertypes of the struct/class at point."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareTypeHierarchy params)))
    (if (and items (length> items 0))
        (let* ((item (aref items 0))
               (supers (jsonrpc-request server :typeHierarchy/supertypes
                                        `(:item ,item))))
          (if (and supers (length> supers 0))
              (eglot-gerbil--display-hierarchy
               "Supertypes" (append supers nil) #'identity)
            (message "No supertypes found")))
      (message "No type hierarchy item at point"))))

;;;###autoload
(defun eglot-gerbil-subtypes ()
  "Show subtypes of the struct/class at point."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareTypeHierarchy params)))
    (if (and items (length> items 0))
        (let* ((item (aref items 0))
               (subs (jsonrpc-request server :typeHierarchy/subtypes
                                      `(:item ,item))))
          (if (and subs (length> subs 0))
              (eglot-gerbil--display-hierarchy
               "Subtypes" (append subs nil) #'identity)
            (message "No subtypes found")))
      (message "No type hierarchy item at point"))))

;; =====================================================================
;; Hierarchy display (shared by call and type hierarchy)
;; =====================================================================

(defun eglot-gerbil--display-hierarchy (title items extract-fn)
  "Display hierarchy ITEMS in a results buffer.
TITLE is the heading.  EXTRACT-FN takes an entry and returns the
item plist containing :name, :uri, :range, :kind."
  (let ((buf (get-buffer-create (format "*eglot-gerbil: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s\n\n" title)
                            'face '(:weight bold :height 1.1)))
        (seq-doseq (entry items)
          (let* ((target (funcall extract-fn entry))
                 (name (plist-get target :name))
                 (kind (plist-get target :kind))
                 (uri (plist-get target :uri))
                 (range (plist-get target :range))
                 (start (plist-get range :start))
                 (line (1+ (plist-get start :line)))
                 (file (eglot-gerbil--uri-to-path uri))
                 (short-file (file-name-nondirectory file)))
            (insert-text-button
             (format "%s [%s]" name (eglot-gerbil--symbol-kind-name kind))
             'action (lambda (_btn)
                       (find-file file)
                       (goto-char (point-min))
                       (forward-line (1- line)))
             'follow-link t)
            (insert (format "  %s:%d\n" short-file line))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun eglot-gerbil--symbol-kind-name (kind)
  "Return a human-readable name for SymbolKind number KIND."
  (pcase kind
    (1 "File") (2 "Module") (3 "Namespace") (4 "Package")
    (5 "Class") (6 "Method") (7 "Property") (8 "Field")
    (9 "Constructor") (10 "Enum") (11 "Interface") (12 "Function")
    (13 "Variable") (14 "Constant") (15 "String") (16 "Number")
    (17 "Boolean") (18 "Array") (19 "Object") (20 "Key")
    (21 "Null") (22 "EnumMember") (23 "Struct") (24 "Event")
    (25 "Operator") (26 "TypeParameter")
    (_ (format "Kind:%d" kind))))

;; =====================================================================
;; Selection range (structural expand/shrink)
;; =====================================================================

(defvar-local eglot-gerbil--selection-stack nil
  "Stack of previous selection ranges for shrinking back.")

;;;###autoload
(defun eglot-gerbil-expand-selection ()
  "Expand the selection to the next enclosing syntactic unit.
Uses the LSP selectionRange protocol to structurally expand."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (params `(:textDocument ,(eglot--TextDocumentIdentifier)
                   :positions ,(vector (eglot--pos-to-lsp-position))))
         (result (jsonrpc-request server :textDocument/selectionRange params)))
    (when (and result (length> result 0))
      (let* ((sel (aref result 0))
             (cur-start (point))
             (cur-end (if (use-region-p) (mark) (point)))
             (range (eglot-gerbil--find-expanding-range
                     sel cur-start cur-end)))
        (when range
          (push (cons cur-start cur-end) eglot-gerbil--selection-stack)
          (let ((beg (eglot--lsp-position-to-point (plist-get range :start)))
                (end (eglot--lsp-position-to-point (plist-get range :end))))
            (goto-char beg)
            (set-mark end)
            (activate-mark)))))))

;;;###autoload
(defun eglot-gerbil-shrink-selection ()
  "Shrink the selection back to the previous range."
  (interactive)
  (if eglot-gerbil--selection-stack
      (let ((prev (pop eglot-gerbil--selection-stack)))
        (goto-char (car prev))
        (if (= (car prev) (cdr prev))
            (deactivate-mark)
          (set-mark (cdr prev))
          (activate-mark)))
    (deactivate-mark)))

(defun eglot-gerbil--find-expanding-range (sel-range cur-start cur-end)
  "Walk SEL-RANGE tree for a range strictly larger than CUR-START..CUR-END."
  (let ((range (plist-get sel-range :range))
        (parent (plist-get sel-range :parent)))
    (when range
      (let ((beg (eglot--lsp-position-to-point (plist-get range :start)))
            (end (eglot--lsp-position-to-point (plist-get range :end)))
            (lo (min cur-start cur-end))
            (hi (max cur-start cur-end)))
        (if (and (<= beg lo) (>= end hi)
                 (or (< beg lo) (> end hi)))
            range
          (when parent
            (eglot-gerbil--find-expanding-range parent cur-start cur-end)))))))

;; =====================================================================
;; Custom commands
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-run-test ()
  "Run the test suite at point via the gerbil-lsp server.
The server locates the test definition and runs `gerbil test',
reporting results via window/showMessage."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let ((sym (thing-at-point 'symbol t)))
    (unless sym
      (user-error "No symbol at point"))
    (jsonrpc-request (eglot--current-server-or-lose)
                     :workspace/executeCommand
                     `(:command "gerbil-lsp.runTest"
                       :arguments ,(vector sym)))))

;;;###autoload
(defun eglot-gerbil-show-references ()
  "Show reference count for the symbol at point."
  (interactive)
  (eglot-gerbil--ensure-server)
  (let* ((pos (eglot--pos-to-lsp-position))
         (uri (eglot-gerbil--path-to-uri (buffer-file-name))))
    (jsonrpc-request (eglot--current-server-or-lose)
                     :workspace/executeCommand
                     `(:command "gerbil-lsp.showReferences"
                       :arguments ,(vector uri
                                          (plist-get pos :line)
                                          (plist-get pos :character))))))

;; =====================================================================
;; Build / compile commands
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-compile-file ()
  "Compile the current file with gxc via the gerbil-lsp server.
Runs `gxc -S' on the file and reports results via window/showMessage."
  (interactive)
  (eglot-gerbil--ensure-server)
  (unless buffer-file-name
    (user-error "Buffer has no associated file"))
  (jsonrpc-notify (eglot--current-server-or-lose)
                  :workspace/executeCommand
                  `(:command "gerbil-lsp.compileFile"
                    :arguments ,(vector (eglot-gerbil--path-to-uri buffer-file-name)))))

;;;###autoload
(defun eglot-gerbil-build-project ()
  "Build the Gerbil project containing the current file.
Walks up to find build.ss or gerbil.pkg and runs `gerbil build'
in that directory.  Results reported via window/showMessage."
  (interactive)
  (eglot-gerbil--ensure-server)
  (unless buffer-file-name
    (user-error "Buffer has no associated file"))
  (jsonrpc-notify (eglot--current-server-or-lose)
                  :workspace/executeCommand
                  `(:command "gerbil-lsp.buildProject"
                    :arguments ,(vector (eglot-gerbil--path-to-uri buffer-file-name)))))

;; =====================================================================
;; Workspace symbol search with completion
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-workspace-symbol (query)
  "Search workspace symbols matching QUERY with a completion UI."
  (interactive "sWorkspace symbol query: ")
  (eglot-gerbil--ensure-server)
  (let* ((server (eglot--current-server-or-lose))
         (result (jsonrpc-request server :workspace/symbol
                                 `(:query ,query))))
    (if (and result (length> result 0))
        (let* ((items (append result nil))
               (candidates
                (mapcar (lambda (item)
                          (let* ((name (plist-get item :name))
                                 (kind (plist-get item :kind))
                                 (loc (plist-get item :location))
                                 (uri (plist-get loc :uri))
                                 (range (plist-get loc :range))
                                 (start (plist-get range :start))
                                 (line (1+ (plist-get start :line)))
                                 (file (eglot-gerbil--uri-to-path uri)))
                            (cons (format "%s  [%s]  %s:%d"
                                          name
                                          (eglot-gerbil--symbol-kind-name kind)
                                          (file-name-nondirectory file)
                                          line)
                                  item)))
                        items))
               (selected (completing-read "Symbol: " candidates nil t))
               (item (cdr (assoc selected candidates))))
          (when item
            (let* ((loc (plist-get item :location))
                   (uri (plist-get loc :uri))
                   (start (plist-get (plist-get loc :range) :start))
                   (file (eglot-gerbil--uri-to-path uri)))
              (find-file file)
              (goto-char (point-min))
              (forward-line (plist-get start :line)))))
      (message "No symbols found matching \"%s\"" query))))

;; =====================================================================
;; Organize imports
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-organize-imports ()
  "Organize imports in the current buffer via code action."
  (interactive)
  (eglot-gerbil--ensure-server)
  (eglot-code-actions nil nil "source.organizeImports" t))

;; =====================================================================
;; Refactoring commands (extract, wrap, add-export, cond↔if)
;; =====================================================================

;;;###autoload
(defun eglot-gerbil-extract-to-def ()
  "Extract the selected expression to a new top-level `def'.
Requires an active region.  The server replaces the selection with
a call to the new function and inserts the def above."
  (interactive)
  (eglot-gerbil--ensure-server)
  (unless (use-region-p)
    (user-error "Select the expression to extract first"))
  (eglot-code-actions (region-beginning) (region-end) "refactor.extract" t))

;;;###autoload
(defun eglot-gerbil-wrap-in-form ()
  "Wrap the selected expression in a Scheme form.
Presents available wrapping options (when, unless, let, begin, try).
Requires an active region."
  (interactive)
  (eglot-gerbil--ensure-server)
  (unless (use-region-p)
    (user-error "Select the expression to wrap first"))
  (eglot-code-actions (region-beginning) (region-end) "refactor.rewrite"))

;;;###autoload
(defun eglot-gerbil-add-missing-export ()
  "Add the symbol at point to the file's (export ...) form.
Only offered when the cursor is on a defined-but-not-exported symbol."
  (interactive)
  (eglot-gerbil--ensure-server)
  (eglot-code-actions (point) (point) "quickfix" t))

;;;###autoload
(defun eglot-gerbil-convert-conditional ()
  "Convert between `cond' and `if' at point.
If the cursor is on a `cond' with two clauses, converts to `if'.
If the cursor is on an `if', converts to `cond'."
  (interactive)
  (eglot-gerbil--ensure-server)
  (eglot-code-actions (point) (point) "refactor.rewrite" t))

;; =====================================================================
;; Diagnostic tag faces (unused = faded, deprecated = strikethrough)
;; =====================================================================

(defface eglot-gerbil-unnecessary-face
  '((t :inherit shadow))
  "Face for unnecessary/unused code (DiagnosticTag.Unnecessary).
Applied to unused imports and dead code."
  :group 'eglot-gerbil)

(defface eglot-gerbil-deprecated-face
  '((t :strike-through t))
  "Face for deprecated symbols (DiagnosticTag.Deprecated)."
  :group 'eglot-gerbil)

;; =====================================================================
;; Project configuration
;; =====================================================================

(defun eglot-gerbil--project-root ()
  "Find the Gerbil project root by locating gerbil.pkg."
  (when buffer-file-name
    (locate-dominating-file buffer-file-name "gerbil.pkg")))

;;;###autoload
(defun eglot-gerbil-edit-project-config ()
  "Open or create the .gerbil-lsp.json project configuration file.
If the file does not exist, inserts a starter template."
  (interactive)
  (let* ((root (or (eglot-gerbil--project-root)
                   (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   default-directory))
         (config (expand-file-name ".gerbil-lsp.json" root)))
    (find-file config)
    (when (= (buffer-size) 0)
      (insert "{\n"
              "  \"gxc-path\": \"gxc\",\n"
              "  \"gxc-flags\": [\"-O\"],\n"
              "  \"loadpath\": [],\n"
              "  \"diagnostics-delay\": 1500,\n"
              "  \"disabled-features\": []\n"
              "}\n")
      (goto-char (point-min))
      (message "Created .gerbil-lsp.json template -- save and restart Eglot to apply"))))

;; =====================================================================
;; Helper
;; =====================================================================

(defun eglot-gerbil--ensure-server ()
  "Signal an error if no Eglot server is active."
  (unless (eglot-current-server)
    (user-error "No active Eglot session (run M-x eglot first)")))

;; =====================================================================
;; Minor mode
;; =====================================================================

(defvar eglot-gerbil-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Custom commands
    (define-key map (kbd "C-c l t") #'eglot-gerbil-run-test)
    (define-key map (kbd "C-c l r") #'eglot-gerbil-show-references)
    ;; Call hierarchy
    (define-key map (kbd "C-c l c i") #'eglot-gerbil-incoming-calls)
    (define-key map (kbd "C-c l c o") #'eglot-gerbil-outgoing-calls)
    ;; Type hierarchy
    (define-key map (kbd "C-c l h s") #'eglot-gerbil-supertypes)
    (define-key map (kbd "C-c l h b") #'eglot-gerbil-subtypes)
    ;; Selection range
    (define-key map (kbd "C-c l +") #'eglot-gerbil-expand-selection)
    (define-key map (kbd "C-c l -") #'eglot-gerbil-shrink-selection)
    ;; Code lenses
    (define-key map (kbd "C-c l l") #'eglot-gerbil-refresh-code-lenses)
    ;; Workspace symbols
    (define-key map (kbd "C-c l s") #'eglot-gerbil-workspace-symbol)
    ;; Organize imports
    (define-key map (kbd "C-c l o") #'eglot-gerbil-organize-imports)
    ;; Refactoring
    (define-key map (kbd "C-c l x") #'eglot-gerbil-extract-to-def)
    (define-key map (kbd "C-c l (") #'eglot-gerbil-wrap-in-form)
    (define-key map (kbd "C-c l e") #'eglot-gerbil-add-missing-export)
    (define-key map (kbd "C-c l C") #'eglot-gerbil-convert-conditional)
    ;; Build / compile
    (define-key map (kbd "C-c l b") #'eglot-gerbil-build-project)
    (define-key map (kbd "C-c l B") #'eglot-gerbil-compile-file)
    ;; Project config
    (define-key map (kbd "C-c l p") #'eglot-gerbil-edit-project-config)
    ;; Standard Eglot commands on convenient keys
    (define-key map (kbd "C-c l a") #'eglot-code-actions)
    (define-key map (kbd "C-c l n") #'eglot-rename)
    (define-key map (kbd "C-c l f") #'eglot-format-buffer)
    (define-key map (kbd "C-c l d") #'xref-find-definitions)
    (define-key map (kbd "C-c l D") #'eglot-find-declaration)
    (define-key map (kbd "C-c l T") #'eglot-find-typeDefinition)
    (define-key map (kbd "C-c l R") #'xref-find-references)
    (define-key map (kbd "C-c l i") #'eglot-find-implementation)
    (define-key map (kbd "C-c l w") #'xref-find-apropos)
    map)
  "Keymap for `eglot-gerbil-mode'.")

;;;###autoload
(define-minor-mode eglot-gerbil-mode
  "Minor mode for Gerbil-specific Eglot extensions.
Adds code lenses, call/type hierarchy, selection range, refactoring
commands, and convenient keybindings.

\\{eglot-gerbil-mode-map}"
  :lighter " GLS"
  :keymap eglot-gerbil-mode-map
  (if eglot-gerbil-mode
      (progn
        (when (and eglot-gerbil-enable-inlay-hints
                   (fboundp 'eglot-inlay-hints-mode))
          (eglot-inlay-hints-mode 1))
        (when eglot-gerbil-enable-code-lenses
          (add-hook 'after-save-hook #'eglot-gerbil--maybe-refresh-lenses nil t)))
    (eglot-gerbil--clear-code-lenses)
    (when (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode -1))
    (remove-hook 'after-save-hook #'eglot-gerbil--maybe-refresh-lenses t)))

;; =====================================================================
;; Eglot managed-mode hook (auto-activate)
;; =====================================================================

(defun eglot-gerbil--managed-mode-hook ()
  "Activate or deactivate `eglot-gerbil-mode' with Eglot."
  (when (derived-mode-p 'gerbil-mode)
    (if (bound-and-true-p eglot--managed-mode)
        (progn
          (eglot-gerbil-mode 1)
          ;; Wire up semantic token faces
          (when (boundp 'eglot--semantic-tokens-faces-override)
            (setq-local eglot--semantic-tokens-faces-override
                        eglot-gerbil-semantic-token-faces))
          ;; Initial code lens fetch after server settles
          (when eglot-gerbil-enable-code-lenses
            (let ((buf (current-buffer)))
              (run-with-idle-timer
               1 nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (eglot-gerbil--maybe-refresh-lenses))))))))
      (eglot-gerbil-mode -1))))

(add-hook 'eglot-managed-mode-hook #'eglot-gerbil--managed-mode-hook)

;; =====================================================================
;; Auto-start support
;; =====================================================================

(defun eglot-gerbil--maybe-auto-start ()
  "Start Eglot automatically if `eglot-gerbil-auto-start' is non-nil."
  (when eglot-gerbil-auto-start
    (eglot-ensure)))

;;;###autoload
(with-eval-after-load 'gerbil-mode
  (add-hook 'gerbil-mode-hook #'eglot-gerbil--maybe-auto-start))

;; =====================================================================
;; Provide
;; =====================================================================

(provide 'eglot-gerbil)

;;; eglot-gerbil.el ends here
