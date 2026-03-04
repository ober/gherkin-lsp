;;; -*- Gerbil -*-
;;; LSP protocol type constructors
;;; All functions return hash tables that serialize to JSON via :std/text/json
(export #t)

;;; LSP Symbol kinds (CompletionItemKind / SymbolKind)
(def SymbolKind.File          1)
(def SymbolKind.Module        2)
(def SymbolKind.Namespace     3)
(def SymbolKind.Package       4)
(def SymbolKind.Class         5)
(def SymbolKind.Method        6)
(def SymbolKind.Property      7)
(def SymbolKind.Field         8)
(def SymbolKind.Constructor   9)
(def SymbolKind.Enum         10)
(def SymbolKind.Interface    11)
(def SymbolKind.Function     12)
(def SymbolKind.Variable     13)
(def SymbolKind.Constant     14)
(def SymbolKind.String       15)
(def SymbolKind.Number       16)
(def SymbolKind.Boolean      17)
(def SymbolKind.Array        18)
(def SymbolKind.Object       19)
(def SymbolKind.Key          20)
(def SymbolKind.Null         21)
(def SymbolKind.EnumMember   22)
(def SymbolKind.Struct       23)
(def SymbolKind.Event        24)
(def SymbolKind.Operator     25)
(def SymbolKind.TypeParameter 26)

;;; CompletionItemKind
(def CompletionItemKind.Text          1)
(def CompletionItemKind.Method        2)
(def CompletionItemKind.Function      3)
(def CompletionItemKind.Constructor   4)
(def CompletionItemKind.Field         5)
(def CompletionItemKind.Variable      6)
(def CompletionItemKind.Class         7)
(def CompletionItemKind.Interface     8)
(def CompletionItemKind.Module        9)
(def CompletionItemKind.Property     10)
(def CompletionItemKind.Keyword      14)
(def CompletionItemKind.Snippet      15)
(def CompletionItemKind.Struct       22)
(def CompletionItemKind.Macro        CompletionItemKind.Snippet)

;;; DiagnosticSeverity
(def DiagnosticSeverity.Error       1)
(def DiagnosticSeverity.Warning     2)
(def DiagnosticSeverity.Information 3)
(def DiagnosticSeverity.Hint        4)

;;; DiagnosticTag
(def DiagnosticTag.Unnecessary 1)
(def DiagnosticTag.Deprecated  2)

;;; TextDocumentSyncKind
(def TextDocumentSyncKind.None        0)
(def TextDocumentSyncKind.Full        1)
(def TextDocumentSyncKind.Incremental 2)

;;; MarkupKind
(def MarkupKind.PlainText "plaintext")
(def MarkupKind.Markdown  "markdown")

;;; InsertTextFormat
(def InsertTextFormat.PlainText 1)
(def InsertTextFormat.Snippet   2)

;;; --- Type constructors ---

(def (make-text-edit range new-text)
  (hash ("range" range) ("newText" new-text)))

(def (make-workspace-edit changes)
  ;; changes: hash of uri → TextEdit[]
  (hash ("changes" changes)))

(def (make-diagnostic range message
                      severity: (severity DiagnosticSeverity.Error)
                      source: (source "gerbil-lsp")
                      code: (code (void))
                      tags: (tags (void))
                      related-information: (related-information (void)))
  (let ((diag (hash ("range" range)
                     ("message" message)
                     ("severity" severity)
                     ("source" source))))
    (unless (void? code)
      (hash-put! diag "code" code))
    (unless (void? tags)
      (hash-put! diag "tags" (if (list? tags) (list->vector tags) tags)))
    (unless (void? related-information)
      (hash-put! diag "relatedInformation"
        (if (list? related-information)
          (list->vector related-information)
          related-information)))
    diag))

(def (make-completion-item label
                           kind: (kind CompletionItemKind.Text)
                           detail: (detail (void))
                           documentation: (documentation (void))
                           insert-text: (insert-text (void)))
  (let ((item (hash ("label" label) ("kind" kind))))
    (unless (void? detail)
      (hash-put! item "detail" detail))
    (unless (void? documentation)
      (hash-put! item "documentation"
                 (if (string? documentation)
                   (hash ("kind" MarkupKind.Markdown) ("value" documentation))
                   documentation)))
    (unless (void? insert-text)
      (hash-put! item "insertText" insert-text))
    item))

(def (make-hover contents (range (void)))
  (let ((h (hash ("contents" (hash ("kind" MarkupKind.Markdown)
                                    ("value" contents))))))
    (unless (void? range)
      (hash-put! h "range" range))
    h))

(def (make-signature-help signatures active-signature active-parameter)
  (hash ("signatures" signatures)
        ("activeSignature" active-signature)
        ("activeParameter" active-parameter)))

(def (make-signature-information label
                                 documentation: (documentation (void))
                                 parameters: (parameters '()))
  (let ((sig (hash ("label" label) ("parameters" (list->vector parameters)))))
    (unless (void? documentation)
      (hash-put! sig "documentation"
                 (hash ("kind" MarkupKind.Markdown) ("value" documentation))))
    sig))

(def (make-parameter-information label (documentation (void)))
  (let ((param (hash ("label" label))))
    (unless (void? documentation)
      (hash-put! param "documentation" documentation))
    param))

(def (make-document-symbol name kind range selection-range
                            children: (children (void)))
  (let ((sym (hash ("name" name)
                    ("kind" kind)
                    ("range" range)
                    ("selectionRange" selection-range))))
    (unless (void? children)
      (hash-put! sym "children" (list->vector children)))
    sym))

(def (make-symbol-information name kind location)
  (hash ("name" name) ("kind" kind) ("location" location)))
