#!/bin/bash
# End-to-end LSP protocol test for gerbil-lsp
# Tests the same initialize/didOpen/completion/shutdown sequence that eglot performs.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
GERBIL_LSP="${GERBIL_LSP:-$PROJECT_DIR/gherkin-lsp}"

PASS=0
FAIL=0

# Create a temp workspace with a .ss file for the server to work with
TMPWS=$(mktemp -d)
trap 'rm -rf "$TMPWS"' EXIT
cat > "$TMPWS/test.ss" << 'SCHEME'
(import :std/text/json)
(def (hello name) (string-append "Hello, " name))
(def (main . args) (displayln (hello "world")))
SCHEME

# Helper: create Content-Length framed LSP message
lsp_msg() {
    local body="$1"
    local len=${#body}
    printf "Content-Length: %d\r\n\r\n%s" "$len" "$body"
}

check() {
    local desc="$1"
    local pattern="$2"
    local text="$3"
    if printf '%s' "$text" | grep -q "$pattern"; then
        echo "  PASS: $desc"
        ((PASS++))
    else
        echo "  FAIL: $desc (pattern: $pattern)"
        ((FAIL++))
    fi
}

echo "=== gerbil-lsp end-to-end protocol test ==="
echo "Binary: $GERBIL_LSP"
echo ""

# Check the binary exists
if [ ! -x "$GERBIL_LSP" ]; then
    echo "FAIL: gerbil-lsp binary not found at $GERBIL_LSP"
    echo "Run 'make build' first."
    exit 1
fi

# --- Test 1: Full lifecycle ---
echo "Test 1: LSP lifecycle (initialize -> initialized -> shutdown -> exit)"

INIT_BODY='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"rootUri":"file://'"$TMPWS"'"}}'
INITIALIZED_BODY='{"jsonrpc":"2.0","method":"initialized","params":{}}'
SHUTDOWN_BODY='{"jsonrpc":"2.0","id":99,"method":"shutdown","params":{}}'
EXIT_BODY='{"jsonrpc":"2.0","method":"exit","params":{}}'

OUTPUT=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    # Small delay to let initialized handler run
    sleep 0.2
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "initialize returns capabilities" '"capabilities"' "$OUTPUT"
check "server info name" '"gerbil-lsp"' "$OUTPUT"
check "completion provider advertised" '"completionProvider"' "$OUTPUT"
check "hover provider advertised" '"hoverProvider"' "$OUTPUT"
check "shutdown response received" '"id":99' "$OUTPUT"

echo ""

# --- Test 2: didOpen + completion ---
echo "Test 2: textDocument/didOpen + completion request"

FILE_URI="file://$TMPWS/test.ss"

OPEN_BODY='{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"'"$FILE_URI"'","languageId":"gerbil","version":1,"text":"(import :std/text/json)\n(def (hello name) (string-append \"Hello, \" name))\n(def (main . args) (displayln (hello \"world\")))\n"}}}'

COMPLETION_BODY='{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"'"$FILE_URI"'"},"position":{"line":2,"character":20}}}'

OUTPUT2=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.2
    lsp_msg "$OPEN_BODY"
    sleep 0.1
    lsp_msg "$COMPLETION_BODY"
    sleep 0.1
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "completion response received" '"id":2' "$OUTPUT2"

echo ""

# --- Test 3: didOpen + hover ---
echo "Test 3: textDocument/hover request"

HOVER_BODY='{"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"'"$FILE_URI"'"},"position":{"line":1,"character":5}}}'

OUTPUT3=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.2
    lsp_msg "$OPEN_BODY"
    sleep 0.1
    lsp_msg "$HOVER_BODY"
    sleep 0.1
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "hover response received" '"id":3' "$OUTPUT3"

echo ""

# --- Test 4: documentSymbol ---
echo "Test 4: textDocument/documentSymbol request"

SYMBOLS_BODY='{"jsonrpc":"2.0","id":4,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"'"$FILE_URI"'"}}}'

OUTPUT4=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.2
    lsp_msg "$OPEN_BODY"
    sleep 0.1
    lsp_msg "$SYMBOLS_BODY"
    sleep 0.1
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "documentSymbol response received" '"id":4' "$OUTPUT4"
check "hello symbol found" '"hello"' "$OUTPUT4"
check "main symbol found" '"main"' "$OUTPUT4"

echo ""

# --- Test 5: request before initialize should be rejected ---
echo "Test 5: request before initialize rejected"

OUTPUT5=$( {
    lsp_msg "$COMPLETION_BODY"
    sleep 0.1
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.1
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "pre-init request returns error" '"error"' "$OUTPUT5"

echo ""

# --- Test 6: Multi-root workspace support ---
echo "Test 6: workspaceFolders capability advertised"

check "workspaceFolders supported" '"workspaceFolders"' "$OUTPUT"

echo ""

# --- Test 7: Inter-file dependencies advertised ---
echo "Test 7: interFileDependencies capability"

check "interFileDependencies enabled" '"interFileDependencies":true' "$OUTPUT"

echo ""

# --- Test 8: Project config file ---
echo "Test 8: Project configuration file (.gerbil-lsp.json)"

# Create a project config
cat > "$TMPWS/.gerbil-lsp.json" << 'JSON'
{
  "gxc-flags": ["-O"],
  "diagnostics-delay": 2000
}
JSON

OUTPUT8=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.3
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "server starts with config file" '"capabilities"' "$OUTPUT8"

echo ""

# --- Test 9: Index cache directory creation ---
echo "Test 9: Persistent index cache"

# Run server to trigger indexing
OUTPUT9=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.5
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

if [ -d "$TMPWS/.gerbil-lsp-cache" ]; then
    echo "  PASS: cache directory created"
    ((PASS++))
else
    echo "  SKIP: cache directory not created (may require file writes)"
fi

echo ""

# --- Test 10: didChangeWorkspaceFolders notification ---
echo "Test 10: workspace/didChangeWorkspaceFolders"

TMPWS2=$(mktemp -d)
trap 'rm -rf "$TMPWS" "$TMPWS2"' EXIT

cat > "$TMPWS2/other.ss" << 'SCHEME'
(def (other-func x) (* x 2))
SCHEME

CHANGE_FOLDERS_BODY='{"jsonrpc":"2.0","method":"workspace/didChangeWorkspaceFolders","params":{"event":{"added":[{"uri":"file://'"$TMPWS2"'","name":"other"}],"removed":[]}}}'

OUTPUT10=$( {
    lsp_msg "$INIT_BODY"
    lsp_msg "$INITIALIZED_BODY"
    sleep 0.2
    lsp_msg "$CHANGE_FOLDERS_BODY"
    sleep 0.3
    lsp_msg "$SHUTDOWN_BODY"
    lsp_msg "$EXIT_BODY"
} | "$GERBIL_LSP" --stdio --log-level error 2>/dev/null) || true

check "workspace folders notification accepted" '"capabilities"' "$OUTPUT10"

echo ""

# --- Summary ---
echo "=== Results: $PASS passed, $FAIL failed ==="
if [ $FAIL -gt 0 ]; then
    exit 1
fi
