SCHEME = $(HOME)/.local/bin/scheme
GHERKIN = $(or $(GHERKIN_DIR),$(HOME)/mine/gherkin/src)
LIBDIRS = src:$(GHERKIN)
COMPILE = $(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries

.PHONY: all compile gherkin binary clean help run

all: gherkin compile

# Step 1: Translate .ss → .sls via gherkin compiler
gherkin:
	$(COMPILE) < build-gherkin.ss

# Step 2: Compile .sls → .so via Chez
compile: gherkin
	$(COMPILE) < build-all.ss

# Build = full pipeline (translate + compile + link binary)
build: binary

# Native binary
binary: clean gherkin
	$(SCHEME) -q --libdirs $(LIBDIRS) --program build-binary.ss

# Run interpreted
run: all
	$(SCHEME) -q --libdirs $(LIBDIRS) --program lsp.ss -- --stdio

clean:
	rm -f lsp-main.o lsp_program.h
	rm -f lsp.boot lsp-all.so lsp.so lsp.wpo
	rm -f petite.boot scheme.boot
	find src -name '*.so' -o -name '*.wpo' | xargs rm -f 2>/dev/null || true
	# Remove generated .sls files (keep handwritten ones like compat.sls)
	rm -f src/lsp/util-log.sls src/lsp/util-string.sls src/lsp/util-position.sls
	rm -f src/lsp/types.sls src/lsp/jsonrpc.sls src/lsp/state.sls
	rm -f src/lsp/capabilities.sls src/lsp/validation.sls src/lsp/transport.sls
	rm -f src/lsp/server.sls
	rm -f src/lsp/analysis-*.sls
	rm -f src/lsp/handlers-*.sls
	rm -f src/lsp/lib.sls src/lsp/main.sls

help:
	@echo "Targets:"
	@echo "  all       - Translate .ss→.sls + compile .sls→.so"
	@echo "  build     - Build standalone binary (./gherkin-lsp)"
	@echo "  binary    - Same as build"
	@echo "  run       - Run LSP server interpreted (no binary)"
	@echo "  gherkin   - Translate .ss → .sls only"
	@echo "  compile   - Compile .sls → .so only"
	@echo "  clean     - Remove all build artifacts"
	@echo "  help      - Show this help"
