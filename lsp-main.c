/*
 * lsp-main.c — Custom entry point for gherkin-lsp.
 *
 * Boot files (petite.boot, scheme.boot, lsp.boot) are embedded as C byte
 * arrays and registered via Sregister_boot_file_bytes — no external files needed.
 *
 * Threading workaround: Programs in boot files cannot create threads
 * (Chez bug — fork-thread blocks on internal GC futex). The program is
 * loaded separately via Sscheme_script on a memfd.
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include "scheme.h"
#include "lsp_program.h"      /* generated: lsp_program_data[], lsp_program_size */
#include "lsp_petite_boot.h"  /* generated: petite_boot_data[], petite_boot_size */
#include "lsp_scheme_boot.h"  /* generated: scheme_boot_data[], scheme_boot_size */
#include "lsp_lsp_boot.h"     /* generated: lsp_boot_data[], lsp_boot_size */

int main(int argc, char *argv[]) {
    /* Save args in positional env vars: LSP_ARGC, LSP_ARG0, LSP_ARG1, ... */
    char countbuf[32];
    snprintf(countbuf, sizeof(countbuf), "%d", argc - 1);
    setenv("LSP_ARGC", countbuf, 1);

    for (int i = 1; i < argc; i++) {
        char name[32];
        snprintf(name, sizeof(name), "LSP_ARG%d", i - 1);
        setenv(name, argv[i], 1);
    }

    /* Create memfd for embedded program .so */
    int fd = memfd_create("lsp-program", MFD_CLOEXEC);
    if (fd < 0) {
        perror("memfd_create");
        return 1;
    }
    if (write(fd, lsp_program_data, lsp_program_size) != (ssize_t)lsp_program_size) {
        perror("write memfd");
        close(fd);
        return 1;
    }
    char prog_path[64];
    snprintf(prog_path, sizeof(prog_path), "/proc/self/fd/%d", fd);

    /* Initialize Chez Scheme */
    Sscheme_init(NULL);

    /* Register embedded boot files (no external files needed) */
    Sregister_boot_file_bytes("petite", (void*)petite_boot_data, petite_boot_size);
    Sregister_boot_file_bytes("scheme", (void*)scheme_boot_data, scheme_boot_size);
    Sregister_boot_file_bytes("lsp",    (void*)lsp_boot_data,    lsp_boot_size);

    /* Build heap from registered boot files (libraries only — no program) */
    Sbuild_heap(NULL, NULL);

    /* Run the program via Sscheme_script (NOT Sscheme_start) */
    const char *script_args[] = { argv[0] };
    int status = Sscheme_script(prog_path, 1, script_args);

    close(fd);
    Sscheme_deinit();
    return status;
}
