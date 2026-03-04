;;; -*- Gerbil -*-
;;; Compatibility shim for Gerbil v0.18
;;; Re-exports from standard v0.18 module paths
(import :std/format
        :std/sort
        :std/misc/ports
        :std/misc/string
        :std/cli/getopt)
(export format fprintf sort
        read-file-string read-all-as-string
        string-trim-eol
        call-with-getopt flag option)
