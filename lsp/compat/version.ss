;;; Version detection for Gerbil v0.18 vs v0.19 compatibility
(export gerbil-v019? gerbil-minor-version)

;; Parse minor version from (gerbil-version-string)
;; e.g. "v0.18.1-173-gb3417266" -> 18
;;      "v0.19-rc1"             -> 19
(def *gerbil-minor-version*
  (let* ((vs (gerbil-version-string))
         (parts (string-split vs #\.))
         (minor-str (cadr parts)))
    (string->number minor-str)))

(def (gerbil-v019?)
  (>= *gerbil-minor-version* 19))

(def (gerbil-minor-version)
  *gerbil-minor-version*)
