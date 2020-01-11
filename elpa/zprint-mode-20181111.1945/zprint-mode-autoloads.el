;;; zprint-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zprint-mode" "zprint-mode.el" (0 0 0 0))
;;; Generated autoloads from zprint-mode.el

(autoload 'zprint "zprint-mode" "\
Reformat code using zprint.
If region is active, reformat it; otherwise reformat entire buffer.
When called interactively, or with prefix argument IS-INTERACTIVE,
show a buffer if the formatting fails

\(fn &optional IS-INTERACTIVE)" t nil)

(autoload 'zprint-mode "zprint-mode" "\
Minor mode for reformatting Clojure(Script) code using zprint

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zprint-mode-autoloads.el ends here
