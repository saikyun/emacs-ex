(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/startup.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-indent-indent-style (quote aggressive))
 '(auto-indent-next-pair nil)
 '(auto-indent-on-save-file t)
 '(company-auto-complete t)
 '(company-begin-commands nil)
 '(company-idle-delay nil)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(hl-sexp-background-colors (quote ("gray1" "gray20")))
 '(hl-sexp-colors nil)
 '(love-exe "/Applications/love.app/Contents/MacOS/love")
 '(miracle-always-pretty-print t)
 '(miracle-custom-eval-wrappers nil)
 '(monroe-default-host "localhost:3722")
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote alt))
 '(package-selected-packages
   (quote
    (projectile helm-ag company-anaconda anaconda-mode eglot lsp-mode helm-swoop helm-company company monroe dracula-theme omnisharp csharp-mode lua-mode multiple-cursors multi-line inf-clojure avy flycheck tide exec-path-from-shell paredit-everywhere paredit highlight-symbol helm-projectile cider auto-indent-mode auto-auto-indent)))
 '(safe-local-variable-values
   (quote
    ((eval setq byte-compile-not-obsolete-vars
           (quote
            (display-buffer-function)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
