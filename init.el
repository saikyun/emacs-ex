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
 '(cider-prompt-for-symbol nil)
 '(company-auto-complete t)
 '(company-begin-commands nil)
 '(company-idle-delay nil)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "eb7be1648009af366d83f855191057bdc09348a2d9353db31da03b1cdec50cc5" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(fci-rule-color "#151515")
 '(global-highlight-parentheses-mode t)
 '(helm-ag-ignore-patterns (quote ("*.min.js")))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-paren-attributes nil)
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("firebrick1")))
 '(hl-paren-delay 0.001)
 '(hl-paren-highlight-adjacent t)
 '(hl-sexp-background-colors (quote ("bisque1" "bisque2")))
 '(hl-sexp-colors nil)
 '(js-indent-level 2)
 '(love-exe "/Applications/love.app/Contents/MacOS/love")
 '(magit-diff-use-overlays nil)
 '(miracle-always-pretty-print t)
 '(miracle-custom-eval-wrappers nil)
 '(monroe-default-host "localhost:3722")
 '(neo-theme (quote nerd))
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote alt))
 '(package-selected-packages
   (quote
    (zprint-mode flycheck-clj-kondo smartparens cider edn multiple-cursors coffee-mode solarized-theme command-log-mode jazz-theme monokai-theme prettier-js web-mode neotree magit cargo company-racer racer flymake-rust rust-mode highlight-parentheses expand-region projectile helm-ag company-anaconda anaconda-mode eglot lsp-mode helm-swoop helm-company company monroe dracula-theme omnisharp csharp-mode lua-mode multi-line inf-clojure avy flycheck tide exec-path-from-shell paredit-everywhere paredit highlight-symbol helm-projectile auto-indent-mode auto-auto-indent)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(projectile-globally-ignored-file-suffixes (quote (".min.js" ".meta")))
 '(safe-local-variable-values
   (quote
    ((eval setq byte-compile-not-obsolete-vars
	   (quote
	    (display-buffer-function))))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-pairing nil)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
