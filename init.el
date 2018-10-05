(require 'package)
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
 '(auto-indent-delete-trailing-whitespace-on-save-file t)
 '(auto-indent-on-save-file t)
 '(beacon-color "#c82829")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(highlight-symbol-idle-delay 0.5)
 '(inferior-lisp-program "lein run -m clojure.main")
 '(org-agenda-files
   (quote
    ("~/notes/" "~/.emacs.d/startup.org" "~/devops/pts-differ/plan/start.org")))
 '(org-babel-tangle-lang-exts
   (quote
    (("emacs-lisp" . "el")
     ("elisp" . "el")
     ("visual-basic" . "vbs"))))
 '(package-selected-packages
   (quote
    (cider highlight-symbol vbasense helm-projectile org-pomodoro paredit avy color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized projectile helm)))
 '(vbasense-tli-files
   (quote
    ("c:/WINDOWS/system32/vbscript.dll/3" "c:/WINDOWS/system32/scrrun.dll" "c:/WINDOWS/system32/wshom.ocx" "c:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE" "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA7.1/VBE7.DLL" "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA7.1/VBEUI.DLL" "c:/Program Files/Common Files/Microsoft Shared/VBA/VBA7.1/VBEUIRES.DLL" "c:/Program Files/Common Files/Microsoft Shared/OFFICE11/MSO.DLL" "c:/WINDOWS/system32/stdole2.tlb")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
