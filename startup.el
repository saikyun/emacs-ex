(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(server-start)

(desktop-save-mode 1)

(setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files\\Git\\mingw64\\bin"))
(setenv "PATH" (concat (getenv "PATH") ";C:\\Users\\u045862\\AppData\\Roaming"))
(add-to-list 'exec-path "C:/Users/u045862/AppData/Roaming")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))

(add-hook 'org-mode-hook '(lambda ()
                            (add-hook 'after-save-hook 'org-babel-tangle nil t)))

(require 'projectile)
  (projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-switch-project-action #'helm-projectile)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)

  (setq cider-repl-use-pretty-printing t)
(require 'cider)

  (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
  (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)

(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(control shift f3)] 'highlight-symbol-remove-all)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(add-to-list 'load-path "~/.emacs.d/scripts")
(let ((default-directory  "~/.emacs.d/scripts/"))
    (normal-top-level-add-subdirs-to-load-path))

(require 'better-defaults)
(require 'auto-indent-mode)

(auto-indent-global-mode)

(global-linum-mode)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)))

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(defun end-of-line-newline ()
  (interactive)
  (end-of-line)
  (newline))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(global-set-key (kbd "M-m") 'delete-indentation)
(global-set-key (kbd "C-S-z") 'revert-buffer)

(global-set-key (kbd "C-S-m") 'end-of-line-newline)
(global-set-key (kbd "<tab>") 'completion-at-point)
(define-key minibuffer-local-map (kbd "<tab>") 'helm-select-action)

(global-set-key (kbd "C-.") 'avy-goto-word-1)
(global-set-key (kbd "C-:") 'avy-goto-line)
(global-set-key (kbd "C-M-:") 'avy-copy-line)

(global-set-key (kbd "C-c b p") 'show-file-name)

(global-set-key (kbd "C-c C-c") 'eval-defun)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(require 'visual-basic-mode)

    (add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;;  (require 'vbasense)

  ;;  (vbasense-config-default)
