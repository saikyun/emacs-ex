;; -*- lexical-binding: t -*-

(server-start)

(desktop-save-mode 1)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files\\Git\\mingw64\\bin"))
(setenv "PATH" (concat (getenv "PATH") ";C:\\Users\\u045862\\AppData\\Roaming"))
(add-to-list 'exec-path "C:/Users/u045862/AppData/Roaming")

;; (keyboard-translate ?\C-u ?\C-x)
;; (keyboard-translate ?\C-x ?\C-u)

;; (define-key key-translation-map [?\M-u] [?\M-x])
;; (define-key key-translation-map [?\M-x] [?\M-u])

(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)

(define-key key-translation-map [?\M-x] [?\M-u])
(define-key key-translation-map [?\M-u] [?\M-x])

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs-saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

(setq create-lockfiles nil)

(when (memq window-system '(mac ns))
(add-to-list 'exec-path "/usr/local/bin")
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/scripts")
  (let ((default-directory  "~/.emacs.d/scripts/"))
    (normal-top-level-add-subdirs-to-load-path))

  (require 'better-defaults)
  (require 'auto-indent-mode)

(global-linum-mode)

  (require 'highlight-sexps)

(defun paredit-expand ()
    (interactive)
    (paredit-backward-up)
    (let ((start (point)))
      (paredit-forward)
      (let ((end (point)))
        (goto-char start)
        (set-mark end))))

  (when (require 'so-long nil :noerror)
    (so-long-enable))

(require 'lua-mode)
;;  (require 'love-minor-mode)

  (define-key lua-mode-map (kbd "C-c C-c") (lambda () (interactive) (compile "love .")))
  (add-hook 'lua-mode-hook
            (lambda ()
              (if  (file-exists-p "main.lua")
                  (progn 
                    (set (make-local-variable 'compile-command) "love . ")
                    (add-to-list 'compilation-error-regexp-alist 'love t)
                    (add-to-list 'compilation-error-regexp-alist-alist
                                 '(love "^Error: Syntax error: \\(.*?\\):\\([0-9]+\\):.*$" 1 2) t))
                (set (make-local-variable 'compile-command)
                     (concat "lua " (file-name-nondirectory buffer-file-name))))))

(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))

(add-hook 'org-mode-hook '(lambda ()
                            (add-hook 'after-save-hook 'org-babel-tangle nil t)))

(defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; formats the buffer before saving
  ;;(add-hook 'before-save-hook 'tide-format-before-save)
  (require 'prettier-js)
  (add-hook 'web-mode-hook 'prettier-js-mode)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
;;  (flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'projectile)
  (projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-switch-project-action #'helm-projectile)

(require 'helm-swoop)
  (require 'helm-ag)
  (require 'projectile)

  (defun helm-ag-swoop (&optional $query)
    "Open buffers before `helm-multi-swoop-all'"
    (interactive)
    (setq helm-multi-swoop-query (helm-multi-swoop--get-query $query))
    (let* ((dir-path (projectile-project-root))
           (ag-cmd (format "ag -l '%s' %s"
                           (or $query "")
                           dir-path))
           (file-paths (split-string
                        (shell-command-to-string ag-cmd))))
      ;; helm-swoop only operates on buffer content -- so prepare some
      (dolist (file file-paths)
        (find-file-noselect file))
      (helm-multi-swoop--exec nil
                              :$query helm-multi-swoop-query
                              :$buflist (helm-multi-swoop--get-buffer-list))))

  (defun helm-ag-swoop-clojure (&optional $query)
    "Open buffers before `helm-multi-swoop-all'"
    (interactive)
    (setq helm-multi-swoop-query (helm-multi-swoop--get-query $query))
    (let* ((dir-path (projectile-project-root))
           (ag-cmd (format "ag -G '\.(clj|cljs|cljc)$' -l '%s' %s"
                           (or $query "")
                           dir-path))
           (file-paths (split-string
                        (shell-command-to-string ag-cmd))))
      ;; helm-swoop only operates on buffer content -- so prepare some
      (dolist (file file-paths)
        (find-file-noselect file))
      (helm-multi-swoop--exec nil
                              :$query helm-multi-swoop-query
                              :$buflist (helm-multi-swoop--get-buffer-list))))

;;  (setq projectile-indexing-method 'native)

(require 'clojure-mode)
  (require 'acrepl)
  (require 'acrepl-interaction)
  (require 'acrepl-shadow)

(global-set-key (kbd "C-<tab>") 'acrepl-auto-complete-dotdot-form)

  (require 'flycheck-clj-kondo)

  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (add-hook 'clojure-mode 'enable-paredit-mode)
;;  (remove-hook 'clojurescript-mode 'acrepl-interaction-enable)
  (defun thing ()
    (when (acrepl-shadow-conns-for-project)
              (acrepl-interaction-mode)))
  (add-hook 'clojurescript-mode-hook 'thing)
  (add-hook 'clojurec-mode-hook 'thing)
;;  (add-hook 'clojurescript-mode-hook 'flycheck-mode)

  (define-clojure-indent
    (alet 'defun)
    (mlet 'defun))


;;; auto-complete-etags.el --- Auto-completion source for etags

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Version: $Id: auto-complete-etags.el,v 0.2 2009/04/23 00:38:01 coldnew Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags)

;;; Code:

(provide 'auto-complete-etags)
(require 'auto-complete)
(eval-when-compile
  (require 'cl))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defvar ac-source-etags
  '((candidates . (lambda ()
                    (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3))
  "Source for etags.")


;;; auto-complete-etags.el ends here

(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)

  (setq cider-repl-use-pretty-printing t)
(require 'cider)

(define-key cider-repl-mode-map (kbd "RET") #'cider-repl-return)
(define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-newline-and-indent)

(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(control shift f3)] 'highlight-symbol-remove-all)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(require 'paredit)

  (add-hook 'paredit-mode-hook 'highlight-sexps-mode)

  (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "A-<backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "A-<delete>") 'paredit-forward-kill-word)

  (define-key paredit-mode-map (kbd "C-M-ä") 'kill-sexp)
  (define-key paredit-mode-map (kbd "C-M-å") 'backward-kill-sexp)

  (define-key paredit-mode-map (kbd "<C-left>") 'paredit-backward)
  (define-key paredit-mode-map (kbd "<C-right>") 'paredit-forward)
  (define-key paredit-mode-map (kbd "<C-down>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "<C-up>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "<C-M-down>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "<C-M-up>") 'paredit-backward-down)

  (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)

  (define-key paredit-mode-map (kbd "<C-M-left>") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "<C-M-right>") 'paredit-backward-barf-sexp)

  (define-key paredit-mode-map (kbd "C-c (") 'paredit-wrap-sexp)

  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)

  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
    return.")
  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
    open and indent an empty line between the cursor and the text.  Move the
    cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

  (defun start-of-sexp (pt)
    "Start of the s-expression surrounding PT."
    (save-excursion (cadr (syntax-ppss pt))))

  (defun end-of-sexp (pt)
    "End of s-expression that matches beginning point PT."
    (condition-case nil
        (scan-sexps pt 1)
      (error nil)))

  (defun expand-sexp ()
    (interactive)
    (let* ((pt (point))
           (prev (start-of-sexp pt)))
      (when prev
        (let ((next (end-of-sexp prev)))
          (when next
            (set-mark prev)
            (goto-char next))))))

  (defun eval--wrapping-sexp (arg f)
    (let* ((pt (point))
           (prev (start-of-sexp pt)))
      (when prev
        (let ((next (end-of-sexp prev)))
          (when next
            (goto-char next)
            (funcall f arg)
            (goto-char pt))))))

  (defun inside-end-of-sexp ()
    (interactive)
    (let* ((pt (point))
           (prev (start-of-sexp pt)))
      (when prev
        (let ((next (end-of-sexp prev)))
          (when next
            (goto-char next)
            (backward-char))))))

  (defun electrify-return-end-of-sexp (arg)
    (interactive "P")
    (inside-end-of-sexp)
    (electrify-return-if-match arg)
    (previous-line)
    (when (= (string-match-p "^\\s-*$" (thing-at-point 'line)) 0)
      (delete-indentation))
    (next-line))

  (defun return-end-of-sexp ()
    (interactive)
    (inside-end-of-sexp)
    (newline))

  (defun eval-wrapping-sexp (arg)
    (interactive "P")
    (eval--wrapping-sexp arg 'eval-last-sexp))

  (define-key paredit-mode-map (kbd "M-h") 'expand-sexp)

;;  (define-key paredit-mode-map (kbd "<return>") 'electrify-return-if-match)
  (define-key paredit-mode-map (kbd "<C-return>") 'end-of-line-newline)
(define-key paredit-mode-map (kbd "<S-return>") 'return-end-of-sexp)
;;  (define-key paredit-mode-map (kbd "<S-return>") 'electrify-return-end-of-sexp)

(require 'auto-indent-mode)
;;(auto-indent-global-mode)

(require 'inf-clojure)

  (require 'miracle)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  ;;(add-hook 'clojure-mode-hook 'miracle-interaction-mode)


  (defcustom arcadia-repl-port 37220
    "Port to connect to Arcadia repl.")

  (defun arcadia-repl ()
    "Attempts to connect to a running Arcadia instance over the Arcadia socket-repl."
    (interactive)
    (inf-clojure-connect "localhost" arcadia-repl-port))


  ;; inf-clojure's argslists eldoc support spams the Arcadia repl
  ;; and slows down emacs. This (removable) empty wrapper function is a
;; quick kludge to disable it.
  (defun arcadia-inf-clojure-eldoc-setup-wrapper (orig-fun &rest args))

  ;; Temporary hack that disables eldoc for inf-clojure.
  (advice-add 'inf-clojure-eldoc-setup :around #'arcadia-inf-clojure-eldoc-setup-wrapper)

  ;; (setq inf-clojure-repl-type 'clojure)

  (defun inf-clojure-change-to-ns (nsn)
    (interactive "sNamespace to go to: ")
    (inf-clojure--process-response
     (concat "(do (if-not (find-ns '" nsn ") (try (require '" nsn " :reload) (catch Exception e (ns " nsn " )))) (in-ns '" nsn "))")
     (inf-clojure-proc)))

  (defun inf-clojure-eval-in-ns (nsn command)
    (interactive "sNamespace to go to: \nsCommand: ")
    (inf-clojure--process-response
     (concat "(do (if-not (find-ns '" nsn ") (try (require '" nsn " :reload) (catch Exception e (ns " nsn " )))) (in-ns '" nsn ")" command ")")
     (inf-clojure-proc)))

  (defun inf-clojure-eval-in-ns-of-current-file (command)
    (interactive "sCommand: ")
    (if-let ((ns (clojure-find-ns)))
        (inf-clojure-eval-in-ns ns command)
      (inf-clojure--process-response command (inf-clojure-proc))))

  (defun inf-clojure-eval-last-sexp-in-ns-of-current-file ()
    (interactive)
    (if (not (equal (buffer-name (current-buffer)) inf-clojure-buffer))
        (inf-clojure-set-ns nil))
    (inf-clojure-eval-last-sexp))

  (defun go-to-csharp-definition ()
    "Go to the definition of a C# class from a clj-file."
    (interactive)
    (let ((pos (- (point) (line-beginning-position)))
          (beg (progn (re-search-forward "[[:space:]]")
                      (match-beginning 0)))
          (end (progn (backward-char)
                      (re-search-backward "[[:space:]]")
                      (match-end 0)))
                                          ;(beg (line-beginning-position))
                                          ;(end (line-end-position))
          )
      (copy-region-as-kill beg end)
      (find-file-other-window (concat
                               (cdr (assoc :project-root omnisharp--server-info))
                               "/temp-file.cs"))
      (erase-buffer)
      (let ((buffer-name (buffer-name)))
        (insert "using UnityEngine;

  public class Lul {
  ")
        (yank)
        (insert "
  }")
        (previous-line)
        (end-of-line)
        (backward-char)
        (omnisharp-go-to-definition))))

  (defvar get-interns-form
    "(defn ns-interns-of-aliases
  [ns]
  (->> ns
  ns-aliases
  (map #(vector (first %) (keys (ns-interns (second %)))))
  (into {})))

  (defn keys-to-prefixes
  [coll]
  (->> coll
  (map (fn [[k vs]] (map #(str k \"/\" %) vs)))
  flatten
  (map symbol)))")

  (defvar get-all-vars-form
    "(defn get-all-vars [ns]
    (map str (concat (keys (ns-map ns))
            (keys-to-prefixes (ns-interns-of-aliases ns)))))")

  (defun arcadia-get-public-members ()
    "Get the public members of a type."
    (interactive)
    (let* ((pos (point))
           (beg (progn (re-search-backward "(")
                       (match-beginning 0)))
           (end (progn (re-search-forward ")")
                       (match-end 0)))
           (identifier (buffer-substring beg end))
           (names (cdr (car (read-from-string identifier)))))
      (print names)
      (print (string-join (mapcar 'prin1-to-string names) " '"))
      (let ((res (car (read-from-string (inf-clojure--process-response
                                         (concat "(get-names (get-public-members (get-type-of-nested-member "
                                                 (string-join (mapcar 'prin1-to-string names) " '")
                                                 ")))")
                                         (inf-clojure-proc))))))
        (goto-char pos)
        (print res)
        res)))

  (defun helm-arcadia-show-members ()
    (interactive)
    (let ((chosen (helm :sources (helm-build-sync-source "test"
                                   :candidates (arcadia-get-public-members))
                        :buffer "*helm my command*")))
      (when chosen
        (insert " ")
        (insert chosen))))

  (defun inf-clojure-vars ()
    "Gets a list of the functions in the current namespace."
    (interactive)
    (let ((res (car (read-from-string (inf-clojure-eval-in-ns-of-current-file
                                       (concat "(do " get-all-vars-form " (get-all-vars *ns*))"))))))
      (sort (mapcar 'prin1-to-string res) 'string<)))

  (defun helm-arcadia-vars ()
    "List all vars."
    (interactive)
    (let ((chosen (helm :sources (helm-build-sync-source "Functions in namespace"
                                   :candidates (inf-clojure-vars))
                        :buffer "*helm my command*")))
      (when chosen
        (insert chosen))))

  (defun helm-arcadia-completion-at-point ()
    "Gets the last word and starts helm using the word as input, and all the functions available in the current inf-clojure process."
    (interactive)
    (let* ((pos (point))
           (beg (progn (re-search-backward "[[:space:]]\\|\n\\|(\\|^")
                       (forward-char)
                       (match-end 0)))
           (end (progn (re-search-forward "[[:space:]]\\|\n\\|)\\|$")
                       (backward-char)
                       (match-beginning 0)))
           (identifier (buffer-substring beg end))
           (parsed-id (car (read-from-string identifier))))

      (let ((chosen (helm :sources (helm-build-sync-source "Functions in namespace"
                                     :candidates (inf-clojure-vars))
                          :buffer "*helm my command*"
                          :input (prin1-to-string parsed-id))))
        (when chosen
          (kill-region beg end)
          (insert chosen)))))

  (defun inf-clojure-source-of-function (fun)
    "Gets the source for a function."
    (interactive "sSource of clojure function: ")
    (let ((res (inf-clojure-eval-in-ns-of-current-file
                (concat "(do (require '[clojure.repl :as temp-clojure-repl-ns]) (temp-clojure-repl-ns/source "
                        fun
                        "))"))))
      (switch-to-buffer-other-window "*inf-clojure-source*")
      (erase-buffer)
      (clojure-mode)
      (insert res)
      (goto-char 0)
      (while (re-search-forward "
" nil t)
        (replace-match ""))))

  (defun helm-inf-clojure-source-of-function ()
    (interactive)
    (let ((chosen (helm :sources (helm-build-sync-source "Functions in namespace"
                                   :candidates (inf-clojure-get-available-functions))
                        :buffer "*helm my command*")))
      (when chosen
        (inf-clojure-source-of-function chosen))))

  (defun inf-clojure-source-of-function-at-point ()
    (interactive)
    (let* ((pos (point))
           (beg (progn (re-search-backward "[[:space:]]\\|\n\\|(")
                       (forward-char)
                       (match-end 0)))
           (end (progn (re-search-forward "[[:space:]]\\|\n\\|)")
                       (backward-char)
                       (match-beginning 0)))
           (identifier (buffer-substring beg end))
           (parsed-id (car (read-from-string identifier))))
      (inf-clojure-source-of-function identifier)))

   (eval-after-load 'clojure-mode
     '(progn
  ;;      (define-key clojure-mode-map (kbd "<tab>") 'helm-arcadia-completion-at-point)
  ;;      (define-key clojure-mode-map (kbd "M-.") 'inf-clojure-source-of-function-at-point)

      ;;  (define-key clojure-mode-map (kbd "C-x C-e") 'inf-clojure-eval-last-sexp-in-ns-of-current-file)
        (define-key clojure-mode-map (kbd "C-ö C-m") 'miracle)
        (define-key clojure-mode-map (kbd "C-ö m") 'miracle)
  ;;      (define-key clojure-mode-map (kbd "C-M-x") 'inf-clojure-eval-defun-in-ns-of-current-file))
        ))

(require 'clojure-mode)

      (add-hook 'clojure-mode-hook 'paredit-mode)
      (add-hook 'clojure-mode-hook 'auto-indent-mode)

      (require 'inf-clojure)

        (define-key inf-clojure-minor-mode-map (kbd "<tab>") 'helm-arcadia-completion-at-point)
      (define-key inf-clojure-minor-mode-map (kbd "M-.") 'inf-clojure-source-of-function-at-point)

    (define-key inf-clojure-mode-map (kbd "<M-return>") 'comint-send-input)

                                              ;(define-key inf-clojure-minor-mode-map (kbd "\C-x\C-e") 'inf-clojure-eval-last-sexp-in-ns-of-current-file)


      (setq inf-clojure-log-activity nil)

      (defun cljs-figwheel-repl ()
        (interactive)
        (inf-clojure "lein figwheel"))

    ;  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

      ;; transpose sexp
      ;; kill sexp
      ;; next/prev sexp
      ;; into/out sexp back/forward

      (fset 'inf-clojure-load-current-file-no-prompt
            (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217845 105 110 102 32 99 108 111 106 117 114 101 45 108 111 97 100 45 105 backspace 102 105 108 101 return return] 0 "%d")) arg)))

      ;; (eval-after-load 'clojure-mode
      ;;   '(progn
      ;;      (define-key clojure-mode-map (kbd "C-:") nil)
      ;;      (define-key clojure-mode-map (kbd "C-c C-j") 'cljs-figwheel-repl)
      ;;      (define-key clojure-mode-map (kbd "C-c C-p") 'arcadia-repl)
      ;;      (define-key clojure-mode-map (kbd "C-c C-z") 'inf-clojure-switch-to-repl)
      ;;      (define-key clojure-mode-map (kbd "C-å") 'inf-clojure-eval-defun)
      ;;      (define-key clojure-mode-map (kbd "C-c C-l") 'inf-clojure-load-current-file-no-prompt)
      ;;      (define-key clojure-mode-map (kbd "C-S-c C-l") 'inf-clojure-eval-buffer)
      ;;      (define-key clojure-mode-map (kbd "C-x C-ö") 'inf-clojure-eval-defun-and-go)))

  (define-key inf-clojure-minor-mode-map (kbd "C-c C-l") 'inf-clojure-load-current-file-no-prompt)

      (add-hook 'inf-clojure-mode-hook 'paredit-mode)

      (provide 'clojure-conf)

(require 'multi-line)
(global-set-key (kbd "C-c d") 'multi-line)

(add-hook 'python-mode-hook 'anaconda-mode)

(require 'magit)

(global-magit-file-mode)

(add-hook 'rust-mode 'racer-mode)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(defun eval-buffer-with-feedback ()
  (interactive)
  (eval-buffer)
  (message "Evaluated buffer."))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer-with-feedback)

(global-set-key (kbd "M-m") 'delete-indentation)
(global-set-key (kbd "C-S-z") 'revert-buffer)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(global-set-key (kbd "C-S-m") 'end-of-line-newline)
(global-set-key (kbd "C-<return>") 'end-of-line-newline)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(define-key minibuffer-local-map (kbd "<tab>") 'helm-select-action)

(global-set-key (kbd "C-.") 'avy-goto-word-1)
(global-set-key (kbd "C-:") 'avy-goto-line)
(global-set-key (kbd "C-M-:") 'avy-copy-line)

(global-set-key (kbd "C-c b p") 'show-file-name)

(global-set-key (kbd "<M-delete>") 'kill-word)

  (global-set-key (kbd "<A-left>") 'backward-word)
  (global-set-key (kbd "<A-right>") 'forward-word)
(global-set-key (kbd "<A-backspace>") 'backward-kill-word)
(global-set-key (kbd "<A-kp-delete>") 'kill-word)
(global-set-key (kbd "<A-delete>") 'kill-word)

(global-set-key (kbd "C-c C-c") 'eval-defun)
(global-set-key (kbd "C-å") 'paredit-expand)

(global-set-key (kbd "<tab>") 'helm-company)
(define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop-without-pre-input)
(global-set-key (kbd "C-c C-M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching, st
(setq helm-swoop-use-fuzzy-match nil)

;; If you would like to use migemo, enable helm's migemo feature
;; (helm-migemo-mode 1)

(require 'visual-basic-mode)

    (add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;;  (require 'vbasense)

  ;;  (vbasense-config-default)

(defun git-pull ()
    (interactive)
    (shell-command "git pull"))

  (defun git-push ()
    (interactive)
    (shell-command "git push"))

;;  (global-set-key (kbd "C-c C-g") 'git-pull)
  (global-set-key (kbd "C-c C-p") 'git-push)

(defun end-of-line-newline ()
  (interactive)
  (end-of-line)
  (newline))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;;; -*- lexical-binding: t -*-
(require 'company)

(with-eval-after-load "company"
  ;; everywhere
  (global-company-mode)
  ;;
  ;;(global-set-key (kbd "<tab>") #'helm-company)
  (global-set-key (kbd "M-TAB") #'company-complete)

(defun miracle-eval-wrapping-sexp ()
(interactive)
  (let* ((pt (point))
         (prev (start-of-sexp pt)))
    (when prev
      (let ((next (end-of-sexp prev)))
        (when next
          (miracle-eval-region prev next))))))


(require 'acrepl)
(defun acrepl-eval-wrapping-sexp ()
(interactive)
(let* ((pt (point))
         (prev (start-of-sexp pt)))
    (when prev
      (let ((next (end-of-sexp prev)))
        (when next
          (acrepl-send-region prev next))))))

(global-set-key (kbd "C-c C-ö") 'acrepl-eval-wrapping-sexp)

(define-key miracle-interaction-mode-map (kbd "C-c C-g") 'miracle-eval-wrapping-sexp)
(define-key miracle-interaction-mode-map (kbd "C-c C-ö") 'miracle-saves-in-defun)
(define-key miracle-interaction-mode-map (kbd "C-c C-f C-s") 'miracle-instrument-ns)
(define-key miracle-interaction-mode-map (kbd "C-c C-f C-f") 'miracle-instrument-defun)
(define-key miracle-interaction-mode-map (kbd "C-c C-f C-p") 'miracle-unstrument-defun)
(define-key miracle-interaction-mode-map (kbd "C-c C-f C-x") 'miracle-unstrument-ns)
(define-key miracle-interaction-mode-map (kbd "C-c C-f C-l") 'miracle-load-defun)
  ;; for once have escape key cancel things in emacs...
  (define-key company-active-map (kbd "ESC") 'company-abort))

(with-eval-after-load "miracle"
  (defun miracle-eval-string (s callback)
    (miracle-send-eval-string
     s
     (lambda (response)
       (miracle-dbind-response response (id value status)
                               (when (member "done" status)
                                 (remhash id miracle-requests))
                               (when value
                                 (funcall callback nil value))))))

  (defun miracle-get-completions (word callback)
    (interactive)
    (miracle-eval-string
     (format "(do (require '[%s]) (%s/completions \"%s\"))"
             "complete.core" "complete.core" word)
     (lambda (err s)
       (progn
         ;; XXX
         (message (format "received str: %s" s))
         (message (format "err: %s" err))
         (when (not err)
           (funcall callback (read-from-whole-string s)))))))

  (defun company-miracle (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-miracle))
      (prefix (and (or ;;(eq major-mode 'clojurec-mode)
                       ;;(eq major-mode 'clojure-mode)
                       (eq major-mode 'miracle-mode))
                   (get-buffer "*miracle-connection*")
                   (substring-no-properties (company-grab-symbol))))
      (candidates (lexical-let ((arg (substring-no-properties arg)))
                    (cons :async (lambda (callback)
                                   (miracle-get-completions arg callback)))))))

  ;; XXX: problems w/o the following when invoking company-grab-symbol
  (setq cider-mode nil)

  (add-to-list 'company-backends 'company-miracle)

  )

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(fset 'start-medgivande-api
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 6 126 47 106 111 98 98 right 105 115 116 right 109 101 100 right down down 134217829 121 97 114 110 32 119 97 116 99 104 45 116 115 return 24 50 67108914 134217845 101 115 104 101 108 108 return 121 97 114 110 32 115 116 97 114 116 44 backspace 45 108 111 99 97 108 return] 0 "%d")) arg)))

(fset 'start-medgivande-portal
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([21 134217845 101 115 104 101 108 108 return 99 100 return 99 100 32 106 111 98 98 tab 105 115 116 tab 109 101 100 tab tab tab return 121 97 114 110 32 119 97 116 99 104 45 116 115 111 backspace return 24 51 21 134217845 101 115 104 101 108 108 return 121 97 114 110 32 115 116 97 114 116 return] 0 "%d")) arg)))
