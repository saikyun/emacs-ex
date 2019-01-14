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

(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)

(define-key key-translation-map [?\M-x] [?\M-u])
(define-key key-translation-map [?\M-u] [?\M-x])

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

(auto-indent-global-mode)

(global-linum-mode)

(require 'lua-mode)
(require 'love-minor-mode)

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
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

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

  (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-return) 
  (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-newline-and-indent)

(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(control shift f3)] 'highlight-symbol-remove-all)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(require 'paredit)
(define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
(define-key paredit-mode-map (kbd "A-<backspace>") 'paredit-backward-kill-word)

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

(require 'auto-indent-mode)
(auto-indent-global-mode)

(require 'inf-clojure)

(require 'miracle)
(add-hook 'clojure-mode-hook 'clojure-enable-miracle)
(add-hook 'miracle-mode-hook 'paredit-mode)


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

(setq inf-clojure-repl-type 'clojure)

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
    (while (re-search-forward "" nil t)
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

;; (eval-after-load 'clojure-mode
;;   '(progn
;;      (define-key clojure-mode-map (kbd "<tab>") 'helm-arcadia-completion-at-point)
;;      (define-key clojure-mode-map (kbd "M-.") 'inf-clojure-source-of-function-at-point)

;;      (define-key clojure-mode-map (kbd "C-x C-e") 'inf-clojure-eval-last-sexp-in-ns-of-current-file)
;;      (define-key clojure-mode-map (kbd "C-M-x") 'inf-clojure-eval-defun-in-ns-of-current-file)))

(require 'clojure-mode)

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'auto-indent-mode)

  (require 'inf-clojure)

    (define-key inf-clojure-minor-mode-map (kbd "<tab>") 'helm-arcadia-completion-at-point)
  (define-key inf-clojure-minor-mode-map (kbd "M-.") 'inf-clojure-source-of-function-at-point)

(define-key inf-clojure-mode-map (kbd "<M-return>") 'comint-send-input)

                                          ;(define-key inf-clojure-minor-mode-map (kbd "\C-x\C-e") 'inf-clojure-eval-last-sexp-in-ns-of-current-file)


;  (setq inf-clojure-log-activity nil)

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

  (add-hook 'inf-clojure-mode-hook 'paredit-mode)

  (provide 'clojure-conf)

(require 'multi-line)
(global-set-key (kbd "C-c d") 'multi-line)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(global-set-key (kbd "M-m") 'delete-indentation)
(global-set-key (kbd "C-S-z") 'revert-buffer)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(global-set-key (kbd "C-S-m") 'end-of-line-newline)
(global-set-key (kbd "C-<return>") 'end-of-line-newline)
(global-set-key (kbd "<tab>") 'completion-at-point)

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

(global-set-key (kbd "C-c C-c") 'eval-defun)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(require 'visual-basic-mode)

    (add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;;  (require 'vbasense)

  ;;  (vbasense-config-default)

(defun end-of-line-newline ()
  (interactive)
  (end-of-line)
  (newline))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
