;;; This file sets up global god mode configurations to make it "smart".
(require 'paredit)
(require 'god-mode)
(god-mode-all)

(dolist (mode '(helm-major-mode
                minibuffer-inactive-mode
                org-mode
                term-mode
                slime-repl-mode
                sldb-mode
                markdown-mode
                magit-status-mode
                pdf-view-mode
                dired-mode
                shell-mode
                inferior-python-mode))
  (add-to-list 'god-exempt-major-modes mode))

(global-set-key (kbd "<escape>") 'god-local-mode)
(setq god-mod-alist '((nil . "C-")
                      ("m" . "M-")
                      ("u" . "C-M-")))

(dolist (key '("q"))
  (define-key god-local-mode-map (kbd key) 'god-local-mode))
(dolist (key-pair '(("C-x C-h" "C-x h")
                    ("C-h C-k" "C-h k")
                    ("C-h C-v" "C-h v")
                    ("C-h C-f" "C-h f")))
  (add-to-list 'god-mode-translate-alist key-pair))
(use-package fundamental-mode
  :bind (("C-a" . backward-word)
         ("C-s" . isearch-forward)
         ("C-f" . forward-char)
         ("C-j" . newline)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ;; ("C-g" . keyboard-quit)
         ("C-i" . indent-for-tab-command)
         ("C-u" . god-local-mode)
         ("C-r" . query-replace)
         ("C-S-r" . replace-symbol)
         ("C-e" . forward-word)
         ("C-z" . undo)
         ("C-k" . kill-line)
         ("C-d" . delete-char)
         ("C-o" . other-window)
         ("C-l" . imenu-list-smart-toggle-and-search)
         ("C-v" . scroll-up)
         ("C-t" . goto-line)
         ("C-<tab>" . indent-for-tab-command)
         ("C-;" . comment-line)
         ("M-a" . beginning-of-visual-line)
         ("M-e" . end-of-visual-line)))

(defun god-mode-toggle ()
  (when (not (member major-mode god-exempt-major-modes))
    (if god-local-mode
        (god-local-mode -1)
      (god-local-mode))))

(defun god-mode-spc-and-toggle ()
  (interactive)
  (insert " ")
  (god-mode-toggle))

(setq god-mode-insert-and-exit-keys
      '("'" "," ":" "/" "-" "SPC" "*" "@" "_" "+" "=" "!" "#" "$" "%" "^" "&" "."))
(dolist (key god-mode-insert-and-exit-keys)
  (define-key god-local-mode-map (kbd key) 'god-mode-insert-and-exit))
(defun god-mode-insert-and-exit ()
  (interactive)
  (let ((last-key (ignore-errors (edmacro-format-keys (vector last-input-event)))))
    (insert (kbd last-key)))
  (god-local-mode -1))

(setq god-mode-insert-and-enter-keys '("<up>" "<down>" "<right>"))
(defun god-mode-insert-and-enter ()
  (interactive)
  (let ((last-key (ignore-errors (edmacro-format-keys (vector last-input-event)))))
    (when (not (member major-mode god-exempt-major-modes))
      (cond ((or (member last-key god-mode-insert-and-enter-keys)
                 (ignore-errors (and (string= "C-" (subseq last-key 0 2))
                                     (not (member (aref last-key 2)
                                                  '(?\  ?\g ?\o))))))
             (god-local-mode))))))

(add-hook 'post-command-hook 'god-mode-insert-and-enter)

(defmacro god-mode-execute-command-and-exit-mode (mode &rest key-command-list)
  (let ((key-function-list ()))
    (cl-loop for key-command in key-command-list
             for key = (car key-command)
             do (push `(,key . ,(gensym)) key-function-list)
             finally (setq key-function-list (nreverse key-function-list)))
    `(progn
       ,@(cl-loop for (key . symbol) in key-function-list
                  for key-command in key-command-list
                  for commands = (cdr key-command)
                  collect `(defun ,symbol ()
                             (interactive)
                             ,@commands
                             (god-local-mode -1)))       
       (use-package ,mode
         :bind (:map god-local-mode-map
                     ,@key-function-list)))))

(god-mode-execute-command-and-exit-mode
 paredit
 ("<backspace>" (paredit-backward-delete))
 ("<delete>" (paredit-forward-delete))
 ("(" (paredit-open-round))
 ("[" (paredit-open-square))
 ("{" (paredit-open-curly))
 ("<" (paredit-open-angled))
 ("\"" (paredit-doublequote))
 ("<left>" (left-char))
 (")" (paredit-close-round)))

(god-mode-execute-command-and-exit-mode
 smartparens
 ("(" (self-insert-command 1))
 ("[" (self-insert-command 1))
 ("{" (self-insert-command 1))
 ("<delete>" (sp-delete-char))
 ("<left>" (left-char)))
