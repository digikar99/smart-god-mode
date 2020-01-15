;;; This file sets up global god mode configurations to make it "smart".
(require 'paredit)
(require 'god-mode)
(god-mode-all)

(dolist (mode '(helm-major-mode
                minibuffer-inactive-mode
                org-mode
                term-mode
                slime-repl-mode
                sldb-mode))
  (add-to-list 'god-exempt-major-modes mode))

(global-set-key (kbd "<escape>") 'god-local-mode)
(setq god-mod-alist '((nil . "C-")
                      ("m" . "M-")
                      ("t" . "C-M-")))

(dolist (key '("q" "u"))
  (define-key god-local-mode-map (kbd key) 'god-local-mode))
(dolist (key-pair '(("C-x C-h" "C-x h")))
  (add-to-list 'god-mode-translate-alist key-pair))
(use-package fundamental-mode
  :bind (("C-a" . beginning-of-visual-line)
         ("C-s" . isearch-forward)
         ("C-f" . forward-char)
         ("C-j" . newline)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ;; ("C-g" . keyboard-quit)
         ("C-i" . indent-for-tab-command)
         ("C-u" . god-local-mode)
         ("C-r" . replace-symbol)
         ("C-e" . end-of-visual-line)
         ("C-z" . undo)
         ("C-k" . kill-line)
         ("C-d" . delete-char)
         ("C-o" . other-window)
         ("C-l" . imenu-list-smart-toggle-and-search)
         ("C-v" . scroll-up)         
         ("C-<tab>" . indent-for-tab-command)
         ("C-;" . comment-line)
         ("RET" . god-mode-newline-and-toggle)
         ("SPC" . god-mode-spc-and-toggle)))

(defun god-mode-newline-and-toggle (&optional arg interactive)
  "Like newline, but toggles god-mode."
  (interactive "*P\np")
  (barf-if-buffer-read-only)
  ;; Call self-insert so that auto-fill, abbrev expansion etc. happens.
  ;; Set last-command-event to tell self-insert what to insert.
  (let* ((was-page-start (and (bolp) (looking-at page-delimiter)))
         (beforepos (point))
         (last-command-event ?\n)
         ;; Don't auto-fill if we have a numeric argument.
         (auto-fill-function (if arg nil auto-fill-function))
         (arg (prefix-numeric-value arg))
         (postproc
          ;; Do the rest in post-self-insert-hook, because we want to do it
          ;; *before* other functions on that hook.
          (lambda ()
            ;; We are not going to insert any newlines if arg is
            ;; non-positive.
            (or (and (numberp arg) (<= arg 0))
                (cl-assert (eq ?\n (char-before))))
            ;; Mark the newline(s) `hard'.
            (if use-hard-newlines
                (set-hard-newline-properties
                 (- (point) arg) (point)))
            ;; If the newline leaves the previous line blank, and we
            ;; have a left margin, delete that from the blank line.
            (save-excursion
              (goto-char beforepos)
              (beginning-of-line)
              (and (looking-at "[ \t]$")
                   (> (current-left-margin) 0)
                   (delete-region (point)
                                  (line-end-position))))
            ;; Indent the line after the newline, except in one case:
            ;; when we added the newline at the beginning of a line which
            ;; starts a page.
            (or was-page-start
                (move-to-left-margin nil t)))))
    (unwind-protect
        (if (not interactive)
            ;; FIXME: For non-interactive uses, many calls actually
            ;; just want (insert "\n"), so maybe we should do just
            ;; that, so as to avoid the risk of filling or running
            ;; abbrevs unexpectedly.
            (let ((post-self-insert-hook (list postproc)))
              (self-insert-command arg))
          (unwind-protect
              (progn
                (add-hook 'post-self-insert-hook postproc nil t)
                (self-insert-command arg))
            ;; We first used let-binding to protect the hook, but that
            ;; was naive since add-hook affects the symbol-default
            ;; value of the variable, whereas the let-binding might
            ;; only protect the buffer-local value.
            (remove-hook 'post-self-insert-hook postproc t)))
      (cl-assert (not (member postproc post-self-insert-hook)))
      (cl-assert (not (member postproc (default-value 'post-self-insert-hook))))))
  (god-mode-toggle)
  nil)

(defun god-mode-toggle ()
  (when (not (member major-mode god-exempt-major-modes))
    (if god-local-mode
        (god-local-mode -1)
      (god-local-mode))))

(defun god-mode-spc-and-toggle ()
  (interactive)
  (insert-string " ")
  (god-mode-toggle))

(setq god-mode-insert-and-exit-keys
      '("'" "," ":" "/" "-" "*" "@" "_" "+" "=" "!" "#" "$" "%" "^" "&" "."))
(dolist (key god-mode-insert-and-exit-keys)
  (define-key god-local-mode-map (kbd key) 'god-mode-insert-and-exit))
(defun god-mode-insert-and-exit ()
  (interactive)
  (let ((last-key (ignore-errors (edmacro-format-keys (vector last-input-event)))))
    (insert-string (kbd last-key)))
  (god-local-mode -1))

(setq god-mode-insert-and-enter-keys '(")" "]" "}" "<up>" "<down>" "<right>"))
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
         :bind (("(" . paredit-insert-9)
                ("C-(" . paredit-insert-9)
                ("9" . paredit-open-round)
                :map god-local-mode-map
                ,@key-function-list)))))

(god-mode-execute-command-and-exit-mode fundamental)

(defun paredit-insert-9 ()
  (interactive)
  (insert-char ?\9))

(god-mode-execute-command-and-exit-mode
 paredit
 ("<backspace>" (paredit-backward-delete))
 ("<delete>" (paredit-forward-delete))
 ("(" (paredit-insert-9))
 ("[" (paredit-open-square))
 ("{" (paredit-open-curly))
 ("<" (paredit-open-angled))
 ("\"" (paredit-doublequote))
 ("<left>" (left-char))
 ("9" (paredit-open-round)))
