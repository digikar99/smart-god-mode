;; smart-god-mode.el --- Smart-God-like command entering minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns
;; Copyright (C) 2013 Fabián Ezequiel Gallina
;; Copyright (C) 2020 Akhil Wali

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.16.0
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README.md.

;;; Code:

(require 'cl-lib)

(add-hook 'after-change-major-mode-hook 'smart-god-mode-maybe-activate)

(defvar smart-god-local-mode-paused nil)
(make-variable-buffer-local 'smart-god-local-mode-paused)

(defcustom smart-god-mod-alist
  '((nil . "C-")
    ("g" . "M-")
    ("G" . "C-M-"))
  "List of keys and their associated modifer."
  :group 'smart-god
  :type '(alist))

(defcustom smart-god-literal-key
  "SPC"
  "The key used for literal interpretation."
  :group 'smart-god
  :type 'string)

(defcustom smart-god-exempt-major-modes
  '(dired-mode
    grep-mode
    vc-annotate-mode
    git-commit-mode  ; For versions prior to Magit 2.1.0
    magit-popup-mode)
  "List of major modes that should not start with `smart-god-local-mode' enabled."
  :group 'smart-god
  :type '(function))

(defcustom smart-god-exempt-predicates
  (list #'smart-god-exempt-mode-p
        #'smart-god-comint-mode-p
        #'smart-god-git-commit-mode-p
        #'smart-god-view-mode-p
        #'smart-god-special-mode-p)
  "List of predicates checked before enabling `smart-god-local-mode'.
All predicates must return nil for `smart-god-local-mode' to start."
  :group 'smart-god
  :type '(repeat function))

(defcustom smart-god-mode-exit-and-do-keys
  nil
  "List of keys that when pressed cause `smart-god-local-mode' to stop. Once `smart-god-local-mode' is disabled, whatever the command key stood for is run. This variable must be set using `smart-god-mode-set-exit-and-do-keys'."
  :group 'smart-god
  :type 'list)

(defcustom smart-god-mode-do-and-enter-keys
  nil
  "When these keys are pressed, `smart-god-local-mode' is enabled if it was disabled, after doing the action that the key was supposed to do is done."
  :group 'smart-god
  :type 'list)

(defcustom smart-god-mode-auto-enter-on-ctrl-keys
  nil
  "When t, automatically enables `smart-god-local-mode' when Ctrl- keys are used, and the keys are not in `smart-god-mode-auto-enter-exempt-keys'."
  :group 'smart-god
  :type 'bool)

(defcustom smart-god-mode-auto-enter-on-ctrl-exempt-keys
  '("C-g")
  "See `smart-god-mode-auto-enter-on-ctrl-keys'. Keys you may want to put in this list include keys for `keyboard-quit' and `other-window'."
  :group 'smart-god
  :type 'list)

(defun smart-god-mode-do-and-enter ()
  (interactive)
  (let ((last-key (ignore-errors (edmacro-format-keys (vector last-input-event)))))
    ;; (print last-key)
    (when (and smart-god-global-mode
               (not (member major-mode smart-god-exempt-major-modes))
               (or (member last-key smart-god-mode-do-and-enter-keys)
                   (and smart-god-mode-auto-enter-on-ctrl-keys
                        (< 1 (length last-key))
                        (string= "C-" (subseq last-key 0 2))
                        (not (member last-key
                                     smart-god-mode-auto-enter-on-ctrl-exempt-keys)))))
      (smart-god-local-mode))))
(add-hook 'post-command-hook 'smart-god-mode-do-and-enter)


(defvar smart-god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'smart-god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'smart-god-mode-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") nil))
    map))

;;;###autoload
(define-minor-mode smart-god-local-mode
  "Minor mode for running commands."
  nil " Smart-God" smart-god-local-mode-map
  (if smart-god-local-mode
      (run-hooks 'smart-god-mode-enabled-hook)
    (run-hooks 'smart-god-mode-disabled-hook)))

(defun smart-god-mode-exit-and-do ()
  (interactive)
  (smart-god-local-mode -1)
  (let ((last-key (ignore-errors (edmacro-format-keys (vector last-input-event)))))
    (when last-key
      ;; (print last-key)
      (if-let (binding (key-binding (kbd last-key)))
          (call-interactively binding)        
        (when (string= last-key "<backspace>")
          (setq last-key "DEL")
          (when-let (binding (key-binding (kbd last-key)))
            (call-interactively binding)))))
    ;; (print last-key)
    ;; (print (key-binding last-key))
    ))

(defun smart-god-mode-set-exit-and-do-keys (new-value)
  (setq smart-god-mode-exit-and-do-keys new-value)
  (let ((map smart-god-local-mode-map))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'smart-god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (let ((vec (vector i)))
          (when (eq (lookup-key smart-god-local-mode-map vec)
                    'smart-god-mode-exit-and-do)
            (define-key map vec 'smart-god-mode-self-insert)))
        (setq i (1+ i))))
    (dolist (key smart-god-mode-exit-and-do-keys)
      (define-key map (kbd key) 'smart-god-mode-exit-and-do)))
  t)

(defun smart-god-local-mode-pause ()
  "Pause `smart-god-local-mode' if it is enabled.
See also `smart-god-local-mode-resume'."
  (when smart-god-local-mode
    (smart-god-local-mode -1)
    (setq smart-god-local-mode-paused t)))

(defun smart-god-local-mode-resume ()
  "Re-enable `smart-god-local-mode'.
If it was not active when `smart-god-local-mode-pause' was called, nothing happens."
  (when (bound-and-true-p smart-god-local-mode-paused)
    (setq smart-god-local-mode-paused nil)
    (smart-god-local-mode 1)))

(defvar smart-god-global-mode nil
  "Enable `smart-god-local-mode' on all buffers.")

(defvar smart-god-literal-sequence nil
  "Activated after `smart-god-literal-key' is pressed in a command sequence.")

;;;###autoload
(defun smart-god-mode ()
  "Toggle global `smart-god-local-mode'."
  (interactive)
  (setq smart-god-global-mode (not smart-god-global-mode))
  (if smart-god-global-mode
      (smart-god-local-mode 1)
    (smart-god-local-mode -1)))

;;;###autoload
(defun smart-god-mode-all ()
  "Toggle `smart-god-local-mode' in all buffers."
  (interactive)
  (let ((new-status (if (bound-and-true-p smart-god-local-mode) -1 1)))
    (setq smart-god-global-mode t)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (smart-god-mode-activate new-status)))
          (buffer-list))
    (setq smart-god-global-mode (= new-status 1))))

(defun smart-god-mode-maybe-universal-argument-more ()
  "If `smart-god-local-mode' is enabled, call `universal-argument-more'."
  (interactive)
  (if smart-god-local-mode
      (call-interactively #'universal-argument-more)
    (let ((binding (smart-god-mode-lookup-command "u")))
      (if (commandp binding t)
          (call-interactively binding)
        (execute-kbd-macro binding)))))

(define-key universal-argument-map (kbd "u")
  #'smart-god-mode-maybe-universal-argument-more)

(defun smart-god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (smart-god-mode-lookup-key-sequence initial-key)))
    (when (smart-god-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq smart-god-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun smart-god-mode-upper-p (char)
  "Check if CHAR is an upper case character."
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun smart-god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given KEY (or the next keypress, if KEY is nil).
This function sometimes recurses.
KEY-STRING-SO-FAR should be nil for the first call in the sequence."
  (interactive)
  (let ((sanitized-key
         (smart-god-mode-sanitized-key-string
          (or key (read-event key-string-so-far)))))
    (smart-god-mode-lookup-command
     (smart-god-key-string-after-consuming-key sanitized-key key-string-so-far))))

(defun smart-god-mode-sanitized-key-string (key)
  "Convert any special events in KEY to textual representation."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (S-left "S-<left>")
    (S-right "S-<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (t (char-to-string key))))

(defun smart-god-key-string-after-consuming-key (key key-string-so-far)
  "Interpret smart-god-mode special keys for KEY.
Consumes more keys if appropriate.
Appends to key sequence KEY-STRING-SO-FAR."
  (let ((key-consumed t) (next-modifier "") next-key)
    (message key-string-so-far)
    (cond
     ;; Don't check for smart-god-literal-key with the first key
     ((and key-string-so-far (string= key smart-god-literal-key))
      (setq smart-god-literal-sequence t))
     (smart-god-literal-sequence
      (setq key-consumed nil))
     ((and (stringp key) (assoc key smart-god-mod-alist))
      (setq next-modifier (cdr (assoc key smart-god-mod-alist))))
     (t
      (setq key-consumed nil
            next-modifier (cdr (assoc nil smart-god-mod-alist)))))
    (setq next-key
          (if key-consumed
              (smart-god-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (when (and (= (length next-key) 1)
               (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
               ;; If C- is part of the modifier, S- needs to be given
               ;; in order to distinguish the uppercase from the
               ;; lowercase bindings. If C- is not in the modifier,
               ;; then emacs natively treats uppercase differently
               ;; from lowercase, and the S- modifier should not be
               ;; given
               (string-prefix-p "C-" next-modifier))
      (setq next-modifier (concat next-modifier "S-")))
    (if key-string-so-far
        (concat key-string-so-far " " next-modifier next-key)
      (concat next-modifier next-key))))

(defun smart-god-mode-lookup-command (key-string)
  "Execute extended keymaps in KEY-STRING, or call it if it is a command."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           binding)
          ((keymapp binding)
           (smart-god-mode-lookup-key-sequence nil key-string))
          (:else
           (error "Smart-God: Unknown key binding for `%s`" key-string)))))

;;;###autoload
(defun smart-god-mode-maybe-activate (&optional status)
  "Activate `smart-god-local-mode' on individual buffers when appropriate.
STATUS is passed as an argument to `smart-god-mode-activate'."
  (when (not (minibufferp))
    (smart-god-mode-activate status)))

(defun smart-god-mode-activate (&optional status)
  "Activate `smart-god-local-mode' on individual buffers when appropriate.
STATUS is passed as an argument to `smart-god-local-mode'."
  (when (and smart-god-global-mode
             (smart-god-passes-predicates-p))
    (smart-god-local-mode (if status status 1))))

(defun smart-god-exempt-mode-p ()
  "Return non-nil if `major-mode' is exempt.
Members of the `smart-god-exempt-major-modes' list are exempt."
  (memq major-mode smart-god-exempt-major-modes))

(defun smart-god-mode-child-of-p (major-mode parent-mode)
  "Return non-nil if MAJOR-MODE is derived from PARENT-MODE."
  (let ((parent (get major-mode 'derived-mode-parent)))
    (cond ((eq parent parent-mode))
          ((not (null parent))
           (smart-god-mode-child-of-p parent parent-mode))
          (t nil))))

(defun smart-god-comint-mode-p ()
  "Return non-nil if `major-mode' is derived from `comint-mode'."
  (smart-god-mode-child-of-p major-mode 'comint-mode))

(defun smart-god-special-mode-p ()
  "Return non-nil if `major-mode' is special or derived from `special-mode'."
  (eq (get major-mode 'mode-class) 'special))

(defun smart-god-view-mode-p ()
  "Return non-nil if variable `view-mode' is non-nil in current buffer."
  view-mode)

(defun smart-god-git-commit-mode-p ()
  "Return non-nil if a `git-commit-mode' will be enabled in this buffer."
  (and (bound-and-true-p global-git-commit-mode)
       ;; `git-commit-filename-regexp' defined in the same library as
       ;; `global-git-commit-mode'.  Expression above maybe evaluated
       ;; to true because of autoload cookie.  So we perform
       ;; additional check.
       (boundp 'git-commit-filename-regexp)
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)))

(defun smart-god-passes-predicates-p ()
  "Return non-nil if all `smart-god-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds smart-god-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

(provide 'smart-god-mode)

;;; smart-god-mode.el ends here
