# smart-god-mode

>This is a derivative of [god-mode](https://github.com/chrisdone/god-mode).

I'll merge into the main [god-mode](https://github.com/chrisdone/god-mode) some other time - I am still required to set up tests.

An example [use-package](https://www.masteringemacs.org/article/spotlight-use-package-a-declarative-configuration-tool) configuration for setting up the smart-god-mode for your own self is given below:


```emacs-lisp
(use-package smart-god-mode
  :init
  (load "~/.emacs.d/my-packages/smart-god-mode.el")
  (require 'paredit)
	(require 'smartparens)
  (defun smart-god-mode-exit-and-open-round ()
    (interactive)
    (if paredit-mode
        (paredit-open-round)
      (insert "()")
      (backward-char))
    (smart-god-local-mode -1))
  (defun smart-god-mode-exit-and-open-square ()
    (interactive)
    (smart-god-local-mode -1)
    (if paredit-mode
        (paredit-open-square)
      (insert "[]")
      (backward-char)))
  (defun smart-god-mode-exit-and-open-curly ()
    (interactive)
    (smart-god-local-mode -1)
    (if paredit-mode
        (paredit-open-curly)
      (insert "{}")
      (backward-char)))
  (defun smart-god-mode-exit-and-backward-delete
      (&optional argument)
    (interactive)
    (smart-god-local-mode -1)
    (if paredit-mode
        (paredit-backward-delete argument)
      (call-interactively 'sp-backward-delete-char)))
  (defun smart-god-mode-exit-and-doublequote ()
    (interactive) 
    (smart-god-local-mode -1) 
    (if paredit-mode
        (paredit-doublequote) 
      (insert "\"\"")
      (backward-char)))
  (define-key input-decode-map [?\C-\[] (kbd "<C-[>")) ; another hack for C-[ ESC equivalence
  :bind (
         :map paredit-mode-map
         ("(" . smart-god-mode-exit-and-open-round) 
         ("C-(" . smart-god-mode-exit-and-open-round)
         ("[" . smart-god-mode-exit-and-open-square) 
         ("<C-[>" . smart-god-mode-exit-and-open-square)
         ("{" . smart-god-mode-exit-and-open-curly)
         ("C-{" . smart-god-mode-exit-and-open-curly)
         ("<backspace>" . smart-god-mode-exit-and-backward-delete)
         ("\"" . smart-god-mode-exit-and-doublequote)
         ("C-\"" . smart-god-mode-exit-and-doublequote)

         :map smartparens-strict-mode-map
         ("C-(" . smart-god-mode-exit-and-open-round)
         ("<C-[>" . smart-god-mode-exit-and-open-square)
         ("C-{" . smart-god-mode-exit-and-open-curly)
         ("C-\"" . smart-god-mode-exit-and-doublequote)
         ("<backspace>" . smart-god-mode-exit-and-backward-delete))
  :config
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
    (add-to-list 'smart-god-exempt-major-modes mode))
  (setq smart-god-mod-alist '((nil . "C-")
                              ("m" . "M-")
                              ("u" . "C-M-")))
  (dolist (key '("q"))
    (define-key smart-god-local-mode-map (kbd key) 'smart-god-local-mode))
  (smart-god-mode-set-exit-and-do-keys
   '("'" "," ":" "/" "-" "SPC" "*" "@" "_" "+" "=" "!" "#" "$"
     "%" "^" "&" "." "`" "~" "<left>"))
  (setq smart-god-mode-do-and-enter-keys '("<up>" "<down>" "<right>" ")" "]" "}")
        smart-god-mode-auto-enter-on-ctrl-keys t
        smart-god-mode-auto-enter-on-ctrl-exempt-keys '("C-g" "C-o"))
  (smart-god-mode-all))
```  

Our basic worry is when to get in and out of the god-mode. This is handled by several variables and functions:

- smart-god-mode-set-exit-and-do-keys - sets smart-god-mode-exit-and-do-keys; basically sets up various bindings inside smart-god-local-mode-map, that, well, disables (exits) [hmm... we can do some renaming] the god-mode and carries out whatever task the keys were supposed to do. This gets a bit finnicky. So, report any possible bugs! For instance, see the source code for `<backspace>`-`DEL` hack.
- smart-god-mode-do-and-enter-keys
- smart-god-mode-auto-enter-on-ctrl-keys
- smart-god-mode-auto-enter-on-ctrl-exempt-keys

I have added the docstrings for these, which should be sufficiently explanatory; but feel free to point out if something is unclear!

Enjoy!
