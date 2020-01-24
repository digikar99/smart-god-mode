This isn't so much a mode ATM - since it pollutes the fundamental-mode-map. The intention is to "smartly" switch between the normal and insert modes (of god-mode).

I would add a better description later.

[god-mode is here.](https://github.com/chrisdone/god-mode)

For the time being, take a look at the first 45 lines of [the file](https://github.com/digikar99/smart-god-mode/blob/master/smart-god-mode.el), lines around line 120, 130, and the very last form `(god-mode-execute-command-and-exit-mode paredit-mode ...`.

I am finding it quite useful in lisp; not so much in terminals, or org-mode or markdown-mode.

The ultimate development would be to establish a brain computer interface :P; currently it's just hardcoded guesswork of what a programmar might want to do when they press a certain keyboard key.

PS: Turns out I have added a number of functions with names not found in packages.
