;; Recursion: You may want to increase the values of `max-specpdl-size' and
;; `max-lisp-eval-depth'.  In my init file, I set them to 15 and 30 times
;; their default value: Elisp Intro (eintr), Loops & Recursion (chapter 11)
(describe-variable 'max-specpdl-size)
(* max-specpdl-size 15) ; C-x C-e to get result in *Messages* buffer
(customize-variable 'max-specpdl-size) ; multiply by 15
(describe-variable 'max-lisp-eval-depth)
(* max-lisp-eval-depth 30) ; C-x C-e to get result in *Messages* buffer
(customize-variable 'max-lisp-eval-depth) ; multiply by 30