;;; elispExamples.el --- elex - ELisp EXamples -*- fill-column: 80 -*-

;; M-x add-file-local-variable
;; M-x add-file-local-variable-prop-line
;; M-x delete-file-local-variable
;; M-x delete-file-local-variable-prop-line

;; C-x ]           to move forward  one page (to next form feed ^L)
;; C-x [           to move backward one page (to next form feed ^L)
;; C-h f           to display help about a function
;; C-h v           to display help about a variable
;; C-x C-e         to evaluate the last s-expression (function, variable, etc.)
;; C-[ :           to evaluate the s-expression entered at minibuffer prompt
;; M-x             to evaluate interactive function (M-x elex/bnfs)
;; M-x eval-buffer to evaluate all functions in this library (file)

;; C-x n n         to call `narrow-to-region'
;; C-x n p         to call `narrow-to-page'
;; C-x n w         to call `widen'
;; M-x append-to-buffer - copy region to point position in other buffer, prompts

;; Use `defvar'             to create new global variables,
;; `let' and `let*'         to create new local variables,
;; `set', `setq' and `setf' to change values of both global and local variables

;; (setq deactivate-mark t) ; turn off highlighting of the region

(defun elex/optarg (&optional arg)
  "Optional argument is nil when not provided."
  (message "arg is %s" arg))
(elex/optarg)
(elex/optarg "Hello world!")

(defun elex/bnfs (bf fc bs)
  "`interactive' binds items from list to the arguments in same order,
  call this function interactively, or provide 3 arguments."
  (interactive
   (list (buffer-name)
         fill-column
         (buffer-size)))
  (message "Name: %s, fill: %d, size: %d." bf fc bs))

(defun elex/sume (a b c) ; List is created by `list' function
  "Sum example, using `interactive' to bind numbers to arguments."
  (interactive (list 11 33 55))
  (message "Sum of %d, %d, %d is: %d." a b c (+ a b c)))
(elex/sume 4 5 6) ; non-interactive use example

(defun elex/sume (a b c) ; List created by `quote' special form: single quote '
  "Sum example, using `interactive' to bind numbers to arguments."
  (interactive '(11 33 55))
  (message "Sum of %d, %d, %d is: %d." a b c (+ a b c)))
(elex/sume 7 8 9) ; non-interactive use example

(defun elex/unitest (arg)
  "Test for `universal-argument' function.
ARG must be a number.
No arguments:                   return nil.
Universal argument by itself:   return a list with argument (number) inside.
Universal argument with number: return just the number.
Usage:
\\[execute-extended-command] elex/unitest
\\[universal-argument] \\[execute-extended-command] elex/unitest
\\[universal-argument] number \\[execute-extended-command] elex/unitest"
  (interactive "P")
  (message "arg is: %s" arg))

(defun elex/pnv (arg)
  "`prefix-numeric-value' returns argument from `interactive' as number.
\"P\" is a raw argument, \"p\" is converted to a number automatically:
(interactive \"P\")
Usage:
\\[execute-extended-command] elex/pnv
\\[universal-argument] \\[execute-extended-command] elex/pnv
\\[universal-argument] number \\[execute-extended-command] elex/pnv"
  (interactive "P")
  (message "raw: %s, converted: %s" arg (prefix-numeric-value arg)))

(defun elex/optargex (&optional arg)
  "Write an interactive function with an optional argument that tests
whether its argument, a number, is greater than or equal to, or else,
less than the value of `fill-column', and tells you which, in a message.
However, if you do not pass an argument to the function, use 56 as a
default value."
  (interactive "P")
  (or (numberp arg) (consp arg) (setq arg 56))
  (setq arg (prefix-numeric-value arg))
  (if (>= arg fill-column)
      (message "%d >= %d" arg fill-column)
    (message "%d < %d" arg fill-column)))

(defun elex/rtest (b e)
  "\"r\" assigns begin and end of a region to 2 arguments."
  (interactive "r")
  (message "Begin: %s   End: %s" b e))



;; `car' - Contents of the   Address part of the Register
;; `cdr' - Contents of the Decrement part of the Register, pronounced "could-er"
;; How lists are actually constructed (what `list' function does):
(list 'a 'b 'c)
;; nil is the same as ()
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b (cons 'c ())))



;; Loops and recursion
;; `while' loops always return nil
(defvar elex/num nil "Counter")
(setq elex/num 5)
(while (> elex/num 0) (message "%d" (setq elex/num (1- elex/num))))
(dolist (elex/num '(1 2 3 4)) (message "%d" elex/num))
;; (dotimes (elex/num 4) (insert-for-yank "Emacs is better than Vim!\n"))

(defvar elex/elist '(a b c d) "Example list.")
(defun elex/whilelst (lst)
  "Print each element of LST on a line of its own."
  (or (listp lst) (setq lst nil))
  (while lst
    (message "%s" (car lst))
    (setq lst (cdr lst))))
;; C-u C-x C-e - result of evaluation will be printed
(elex/whilelst elex/elist)

(defun elex/triu (num)
  "Add number of items in a triangle."
  (let ((total 0)
        (rnum 1))
    (while (<= rnum num)
      (set 'total (+ total rnum))
      (set 'rnum (1+ rnum)))
    (message "%d" total)))
(elex/triu 5)
(elex/triu 9)

(defun elex/trid (num)
  "Add NUMBER of items in a triangle.

\(fn NUMBER)"
  (let ((total 0))
    (while (> num 0)
      (set 'total (+ total num))
      (set 'num (1- num)))
    (message "%d" total)))
(elex/trid 5)
(elex/trid 9)

(defun elex/trit (num)
  "Add number of items in a triangle using `dotimes'."
  (let ((total 0)) ; Define local variable
    (dotimes (times num)
      (set 'total (+ total times 1)))
    (message "%d" total)))
(elex/trit 5)
(elex/trit 9)



;; Regular expressions:
;; \|                         Alternative
;; \{ \}                      Repetition
;; \< \>                      Beginning and end of word
;; \_< \_>                    Beginning and ond of symbol
;; \( \)                      Capturing group
;; \1 to \9                   Insert text from group
;; \#1 to \#9                 Insert text from group as an integer
;; \?                         Prompt user for text input
;; \#                         Insert number incremented from 0 after every match
;; \&                         Insert whole match
;; \,(function)               Call elisp function in replace portion of regexp
;; \sCODE                     Match        character from code table
;; \Scode                     Do not match character from code table
;; Code table (can change depending on major mode):
;; -                          Whitespace
;; w                          Alnum
;; _                          Alnum with extra symbols: _ ! and others
;; .                          Punctuation characters
;; ( and )                    Grouped pair: () [] {}
;; "                          String characters: " '
;; < and >                    Open/close comment characters



;; Local Variables:
;; eval: (display-fill-column-indicator-mode 1)
;; tab-width: 2
;; End:
