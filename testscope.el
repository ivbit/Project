;; -*- lexical-binding: nil; -*-

;; C-x C-e to evaluate last expression
;; (eval-buffer)
;; M-x, eval-buffer



;; If `lexical-binding' is t, then error is thrown:
;; Symbol's value as variable is void: testscope/var

(defun testscope/msg ()
  (message "%s" testscope/var))

(defun testscope/test ()
  (let ((testscope/var "Test scope variable.")) (testscope/msg)))

(testscope/test)



;; Anonymous function is defined by `lambda' in scope of `testscope/ltest',
;; it has access to `testscope/arg', which is defined in the same scope

(defun testscope/ltest (testscope/arg)
  (funcall (lambda () (message "%s" testscope/arg))))

(testscope/ltest "Test scope argument.")



;; `defvar' creates a special variable,
;; it is always dynamically bound
;; even when `lexical-binding' is t.
;; It is special only inside `let' and `let*'.

;; `testscope/special' is defined in scope of `testscope/stest',
;; it shadows the globally defined `testscope/special' defined by `defvar',
;; functions defined inside the function have the same scope as that function

(defvar testscope/special "Test scope special.")

(defun testscope/stest (testscope/special)
  (funcall (lambda () (message "%s" testscope/special))))

(testscope/stest "testscope/stest argument.")

;; When `lexical-binding' is t, parameters of caller are not in scope of
;; the called function, even if parameter has the same name as special variable

(setq lexical-binding t)

(defun testscope/getspecial ()
  testscope/special)

(defun testscope/ptest (testscope/special)
  (testscope/getspecial))

(testscope/ptest "testscope/ptest argument.")

;; Variable defined by `defvar' is only special inside `let' and `let*'
(defun testscope/let ()
  (let ((testscope/special "Inside let.")) (testscope/getspecial)))

(testscope/let)



;;; testscope.el ends here
