;; -*- lexical-binding: t; -*-

;; C-x C-e to evaluate last expression
;; (eval-buffer)
;; M-x, eval-buffer

(setq x 0)

(defun getx () x)

(setq x 1)

(let ((x 2)) (getx))
;; result is 1

;; `defvar' creates a special variable,
;; it is always dynamically bound even
;; when `lexical-binding' is t.

