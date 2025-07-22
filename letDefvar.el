;; -*- lexical-binding: t; -*-

;; C-x C-e to evaluate last expression
;; (eval-buffer)
;; M-x, eval-buffer

(defvar x 0 "Always dynamically bound!")

(defun getx () x)

(setq x 1)

(let ((x 2)) (getx))
;; result is 2
;; without `defvar' with just `setq'
;; the result will be 1, because in
;; this buffer - lexical-binding: t

;; `defvar' creates a special variable,
;; it is always dynamically bound even
;; when `lexical-binding' is t.

