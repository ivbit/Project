;;; nasciir.el --- Replace some non-ASCII chars

;; Place cursor on character, C-u C-x = to view description and hex equivalent.

;; `replace-regexp' is for interactive use only.
;; Use `re-search-forward' and `replace-match' instead.

(defun nasciir ()
  "Replace some non-ASCII characters and TAB character.
User init file should not contain non-ASCII characters.
Use hexadecimal representation instead."
  (interactive)
  (save-excursion
    (let ((rlist
           (list
            "[\x2018\x2019\x2032]" "'"
            "[\x2033\x201c\x201d\x201e\x00ab\x00bb]" "\x0022"
            "[\x2022\x2011\x2013\x2212]" "-"
            "[\x2014]" " - "
            "[\x00b1]" "+-"
            "[\x2026]" "..."
            "[\x00b0\x00ad]" ""
            "[\x00a0]" " "
            "[\x0009]" "        "
            "[\x00a9]" "(c)"
            "[\x00ae]" "(r)"
            "[\x2117]" "(p)"
            "[\x2122]" "(tm)"
            "[\x2190]" "<-"
            "[\x2192]" "->"
            "[\x27f5]" "<--"
            "[\x27f6]" "-->"
            "[\x21d0]" "<="
            "[\x21d2]" "=>"
            "[\x22c6\x2605]" "*"
            "[\x20bd]" "\x0440."
            ))
          regexp newtext)
      (message "Begin replacing non-ASCII characters...")
      (while rlist
        ;; (message "%s" rlist)
        ;; (replace-regexp (car rlist) (cadr rlist) nil (point-min) (point-max))
        (setq regexp (car rlist) newtext (cadr rlist) rlist (cddr rlist))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t) (replace-match newtext t t)))
      (message "Done replacing non-ASCII characters."))))



;; Local Variables:
;; mode: emacs-lisp
;; fill-column: 80
;; eval: (display-fill-column-indicator-mode 1)
;; tab-width: 2
;; End:

;;; nasciir.el ends here
