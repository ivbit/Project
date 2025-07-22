
; Mode hooks
(add-hook 'sh-mode-hook
  (lambda ()
    (font-lock-add-keywords nil '(("\\(\\\\[a-z]\\)" 1 font-lock-constant-face t)))
    (sh-electric-here-document-mode)))

(add-hook 'awk-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\(\\\\[0-9a-z]\\|\\<[0-9]+[.]?[0-9]*\\(?:[Ee][+-]?[0-9]+\\)?\\>\\)"
          1 font-lock-constant-face t)))))

(add-hook 'tcl-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\(\\\\[0-9A-Za-z]\\|\\<[0-9]+[.]?[0-9]*\\(?:[Ee][+-]?[0-9]+\\)?\\>\\)"
          1 font-lock-constant-face t)
        ("\\(\\\\u[[:xdigit:]]\\{4\\}\\|\\\\U[[:xdigit:]]\\{8\\}\\|\\\\x[[:xdigit:]]\\{2\\}\\)"
          1 font-lock-type-face t)))))

