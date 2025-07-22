;;; maketabs.el --- Work with tab stops

;; `edit-tab-stops' is an interactive Emacs Lisp function.
;; Edit the tab stops used by `tab-to-tab-stop'.
;; Create a buffer *Tab Stops* containing text describing the tab stops.
;; A colon indicates a column where there is a tab stop.
;; You can add or remove colons and then do C-c C-c to make changes take effect.

;; ELSE part of `if' is an implicit `progn' in Emacs Lisp.
;; Bind key locally before calling `edit-tab-stops', or the local binding will
;; be set in the *Tab Stops* buffer istead of current buffer.
(defun maketabsl (&optional restore)
  "Set tab stops and bind `tab-to-tab-stop' to M-i, or restore user binding.
Press \\[universal-argument], then call this function to restore user binding.
Otherwise it will bind `tab-to-tab-stop' to M-i, then call `edit-tab-stops'."
  (interactive "P")
  (if restore
      (local-unset-key (kbd "M-i"))
    (local-set-key (kbd "M-i") 'tab-to-tab-stop)
    (edit-tab-stops)))



(defun maketabsg (&optional restore)
  "Set tab stops and bind `tab-to-tab-stop' to M-i, or restore user binding.
Press \\[universal-argument], then call this function to restore user binding.
Otherwise it will bind `tab-to-tab-stop' to M-i, then call `edit-tab-stops'."
  (interactive "P")
  (if restore
      (global-set-key (kbd "M-i") 'imenu)
    (global-set-key (kbd "M-i") 'tab-to-tab-stop)
    (edit-tab-stops)))



;; Local Variables:
;; mode: emacs-lisp
;; fill-column: 80
;; eval: (display-fill-column-indicator-mode 1)
;; tab-width: 2
;; End:

;;; maketabs.el ends here
