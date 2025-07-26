;;; killbuffs.el

(defun killbuffs ()
  "Delete surplus system buffers. Call this function twice in a row."
  (interactive)
  (mapc (lambda (bname) (and (get-buffer bname) (kill-buffer bname)))
        (list "*Completions*"
              "*Isearch completions*"
              "*Apropos*"
              "*Help*"
              "*Help*<2>"
              "*Help*<3>"
              "*Help*<4>"
              "*Help*<5>"
              "*Directory*"
              "*Buffer List*"
              "*Ibuffer*"
              "*Messages*"
              "*Compile-Log*"
              "*Native-compile-Log*"
              "*Async-native-compile-log*"
              "*Backtrace*"))
  (delete-other-windows))



;;; end of killbuffs.el
