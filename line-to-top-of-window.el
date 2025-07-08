;;; Line to top of window;
;;; replace three keystroke sequence C-u 0 C-l
(defun line-to-top-of-window ()
  "Move the line that point is on to top of window."
  (interactive)
  (recenter 0))

