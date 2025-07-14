;;; -*- mode: emacs-lisp -*-

(defun blines ()
  "Delete all blank lines in current buffer, or narrowed part."
  (interactive)
  (save-excursion
    (let ((fresult (flush-lines "^\\s-*$" (point-min) (point-max))))
      (cond ((zerop fresult) (message "There were no blank lines to delete."))
            ((= fresult 1) (message "Deleted one blank line."))
            (t (message "Deleted %d blank lines." fresult))))))

;; C-x C-e to install, C-h e *Messages*
;; DO NOT run `eval-buffer', or `load-file' on this file.
(defun pcontents (lst)
  "Display contents of LST in *Messages* buffer."
  (interactive "SList variable: ")
  (and (get-buffer "*Messages*") (kill-buffer "*Messages*"))
  (message "Contents of `%s':" (or (and (called-interactively-p 'any) lst) "variable"))
  (and (called-interactively-p 'any) (setq lst (eval lst)))
  (cond ((listp lst)
         (while lst (message "%s" (car lst)) (setq lst (cdr lst)))
         (switch-to-buffer "*Messages*") nil)
        (t (message "Variable is not a list!"))))

;; M-x, pcontents, name of variable containing list
;; C-h e        switch to         *Messages* buffer
;; q            close             *Messages* buffer
;; C-u q        close and delete  *Messages* buffer
;; C-x <left>   switch to           previous buffer
;; C-x <right>  switch to               next buffer
(pcontents exec-path)
(pcontents initial-environment)
(pcontents process-environment)
(pcontents default-frame-alist)
(pcontents package-archives)
;;; end of file
