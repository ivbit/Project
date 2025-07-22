;;; new-bugfix.el --- -*- lexical-binding: t; -*-
;;; Emacs Lisp Intro - Appendix C "A graph with labeled axes"

;; Same license as code in Emacs Lisp Intro,
;; since its the same code with few bug fixes



;; Bugfix: added third argument `vertical-step'
(defun print-Y-axis (height full-Y-label-width vertical-step)
  "Insert Y axis using HEIGHT and FULL-Y-LABEL-WIDTH.
Height must be the maximum height of the graph.
Full width is the width of the highest label element."
  ;; Value of height and full-Y-label-width
  ;; are passed by print-graph.
  (let ((start (point)))
    (insert-rectangle
     (Y-axis-column height full-Y-label-width vertical-step))
    ;; Place point ready for inserting graph.
    (goto-char start)
    ;; Move point forward by value of full-Y-label-width
    (forward-char full-Y-label-width)))



;; Bugfix: added `symbol-width'
(defun print-X-axis-tic-line
    (number-of-X-tics X-axis-leading-spaces X-axis-tic-element symbol-width)
  "Print ticks for X axis."
  (insert X-axis-leading-spaces)
  (insert X-axis-tic-symbol)  ; Under first column.
  ;; Insert second tic in the right spot.
  (insert (concat
           (make-string
            (-  (* symbol-width X-axis-label-spacing)
                ;; Insert white space up to second tic symbol.
                (* 2 (length X-axis-tic-symbol)))
            ? )
           X-axis-tic-symbol))
  ;; Insert remaining ticks.
  (while (> number-of-X-tics 1)
    (insert X-axis-tic-element)
    (setq number-of-X-tics (1- number-of-X-tics))))



;; Bugfix: added `symbol-width'
(defun X-axis-element (number symbol-width)
  "Construct a numbered X axis element."
  (let ((leading-spaces
         (-  (* symbol-width X-axis-label-spacing)
             (length (number-to-string number)))))
    (concat (make-string leading-spaces ? )
            (number-to-string number))))



;; Bugfix: added `symbol-width' arg, `horizontal-step' after `concat'
(defun print-X-axis-numbered-line
    (number-of-X-tics X-axis-leading-spaces symbol-width
                      &optional horizontal-step)
  "Print line of X-axis numbers"
  (let ((number X-axis-label-spacing)
        (horizontal-step (or horizontal-step 1)))
    (insert X-axis-leading-spaces)
    ;; Delete extra leading spaces.
    (delete-char
     (- (1-
         (length (number-to-string horizontal-step)))))
    (insert (concat
             (number-to-string horizontal-step)
             (make-string
              ;; Insert white space.
              (-  (* symbol-width
                     X-axis-label-spacing)
                  (1-
                   (length
                    (number-to-string horizontal-step)))
                  2)
              ? )
             (number-to-string
              (* number horizontal-step))))
    ;; Insert remaining numbers.
    (setq number (+ number X-axis-label-spacing))
    (while (> number-of-X-tics 1)
      (insert (X-axis-element
               (* number horizontal-step) symbol-width))
      (setq number (+ number X-axis-label-spacing))
      (setq number-of-X-tics (1- number-of-X-tics)))))



;; Bugfix: added `full-Y-label-width', `symbol-width'
(defun print-X-axis
    (numbers-list full-Y-label-width symbol-width horizontal-step)
  "Print X axis labels to length of NUMBERS-LIST.
Optionally, HORIZONTAL-STEP, a positive integer,
specifies how much an X  axis label increments for
each column."
  ;; Value of symbol-width and full-Y-label-width
  ;; are passed by print-graph.
  (let* ((leading-spaces
          (make-string full-Y-label-width ? ))
         ;; symbol-width is provided by graph-body-print
         (tic-width (* symbol-width X-axis-label-spacing))
         (X-length (length numbers-list))
         (X-tic
          (concat
           (make-string
            ;; Make a string of blanks.
            (-  (* symbol-width X-axis-label-spacing)
                (length X-axis-tic-symbol))
            ? )
           ;; Concatenate blanks with tic symbol.
           X-axis-tic-symbol))
         (tic-number
          (if (zerop (% X-length tic-width))
              (/ X-length tic-width)
            (1+ (/ X-length tic-width)))))

    (print-X-axis-tic-line
     tic-number leading-spaces X-tic symbol-width)
    (insert "\n")
    (print-X-axis-numbered-line
     tic-number leading-spaces symbol-width horizontal-step)))



;; Bugfix: added `starting-column', `full-Y-label-width', `symbol-width'
(defun print-graph
    (numbers-list &optional vertical-step horizontal-step)
  "Print labeled bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values.

Optionally, VERTICAL-STEP, a positive integer,
specifies how much a Y axis label increments for
each line.  For example, a step of 5 means that
each row is five units.

Optionally, HORIZONTAL-STEP, a positive integer,
specifies how much an X  axis label increments for
each column."
  (let* ((symbol-width (length graph-blank))
         ;; height is both the largest number
         ;; and the number with the most digits.
         (height (apply #'max numbers-list))
         (height-of-top-line
          (if (zerop (% height Y-axis-label-spacing))
              height
            ;; else
            (* (1+ (/ height Y-axis-label-spacing))
               Y-axis-label-spacing)))
         (vertical-step (or vertical-step 1))
         (full-Y-label-width
          (length
           (concat
            (number-to-string
             (* height-of-top-line vertical-step))
            Y-axis-tic)))
         (starting-column (current-column)))
    (print-Y-axis
     height-of-top-line full-Y-label-width vertical-step)
    (graph-body-print
     numbers-list height-of-top-line symbol-width)
    (print-X-axis numbers-list
     (+ starting-column full-Y-label-width) symbol-width horizontal-step)))



;; Local Variables:
;; fill-column: 80
;; End:

;;; new-bugfix.el ends here
