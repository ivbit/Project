;;; eintr-graph.el --- Appendix C "A graph with labeled axes"

;; Same license as code in Emacs Lisp Intro,
;; since its the same code with 3 minor changes:
;; 2 bug fixes and 1 function `eintr-graph'



;; Appendix C "A graph with labeled axes" code starts here
(defvar top-of-ranges
  '(10  20  30  40  50
    60  70  80  90 100
   110 120 130 140 150
   160 170 180 190 200
   210 220 230 240 250
   260 270 280 290 300)
  "List specifying ranges for `defuns-per-range'.")

(defvar list-for-graph
  '(537 1027 955 785 594
    483  349 292 224 199
    166  120 116  99  90
     80   67  48  52  45
     41   33  28  26  25
     20   12  28  11  13
    220)
  "List of defuns with amount of words per range, last > 300.")
;; (apply #'+ list-for-graph) ;  6785

(defun one-fiftieth (full-range)
  "Return list, each number one-fiftieth of previous."
  (mapcar (lambda (arg) (/ arg 50)) full-range))

(defvar fiftieth-list-for-graph
  (one-fiftieth list-for-graph)
  "Reduced list to display a graph on one computer screen.")

(defvar graph-symbol "*"
  "String used as symbol in graph, usually an asterisk.")

(defvar graph-blank " "
  "String used as blank in graph, usually a blank space.
graph-blank must be the same number of columns wide
as graph-symbol.")

(defvar Y-axis-label-spacing 5
  "Number of lines from one Y axis label to next.")

(defvar Y-axis-tic " - "
  "String that follows number in a Y axis label.")

(defvar X-axis-label-spacing
  (if (boundp 'graph-blank)
      (* 5 (length graph-blank)) 5)
  "Number of units from one X axis label to next.")

(defvar X-axis-tic-symbol "|"
  "String to insert to point to a column in X axis.")

(defun column-of-graph (max-graph-height actual-height)
  "Return MAX-GRAPH-HEIGHT strings; ACTUAL-HEIGHT are graph-symbols.

The graph-symbols are contiguous entries at the end
of the list.
The list will be inserted as one column of a graph.
The strings are either graph-blank or graph-symbol."

  (let ((insert-list nil)
        (number-of-top-blanks
         (- max-graph-height actual-height)))

    ;; Fill in ‘graph-symbols’.
    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1- actual-height)))

    ;; Fill in ‘graph-blanks’.
    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks
            (1- number-of-top-blanks)))

    ;; Return whole list.
    insert-list))

(defun Y-axis-element (number full-Y-label-width)
  "Construct a NUMBERed label element.
A numbered element looks like this `  5 - ',
and is padded as needed so all line up with
the element for the largest number."
  (let ((leading-spaces
         (- full-Y-label-width
            (length
             (concat (number-to-string number)
                     Y-axis-tic)))))
    (concat
     (make-string leading-spaces ? )
     (number-to-string number)
     Y-axis-tic)))

(defun Y-axis-column
    (height width-of-label &optional vertical-step)
  "Construct list of labels for Y axis.
HEIGHT is maximum height of graph.
WIDTH-OF-LABEL is maximum width of label.
VERTICAL-STEP, an option, is a positive integer
that specifies how much a Y axis label increments
for each line.  For example, a step of 5 means
that each line is five units of the graph."
  (let (Y-axis
        (number-per-line (or vertical-step 1)))
    (while (> height 1)
      (if (zerop (% height Y-axis-label-spacing))
          ;; Insert label.
          (setq Y-axis
                (cons
                 (Y-axis-element
                  (* height number-per-line)
                  width-of-label)
                 Y-axis))
        ;; Else, insert blanks.
        (setq Y-axis
              (cons
               (make-string width-of-label ? )
               Y-axis)))
      (setq height (1- height)))
    ;; Insert base line.
    (setq Y-axis (cons (Y-axis-element
                        (or vertical-step 1)
                        width-of-label)
                       Y-axis))
    (nreverse Y-axis)))

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

(defun graph-body-print (numbers-list height symbol-width)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values.
HEIGHT is maximum height of graph.
SYMBOL-WIDTH is number of each column."
  (let (from-position)
    (while numbers-list
      (setq from-position (point))
      (insert-rectangle
       (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      ;; Draw graph column by column.
      (sit-for 0)
      (setq numbers-list (cdr numbers-list)))
    ;; Place point for X axis labels.
    (forward-line height)
    (insert "\n")))

(defun print-X-axis-tic-line
    (number-of-X-tics X-axis-leading-spaces X-axis-tic-element)
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

(defun X-axis-element (number)
  "Construct a numbered X axis element."
  (let ((leading-spaces
         (-  (* symbol-width X-axis-label-spacing)
             (length (number-to-string number)))))
    (concat (make-string leading-spaces ? )
            (number-to-string number))))

(defun print-X-axis-numbered-line
    (number-of-X-tics X-axis-leading-spaces
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
               (* number horizontal-step)))
      (setq number (+ number X-axis-label-spacing))
      (setq number-of-X-tics (1- number-of-X-tics)))))

(defun print-X-axis (numbers-list horizontal-step)
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
     tic-number leading-spaces X-tic)
    (insert "\n")
    (print-X-axis-numbered-line
     tic-number leading-spaces horizontal-step)))

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
            Y-axis-tic))))
    (print-Y-axis
     height-of-top-line full-Y-label-width vertical-step)
    (graph-body-print
     numbers-list height-of-top-line symbol-width)
    (print-X-axis numbers-list horizontal-step)))

(defun eintr-graph ()
  "Print the graph from Appendix C of eintr."
  (interactive)
  (print-graph fiftieth-list-for-graph 50 10))

;; In this buffer:
;; M-x, eval-buffer
;; Switch to *scratch* buffer, then:
;; M-x, eintr-graph
;; To clear the *scratch* buffer:
;; M-x, erase-buffer



;; Local Variables:
;; fill-column: 80
;; End:

;;; eintr-graph.el ends here
