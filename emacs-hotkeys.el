;;; emacs-hotkeys.el --- Some hotkeys and functions bound to them.

;; ESC can be pressed and released instead of holding Alt key (Meta key)
(describe-key (kbd "C-[")) ; C-[ sends the same signal as ESC

;; Place cursor after closing parenthesis, then C-x C-e to evaluate expression:
(describe-function #'eval-last-sexp) ; C-x C-e
(describe-function #'keyboard-quit) ; C-g
(describe-function #'abort-minibuffers) ; C-g (in minibuffer prompt)
(describe-function #'keyboard-escape-quit) ; ESC ESC ESC
(describe-function #'execute-extended-command) ; M-x
(describe-function #'eval-expression) ; M-:, M-ESC :
(describe-function #'repeat-complex-command) ; C-x ESC ESC, C-x M-:
;; Minibuffer history commands: M-n and M-p
(describe-function #'previous-error) ;    M-g p, M-g M-p
(describe-function #'next-error) ; C-x `, M-g n, M-g M-n
;; Emacs pages are between FORM FEED characters ^L (C-q C-l)
(describe-function #'quoted-insert) ; C-q
(describe-variable 'page-delimiter)
(describe-function #'insert-char) ; C-x 8 RET (need to disable `fido-mode')
(describe-function #'make-string) ; (default unbound)



;; Help
(describe-function #'info) ; C-h i
(describe-function #'man) ; (default unbound)
(describe-function #'view-echo-area-messages) ; C-h e
(describe-function #'help-for-help) ; C-h C-h, C-h ?
(describe-function #'help) ; (default unbound)
(describe-function #'describe-mode) ; C-h m
(describe-function #'describe-bindings) ; C-h b
(describe-function #'describe-variable) ; C-h v
(describe-function #'describe-key-briefly) ; C-h c
(describe-function #'describe-key) ; C-h k
(describe-function #'describe-function) ; C-h f
(describe-function #'describe-command) ; C-h x
(describe-function #'xref-find-definitions) ; M-. (make TAGS with etags, ctags)
(describe-function #'xref-find-definitions-other-window) ; C-x 4 .
(describe-function #'xref-find-apropos) ; C-M-. (can crash Emacs)
(describe-function #'apropos-command) ; C-h a
(describe-function #'apropos) ; (default unbound)
(describe-function #'what-cursor-position) ; C-x =
(describe-function #'what-line) ; (default unbound)
(describe-function #'view-hello-file) ; C-h h



;; Narrowing, widening
(describe-function #'narrow-to-region) ; C-x n n
(describe-function #'narrow-to-page) ; C-x n p
(describe-function #'narrow-to-defun) ; C-x n d
(describe-function #'goto-line-relative) ; C-x n g
(describe-function #'widen) ; C-x n w

(describe-function #'save-restriction) ; (default unbound)



;; Movement
;; (customize-variable 'next-screen-context-lines) ; affects M-v, C-v
(describe-function #'beginning-of-buffer) ; M-<
(describe-function #'end-of-buffer) ; M->
(describe-function #'backward-page) ; C-x [
(describe-function #'forward-page) ; C-x ]
(describe-function #'scroll-down-command) ; M-v
(describe-function #'scroll-up-command) ; C-v
(describe-function #'scroll-other-window-down) ; C-M-S-v
(describe-function #'scroll-other-window) ; C-M-v

(describe-function #'reposition-window) ; C-M-l
(describe-function #'recenter-top-bottom) ; C-l
(describe-function #'move-to-window-line-top-bottom) ; M-r
(describe-function #'previous-line) ; C-p
(describe-function #'next-line) ; C-n
(describe-function #'goto-line) ; M-g g, M-g M-g
(describe-function #'move-to-column) ; M-g TAB
(describe-function #'goto-char) ; M-g c

(describe-function #'backward-char) ; C-b
(describe-function #'forward-char) ; C-f
(describe-function #'backward-word) ; M-b
(describe-function #'forward-word) ; M-f
(describe-function #'backward-sentence) ; M-a
(describe-function #'forward-sentence) ; M-e
(describe-function #'backward-paragraph) ; M-{
(describe-function #'forward-paragraph) ; M-}

(describe-function #'backward-up-list) ; C-M-u
(describe-function #'backward-list) ; C-M-p
(describe-function #'forward-list) ; C-M-n
(describe-function #'backward-sexp) ; C-M-b
(describe-function #'forward-sexp) ; C-M f

(describe-function #'recenter) ; (default unbound)
(describe-function #'save-excursion) ; (default unbound)



;; Regular expressions:
;; \|                         Alternative
;; \{ \}                      Repetition
;; \` \'                      Beginning and end of buffer
;; \< \>                      Beginning and end of word
;; \b                         Beginning, or end of word
;; \B                         Not at beginning, or end of word
;; \_< \_>                    Beginning and end of symbol
;; \( \)                      Capturing group
;; \1 to \9                   Insert text from group
;; \#1 to \#9                 Insert text from group as an integer
;; \?                         Prompt user for text input
;; \#                         Insert number incremented from 0 after every match
;; \&                         Insert whole match
;; \,(function)               Call elisp function in replace portion of regexp
;; \w                         Word-constituent character
;; \W                         Not Word-constituent character
;; \sCODE                     Match        character from code table
;; \Scode                     Do not match character from code table
;; Code table (can change depending on major mode):
;; -                          Whitespace
;; w                          Alnum
;; _                          Alnum with extra symbols: _ ! and others
;; .                          Punctuation characters
;; ( and )                    Grouped pair: () [] {}
;; "                          String characters: " '
;; < and >                    Open/close comment characters



;; Search and replace
(describe-function #'search-backward) ; (default unbound)
(describe-function #'re-search-backward) ; (default unbound)
(describe-function #'search-forward) ; (default unbound)
(describe-function #'re-search-forward) ; (default unbound)

;; Inside I-search: M-< first match, M-> last match,
;; M-v previous off-screen match, C-v next off-screen match
;; (customize-variable 'isearch-allow-motion) ; turn on this option
(describe-function #'isearch-backward-regexp) ; C-M-r
(describe-function #'isearch-forward-regexp) ; C-M-s

(describe-function #'isearch-backward) ; C-r
(describe-function #'isearch-repeat-backward) ; C-r, C-M-r (inside I-search)
(describe-function #'isearch-end-of-buffer) ; M-s M-> (inside I-search)
(describe-function #'isearch-forward) ; C-s
(describe-function #'isearch-repeat-forward) ; C-s, C-M-s (inside I-search)
(describe-function #'isearch-beginning-of-buffer) ; M-s M-< (inside I-search)

(describe-function #'isearch-forward-word) ; M-s w
(describe-function #'isearch-forward-symbol) ; M-s _
(describe-function #'isearch-forward-symbol-at-point) ; M-s .
(describe-function #'isearch-forward-thing-at-point) ; M-s M-.

(describe-function #'isearch-yank-char) ; C-M-y (inside I-search)
(describe-function #'isearch-yank-word-or-char) ; C-w (inside I-search)
(describe-function #'isearch-yank-line) ; M-s C-e (inside I-search)
(describe-function #'isearch-yank-kill) ; C-y (inside I-search)
(describe-function #'isearch-yank-pop-only) ; M-y (inside I-search)

(describe-function #'isearch-complete) ; C-M-i (inside I-search)
(describe-function #'isearch-ring-retreat) ; M-p (inside I-search)
(describe-function #'isearch-ring-advance) ; M-n (inside I-search)

(describe-function #'isearch-toggle-case-fold) ; M-c, M-s c (inside I-search)
(describe-function #'isearch-toggle-char-fold) ; M-s ' (inside I-search)
(describe-function #'isearch-toggle-regexp) ; M-r, M-s r (inside I-search)
(describe-function #'isearch-toggle-word) ; M-s w (inside I-search)
(describe-function #'isearch-toggle-symbol) ; M-s _ (inside I-search)
(describe-function #'isearch-toggle-lax-whitespace) ; M-s SPC (inside I-search)

(describe-function #'isearch-abort) ; C-g  (inside I-search)
(describe-function #'isearch-exit) ; RET (inside I-search)

(describe-function #'query-replace) ; M-%
(describe-function #'query-replace-regexp) ; C-M-%
(describe-function #'replace-string) ; (default unbound)
(describe-function #'flush-lines) ; (default unbound)
(describe-function #'keep-lines) ; (default unbound)
(describe-function #'delete-duplicate-lines) ; (default unbound)
(describe-function #'kill-matching-lines) ; (default unbound)
(describe-function #'copy-matching-lines) ; (default unbound)

(describe-function #'occur) ; M-s o
(describe-function #'multi-occur) ; (default unbound)
(describe-function #'multi-occur-in-matching-buffers) ; (default unbound)
(describe-function #'imenu) ; M-i (default unbound)



;; Cut, copy, paste, select region
(describe-function #'backward-delete-char-untabify) ; BACKSPACE
(describe-function #'delete-char) ; C-d
(describe-function #'append-next-kill) ; C-M-w
(describe-function #'backward-kill-word) ; C-BACKSPACE
(describe-function #'kill-word) ; M-d
(describe-function #'zap-to-char) ; M-z
(describe-function #'zap-up-to-char) ; C-M-z (default unbound)
(describe-function #'kill-line) ; C-k
(describe-function #'kill-whole-line) ; C-S-BACKSPACE
(describe-function #'backward-kill-sentence) ; C-x BACKSPACE
(describe-function #'kill-sentence) ; M-k
(describe-function #'backward-kill-sexp) ; C-[ C-BACKSPACE
(describe-function #'kill-sexp) ; C-M-k

(describe-function #'mark-whole-buffer) ; C-x h
(describe-function #'mark-page) ; C-x C-p
(describe-function #'mark-paragraph) ; M-h
(describe-function #'mark-defun) ; C-M-h
(describe-function #'mark-sexp) ; C-M-SPC, C-M-@
(describe-function #'mark-word) ; M-@
(describe-function #'set-mark-command) ; C-SPC, C-@
(describe-function #'exchange-point-and-mark) ; C-x C-x

(describe-function #'count-words-region) ; M-=
(describe-function #'delete-active-region) ; (default unbound)
(describe-function #'delete-region) ; (default unbound)
(describe-function #'kill-region) ; C-w
(describe-function #'kill-ring-save) ; M-w
(describe-function #'yank) ; C-y
(describe-function #'yank-pop) ; M-y

(describe-function #'mouse-save-then-kill) ; MOUSE3
(describe-function #'mouse-secondary-save-then-kill) ; M-MOUSE3

(describe-function #'rectangle-mark-mode) ; C-x SPC
(describe-function #'rectangle-number-lines) ; C-x r N
(describe-function #'open-rectangle) ; C-x r o
(describe-function #'string-rectangle) ; C-x r t
(describe-function #'clear-rectangle) ; C-x r c
(describe-function #'delete-rectangle) ; C-x r d
(describe-function #'kill-rectangle) ; C-x r k
(describe-function #'copy-rectangle-as-kill) ; C-x r M-w
(describe-function #'yank-rectangle) ; C-x r y



;; Case
(describe-function #'capitalize-word) ; M-c
(describe-function #'downcase-word) ; M-l
(describe-function #'upcase-word) ; M-u

(describe-function #'upcase-initials-region) ; (default unbound)
(describe-function #'capitalize-region) ; (default unbound)
(describe-function #'downcase-region) ; C-x C-l
(describe-function #'upcase-region) ; C-x C-u

(describe-function #'upcase-initials) ; (default unbound)
(describe-function #'capitalize) ; (default unbound)
(describe-function #'downcase) ; (default unbound)
(describe-function #'upcase) ; (default unbound)



;; Transpose
(describe-function #'transpose-chars) ; C-t
(describe-function #'transpose-words) ; M-t
(describe-function #'transpose-lines) ; C-x C-t
(describe-function #'transpose-sentences) ; (default unbound)
(describe-function #'transpose-paragraphs) ; (default unbound)
(describe-function #'transpose-sexps) ; C-M-t
(describe-function #'transpose-regions) ; (default unbound)

(describe-function #'transpose-subr) ; (default unbound)
(describe-function #'transpose-subr-1) ; (default unbound)



;; Indentation; whitespace
(describe-function #'indent-for-tab-command) ; TAB
(describe-function #'newline) ; RET
(describe-function #'open-line) ; C-o
(describe-function #'split-line) ; C-M-o
(describe-function #'delete-blank-lines) ; C-x C-o
(describe-function #'delete-indentation) ; M-^
(describe-function #'just-one-space) ; M-SPC
(describe-function #'delete-horizontal-space) ; M-\
(describe-function #'delete-to-left-margin) ; (default unbound)
(describe-function #'delete-trailing-whitespace) ; (default unbound)



;; Comments
(describe-variable 'comment-column)
(describe-function #'comment-indent) ; (default unbound)
(describe-function #'default-indent-new-line) ; M-j, C-M-j
(describe-function #'comment-line) ; C-x C-;
(describe-function #'comment-box) ; (default unbound)
(describe-function #'comment-dwim) ; M-;



;; Working with registers
(describe-function #'number-to-register) ; C-x r n
(describe-function #'increment-register) ; C-x r +
(describe-function #'copy-to-register) ; C-x r s, C-x r x
(describe-function #'insert-register) ; C-x r g, C-x r i
(describe-function #'point-to-register) ; C-x r SPC, C-x r C-SPC, C-x r C-@
(describe-function #'jump-to-register) ; C-x r j

(describe-function #'frameset-to-register) ; C-x r f
(describe-function #'window-configuration-to-register) ; C-x r w

(describe-function #'savehist-mode) ; (default unbound)
(describe-function #'desktop-save-mode) ; (default unbound)



;; Repeat last action (kill, yank, etc.), undo, redo
(describe-function #'repeat) ; C-x z
(describe-function #'undo) ; C-/, C-_, C-x u
(describe-function #'undo-redo) ; C-?, C-M-_



;; Bookmarks
(describe-function #'bookmark-bmenu-list) ; C-x r l
(describe-function #'bookmark-set) ; C-x r m
(describe-function #'bookmark-set-no-overwrite) ; C-x r M
(describe-function #'bookmark-jump) ; C-x r b
(describe-function #'bookmark-delete) ; (default unbound)



;; Buffers, windows, Emacs
(describe-function #'make-frame-command) ; C-x 5 2
(describe-function #'display-buffer-other-frame) ; C-x 5 C-o
(describe-function #'switch-to-buffer-other-frame) ; C-x 5 b
(describe-function #'other-frame) ; C-x 5 o
(describe-function #'delete-other-frames) ; C-x 5 1
(describe-function #'delete-frame) ; C-x 5 0
(describe-function #'split-window-below) ; C-x 2
(describe-function #'split-window-right) ; C-x 3
(describe-function #'other-window) ; C-x o, M-o
(describe-function #'delete-other-windows) ; C-x 1
(describe-function #'delete-window) ; C-x 0
(describe-function #'quit-window) ; q (in special buffers)

(describe-function #'shrink-window-if-larger-than-buffer) ; C-x -
(describe-function #'balance-windows) ; C-x +
(describe-function #'shrink-window) ; (default unbound)
(describe-function #'enlarge-window) ; C-x ^
(describe-function #'shrink-window-horizontally) ; C-x {
(describe-function #'enlarge-window-horizontally) ; C-x }

(describe-function #'kill-buffer-and-window) ; C-x 4 0
(describe-function #'kill-buffer) ; C-x k
(describe-function #'previous-buffer) ; C-x <left>, C-x C-<left>
(describe-function #'next-buffer) ; C-x <right>, C-x C-<right>
(describe-function #'pop-to-buffer) ; (default unbound)
(describe-function #'switch-to-buffer) ; C-x b
(describe-function #'list-buffers) ; unbound (default C-x C-b)
(describe-function #'buffer-menu) ; unbound (may be bound to C-x C-b)
(describe-function #'ibuffer) ; C-x C-b (default unbound)
(describe-function #'revert-buffer) ; g (in special buffers)

(describe-function #'list-directory) ; C-x C-d
(describe-function #'dired) ; C-x d
(describe-function #'dired-jump) ; C-x C-j
(describe-function #'dired-jump-other-window) ; C-x 4 C-j
(describe-variable 'default-directory)
(describe-function #'find-file-other-frame) ; C-x 5 f, C-x 5 C-f
(describe-function #'find-file-other-window) ; C-x 4 f, C-x 4 C-f
(describe-function #'find-file) ; C-x C-f
(describe-function #'find-file-read-only) ; C-x C-r
(describe-function #'read-only-mode) ; C-x C-q
(describe-function #'recover-this-file) ; (default unbound)
(describe-function #'recover-file) ; (default unbound)
(describe-function #'write-file) ; C-x C-w
(describe-function #'save-buffer) ; C-x C-s
(describe-function #'save-some-buffers) ; C-x s
(describe-function #'save-buffers-kill-terminal) ; C-x C-M-c (default C-x C-c)



;; Coding systems
(describe-function #'universal-coding-system-argument) ; C-x RET c
(describe-function #'revert-buffer-with-coding-system) ; C-x RET r
(describe-function #'set-buffer-file-coding-system) ; C-x RET f
(describe-function #'set-file-name-coding-system) ; C-x RET F
(describe-function #'recode-region) ; (default unbound)
;; (setq-default buffer-file-coding-system 'utf-8-unix) ; example
(describe-function #'set-keyboard-coding-system) ; C-x RET k
(describe-function #'set-terminal-coding-system) ; C-x RET t
(describe-function #'set-selection-coding-system) ; C-x RET x
(describe-function #'set-next-selection-coding-system) ; C-x RET X
(describe-function #'set-buffer-process-coding-system) ; C-x RET p

(describe-function #'toggle-input-method) ; C-\
(describe-function #'set-input-method) ; C-x RET C-\
(describe-function #'activate-transient-input-method) ; C-x \



;; Macros
(describe-function #'kmacro-start-macro-or-insert-counter) ; F3
(describe-function #'kmacro-end-or-call-macro) ; F4
(describe-function #'kmacro-start-macro) ; C-x (, C-x C-k s, C-x C-k C-s
(describe-function #'kmacro-end-macro) ; C-x )
(describe-function #'kmacro-end-and-call-macro) ; C-x e

(describe-function #'kmacro-cycle-ring-next) ; C-x C-k C-n
(describe-function #'kmacro-cycle-ring-previous) ; C-x C-k C-p
(describe-function #'kmacro-name-last-macro) ; C-x C-k n
(describe-function #'kmacro-bind-to-key) ; C-x C-k b
(describe-function #'edit-kbd-macro) ; C-x C-k e
(describe-function #'kmacro-step-edit-macro) ; C-x C-k SPC
(describe-function #'kmacro-edit-lossage) ; C-x C-k l
(describe-function #'insert-kbd-macro) ; (default unbound)



;; Version control
(describe-function #'vc-dir) ; C-x v d
(describe-function #'vc-ignore) ; C-x v G
(describe-function #'vc-register) ; C-x v i
(describe-function #'vc-next-action) ; C-x v v
(describe-function #'vc-update) ; C-x v +
(describe-function #'vc-push) ; C-x v P
(describe-function #'vc-revert) ; C-x v u
(describe-function #'vc-insert-headers) ; (default unbound)
(describe-function #'vc-print-root-log) ; C-x v L
(describe-function #'vc-print-log) ; C-x v l
(describe-function #'vc-log-incoming) ; C-x v I
(describe-function #'vc-log-outgoing) ; C-x v O
(describe-function #'vc-update-change-log) ; C-x v a
(describe-function #'vc-root-diff) ; C-x v D
(describe-function #'vc-diff) ; C-x v =
(describe-function #'vc-revision-other-window) ; C-x v ~
(describe-function #'vc-rename-file) ; (default unbound)
(describe-function #'vc-annotate) ; C-x v g
(describe-function #'vc-create-tag) ; C-x v s
(describe-function #'vc-retrieve-tag) ; C-x v r



;; Miscellaneous
(describe-function #'menu-bar-open) ; F10
(describe-function #'toggle-frame-maximized) ; M-F10
(describe-function #'toggle-frame-fullscreen) ; F11
(describe-function #'text-scale-adjust) ; C-x C-0, C-x C--, C-x C-=, C-x C-+

(describe-function #'compose-mail) ; C-x m

(describe-function #'edit-tab-stops) ; (default unbound)
(describe-variable 'tab-stop-list)
(describe-function #'tab-to-tab-stop) ; unbound (default M-i)

(describe-function #'highlight-symbol-at-point) ; M-s h .
(describe-function #'highlight-phrase) ; M-s h p
(describe-function #'highlight-regexp) ; M-s h r
(describe-function #'unhighlight-regexp) ; M-s h u

(describe-function #'async-shell-command) ; M-&
(describe-function #'shell-command) ; M-!
(describe-function #'shell-command-on-region) ; M-|

(describe-function #'universal-argument) ; C-u

(describe-function #'dabbrev-expand) ; M-/
(describe-function #'dabbrev-completion) ; C-M-/
(describe-function #'ispell-word) ; M-$
(describe-function #'flyspell-auto-correct-word) ; C-., C-M-i

(describe-variable 'fill-prefix)
(describe-function #'set-fill-prefix) ; C-x .
(describe-function #'set-fill-column) ; C-x f
(describe-function #'fill-paragraph) ; M-q
(describe-function #'fill-forward-paragraph) ; (default unbound)
(describe-function #'fill-region) ; (default unbound)
(describe-function #'fill-region-as-paragraph) ; (default unbound)



;; Unbound functions
(describe-function #'auto-fill-mode)
(describe-function #'display-fill-column-indicator-mode)
(describe-function #'hl-line-mode)
(describe-function #'whitespace-mode)
(describe-function #'view-mode)
(describe-function #'visual-line-mode)
(describe-function #'variable-pitch-mode)
(describe-function #'buffer-face-mode)
(describe-function #'fido-mode)
(describe-function #'fido-vertical-mode)

(describe-function #'flyspell-mode)
(describe-function #'flyspell-prog-mode)
(describe-function #'ispell-buffer)
(describe-function #'ispell-region)

(describe-function #'compare-windows)
(describe-function #'ediff)

(describe-function #'append-to-buffer)
(describe-function #'copy-to-buffer)
(describe-function #'erase-buffer)
(describe-function #'eval-buffer)

(describe-function #'forward-button)
(describe-function #'forward-comment)
(describe-function #'forward-line)
(describe-function #'forward-same-syntax)
(describe-function #'forward-symbol)
(describe-function #'forward-thing)
(describe-function #'forward-to-indentation)
(describe-function #'forward-to-word)
(describe-function #'forward-visible-line)
(describe-function #'forward-whitespace)
(describe-function #'forward-word-strictly)

(describe-function #'backward-button)
(describe-function #'backward-delete-char)
(describe-function #'backward-kill-paragraph)
(describe-function #'backward-prefix-chars)
(describe-function #'backward-to-indentation)
(describe-function #'backward-to-word)
(describe-function #'backward-word-strictly)

(describe-function #'clipboard-kill-region)
(describe-function #'clipboard-kill-ring-save)
(describe-function #'comment-kill)
(describe-function #'copy-region-as-kill)
(describe-function #'current-kill)
(describe-function #'isearch-yank-from-kill-ring)
(describe-function #'kill-all-abbrevs)
(describe-function #'kill-all-local-variables)
(describe-function #'kill-append)
(describe-function #'kill-backward-chars)
(describe-function #'kill-backward-up-list)
(describe-function #'kill-buffer-ask)
(describe-function #'kill-buffer-if-not-modified)
(describe-function #'kill-comment)
(describe-function #'kill-current-buffer)
(describe-function #'kill-emacs)
(describe-function #'kill-forward-chars)
(describe-function #'kill-local-variable)
(describe-function #'kill-matching-buffers)
(describe-function #'kill-new)
(describe-function #'kill-paragraph)
(describe-function #'kill-process)
(describe-function #'kill-some-buffers)
(describe-function #'kill-this-buffer)
(describe-function #'kill-visual-line)
(describe-function #'mouse-kill)
(describe-function #'mouse-kill-ring-save)
(describe-function #'mouse-kill-secondary)
(describe-function #'mouse-save-then-kill-delete-region)
(describe-function #'read-from-kill-ring)
(describe-function #'save-buffers-kill-emacs)
(describe-function #'yank-from-kill-ring)

(describe-function #'sort-lines)
(describe-function #'sort-fields)
(describe-function #'sort-numeric-fields)
(describe-function #'sort-columns)
(describe-function #'sort-paragraphs)
(describe-function #'sort-regexp-fields)
 
(describe-function #'align)
(describe-function #'align-current)
(describe-function #'align-regexp)

(describe-function #'compile)
(describe-function #'shell)
(describe-function #'eshell)
(describe-function #'ansi-term)
(describe-function #'proced)

(describe-function #'current-time)
(describe-function #'time-convert)
(describe-function #'format-time-string)

(describe-function #'file-exists-p)
(describe-function #'byte-compile-file)
(describe-function #'native-compile)
(describe-function #'file-attributes)
(describe-function #'directory-files-and-attributes)
(describe-function #'directory-files)

(describe-function #'grep)
(describe-function #'grep-find)
(describe-function #'lgrep)
(describe-function #'rgrep)
(describe-function #'rzgrep)

(describe-function #'find-dired)
(describe-function #'find-name-dired)
(describe-function #'find-grep-dired)
(describe-function #'find-lisp-find-dired)

(describe-function #'fundamental-mode)
(describe-function #'text-mode)
(describe-function #'paragraph-indent-text-mode)
(describe-function #'indented-text-mode)
(describe-variable 'adaptive-fill-mode)
(describe-function #'subword-mode)
(describe-function #'emacs-lisp-mode)
(describe-function #'lisp-mode)
(describe-function #'tcl-mode)
(describe-function #'awk-mode)
(describe-function #'m4-mode)
(describe-function #'python-mode)
(describe-function #'outline-mode)
(describe-function #'org-mode)

(describe-function #'speedbar-mode)
(describe-function #'windmove-mode)
(describe-function #'menu-bar-mode)
(describe-function #'tool-bar-mode)
(describe-function #'tab-bar-mode)

(describe-function #'modify-frame-parameters)
(describe-function #'set-frame-parameter)
(describe-function #'set-frame-height)
(describe-function #'set-frame-width)

(describe-variable 'user-real-login-name)
(describe-variable 'user-login-name)
(describe-variable 'initial-environment)
(describe-variable 'process-environment)
(describe-function #'setenv)
(describe-function #'getenv)
(describe-variable 'package-archives)
(describe-variable 'exec-path)
(describe-function #'quote)
(describe-function #'add-to-list)
(describe-variable 'custom-theme-load-path)
(describe-variable 'custom-theme-directory)
(describe-variable 'user-emacs-directory)
(describe-variable 'user-init-file)

(describe-function #'menu-set-font)

(describe-function #'list-faces-display)
(describe-function #'list-colors-display)
(describe-function #'customize)
(describe-function #'customize-face)
(describe-function #'customize-variable)
(describe-function #'customize-group)
(describe-function #'customize-create-theme)
(describe-function #'customize-themes)

(describe-variable #'default-frame-alist)
(customize-variable #'default-frame-alist)

;; Recursion: You may want to increase the values of `max-specpdl-size' and
;; `max-lisp-eval-depth'.  In my init file, I set them to 15 and 30 times
;; their default value: Elisp Intro (eintr), Loops & Recursion (chapter 11)
(describe-variable 'max-specpdl-size)
(* max-specpdl-size 15) ; C-x C-e to get result in *Messages* buffer
(customize-variable 'max-specpdl-size) ; multiply by 15
(describe-variable 'max-lisp-eval-depth)
(* max-lisp-eval-depth 30) ; C-x C-e to get result in *Messages* buffer
(customize-variable 'max-lisp-eval-depth) ; multiply by 30

(describe-function #'deftheme)
(describe-function #'provide)
(describe-function #'require)
(describe-function #'load)
(describe-function #'load-file)
(describe-function #'load-theme)

(describe-function #'makunbound)
(describe-function #'fmakunbound)
(describe-function #'defun)
(describe-function #'defvar)
(describe-function #'defcustom)
(describe-function #'custom-set-variables)
(describe-variable 'eval-expression-print-length)
;; (custom-set-variables '(eval-expression-print-length nil)) ; example
(describe-function #'setq-default)
(describe-function #'setq)
(describe-function #'set-variable)
(describe-function #'type-of)

(describe-function #'kbd)
(describe-function #'global-unset-key)
(describe-function #'global-set-key)
(describe-function #'local-unset-key)
(describe-function #'local-set-key)



;; Local Variables:
;; mode: emacs-lisp
;; fill-column: 80
;; End:

;;; emacs-hotkeys.el ends here
