;;; sep.el - Sam Emulation Package

;; Author: Tobias Gerdin <tobias.gerdin@gmail.com>

;;; Commentary:

;; Differences from original sam:
;; - Regular expressions are Emacs.
;; - Undo is Emacs.
;; - The minibuffer is used instead of a dedicated command window
;;
;; Currently unsupported commands:
;; - s
;; - File (including capital X and Y) and I/O commands
;; - q 
;; - cd

;;; Development plan:

;; Milestone 1: Implement parts of the sam command language related to buffer text 
;; Milestone 2: Add editing popup menu (bind to mouse-3?)
;; Milestone 3: Add the rest of Sam commands

;;; Known bugs:
;; - the region is not highlighted after dot is set by text commands

;;; Code
(setq load-path (cons "~/sep" load-path))
(load "sep-parser")
(load "sep-addr-eval")

;; Sam behaves this way
(delete-selection-mode t)

(defvar sep-address-mark
  (make-addr nil nil)
  "The sam address mark, set by the 'k' command.")

(defalias 'start-of 'addr-left)
(defalias 'end-of 'addr-right)

(defun contents (dot)
  (buffer-substring (start-of dot) (addr-right dot)))

(defmacro with-new-dot (&rest exprs)
  (declare (indent defun))
  `(progn
     (push-mark (point))
     ,@exprs
     (setq mark-active t)))

;;; Text command evaluators. It seems like all of them start with GOTO-CHAR...
;;; Contract: They must leave the point at the end of whatever they did

;; idea: make with-new-dot return the new dot as a cons cell instead of mutating
;; region, would make composition easier?
(defun sep-append (dot text)
  "Inserts TEXT after the DOT, and sets dot to the inserted text"
  (goto-char (end-of dot))
  (with-new-dot
    (insert text)))

;; todo: We should catch '\n' in strings
(defun sep-change (dot text)
  (goto-char (start-of dot))
  (delete-region (start-of dot) (end-of dot))
  (with-new-dot
    (insert text)))

(defun sep-insert (dot text)
  "Inserts text before the DOT, and sets dot to the inserted text"
  (goto-char (start-of dot))
  (with-new-dot
    (insert text)))

(defun sep-delete (dot)
  "Deletes contents of DOT."
  (delete-region (start-of dot) (end-of dot))) ; Apparently puts text in kill
					       ; ring, which is where we want
					       ; it.

(defun sep-substitute (dot regexp text)
  "Substitutes first match of REGEXP in DOT with TEXT. Set dot to the modified
range.

Use 'x/re/c/text/' to perform a global substitution."
  (goto-char (start-of dot))
  (when (re-search-forward regexp (end-of dot))
    (goto-char (start-of dot))
    (with-new-dot 
      (replace-match text)
      (when (> (end-of dot) (point))
	(goto-char (end-of dot))))))

(defun sep-move (dot addr)
  "Move text in dot to after address."
  ;; It would be easier to insert the text before we delete it but then the
  ;; function would not terminate using WITH-NEW-DOT, so we go through some
  ;; small hoops in order to delete it first.
  (let ((dest-addr-marker (set-marker (make-marker)
				      (addr-right (eval-address addr)))))
    (sep-delete dot)
    (goto-char dest-addr-marker)
    (with-new-dot 
      (yank))))

(defun sep-copy (dot addr)
  "Copy text in dot to after address."
  (goto-char (addr-right (eval-address addr)))
  (with-new-dot 
    (insert (contents dot))))

(defun sep-dot-to-region (addr)
  "Sets the Emacs region to the address specified by ADDR, for
integration with normal Emacs commands that act on the region.

This is also a convenient way to simply visualize the extents of
a Sam address."
    (set-mark (start-of addr))
    (goto-char (end-of addr))
    (setq mark-active t))		; This is suppose to highlight region if
					; transient-mark-mode is active, but
					; that does not seem to be the case.
  
;;; Display commands
(defun sep-print (dot)
  "Prints contents of dot."
  (message (contents dot)))

(defun sep-value (dot)
  "Prints value of dot."
  (message (format "%d; #%d,#%d" (line-number-at-pos) (start-of dot) (end-of dot))))

;;; Loops & conditionals
(defun sep-extract (dot regexp command)
  "For each match of REGEXP, set dot and run COMMAND. Set dot to the last match."
  (goto-char (start-of dot))
  ;; The third argument refers to a position before the search started, so 
  ;; we put a mark there before we start and refer to this mark.
  (let ((search-end-marker (set-marker (make-marker) (end-of dot))))
    (while (posix-search-forward regexp search-end-marker t)
      ;; Here we rely on the fact that commands leave the point at the "right"
      ;; position.
      (apply (lookup-operation (command-operation command) cmd-table)
	     (make-addr (match-beginning 0) (match-end 0))
	     (command-args command)))))

;; Considerably more hairy than its twin brother above.
(defun sep-extract-in-between (dot regexp command)
  "Between adjacent matches of REGEXP, set dot and run COMMAND.

This is the complement of SEP-EXTRACT."
  (goto-char (start-of dot))
  (let ((search-end (set-marker (make-marker) (end-of dot)))
        (prev-match-end (make-marker)))
    (while (posix-search-forward regexp search-end t)
      ;; The below does not look so nice, maybe it could be written better..
      (if (marker-position prev-match-end)
	  (let ((command-addr (make-addr (copy-marker prev-match-end) (match-beginning 0))))
	    (set-marker prev-match-end (match-end 0))
	    (apply (lookup-operation (command-operation command) cmd-table)
		   command-addr
		   (command-args command)))
	(set-marker prev-match-end (match-end 0))))))

(defun sep-guard (dot regexp command)
  "If DOT contains a match of REGEXP, run COMMAND."
  (goto-char (start-of dot))
  (when (re-search-forward regexp (end-of dot) t) ; don't signal error if not found
    (apply (lookup-operation (command-operation command) cmd-table)
	   dot
	   (command-args command))))

(defun sep-inverted-guard (dot regexp command)
  "If DOT does not contain a match of REGEXP, run COMMAND."
  (goto-char (start-of dot))
  (unless (re-search-forward regexp (end-of dot) t) ; don't signal error if not found
    (apply (lookup-operation (command-operation command) cmd-table)
	   dot
	   (command-args command))))

;;; Miscellany
(defun sep-set-address-mark (dot)
  (defalias 'dot-to-addr 'identity)
  (setq sep-address-mark (dot-to-addr dot)))

(defun sep-sequence (dot &rest commands)
  "Handles sequences of COMMANDS."
  ;; I find it somewhat unclear how this is supposed to work. Apparently the dot
  ;; should not be propagated from a command to the next command. Instead they
  ;; should use the enclosing loop command's dot, and although they are executed
  ;; in sequence one commands changes should not be visible to another.
  ;;
  ;; This could probably be implemented (as usual) using markers. Create markers
  ;; for the enclosing dot and give these markers as dot the subcommand. Simple!
  ;; In fact, perhaps we should always represent dot as markers...
  ;;
  ;; However, in this case successive appends end up the wrong order. Could we
  ;; just iterate in the reverse direction? Or use another type of mark?
  (let ((dot-start (set-marker (make-marker) (start-of dot)))
	(dot-end (set-marker (make-marker) (end-of dot))))
    (dolist (command commands)
      (apply (lookup-operation (command-operation command) cmd-table)
	     (make-addr dot-start dot-end)
	     (command-args command)))))

;; A hash table would be more approriate but they do not seem to provide any
;; nice initializer. Or maybe a char-table??
(defconst cmd-table
  '((a . sep-append)
    (c . sep-change)
    (i . sep-insert)
    (d . sep-delete)
    (s . sep-substitute)
    (m . sep-move)
    (t . sep-copy)
    (h . sep-dot-to-region)		; bonus command, for Highlight
    (p . sep-print)
    (= . sep-value)
    (x . sep-extract)
    (y . sep-extract-in-between)
    (g . sep-guard)
    (v . sep-inverted-guard)
    (k . sep-set-address-mark)
    (u . (lambda (dot n) (undo n)))
    ({ . sep-sequence))
  "Lookup table for commands.")

;; Use of read-from-minibuffer would allow us to install our own keymap so that
;; we can catch newlines and the like (for use with 'a <text> .' and braces
;; commands).
(defun my-exit-minibuf ()
  (interactive)
  ;; Here we should check if what we have got so far satisfies the parser. If
  ;; not we should wait for the next CR.
  (exit-minibuffer))

(defun sep-read-command ()
  (interactive)
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap minibuffer-local-map)
    ;; catch return key
    (define-key keymap "\r" 'my-exit-minibuf)
    (apply 'sep-eval (parse-str (read-from-minibuffer "sam: " nil keymap)))))

(defun sep-eval-str (input)
  (interactive "ssep: ")		; For now, to not confuse it with Viper
  (apply 'sep-eval (parse-str input)))

;(define-key viper-vi-global-user-map ";" 'sep-command)
(global-set-key '[(C :)] 'sep-read-command)	; C.f. M-:

(defalias 'command-operation 'car)
(defalias 'command-args 'cdr)

(defun lookup-operation (key table)
  "Looks up procedure in alist and returns it."
  (cdr (assq key table)))

(defun sep-eval (command-form addr-form) ; C.f. (eval exp env)
  ;; dispatch on command
  (apply (lookup-operation (command-operation command-form) cmd-table)
	 (eval-address addr-form) ; perhaps we should delegate evaluation of
				  ; address to commands, i.e. pass lazily
	 (command-args command-form)))

