;;;; sem-parser.el - Parser for the command language

;; Inspired by Alex Schroeder's xpath-parser.el.
 
(defconst sep-table
  (wisent-compile-grammar
   '((NUM STR UNDO-CMD)			; we use characters instead of symbols
					; because '$' is reserved in wisent
					; grammars

     ((left ?, ?\;)                     ; + and - are higher precedence than ,
      (left ?+ ?-)			; and ;
      )		

     (expression
      ((address command)
       (list $2 $1))
      ((address)
       (list '(p) $1))			; p is the default command
      ((command)
       (list $1 '(DOT)))		; dot is the default address
      )

     (command
      ((operation)			; d, k, u
       (list $1))
      ((operation ?/ STR ?/)    	; a,c,i cmds. We should really allow any
					; printable character except
					; alphanumerics as delimeter
       (list $1 $3))
      ((operation 13 STR ?.)		; multi-line text commands (does not work yet)
       (list $1 $3))
     ;;  ((operation ?/ STR ?/ STR ?/) ; s cmd conflict?! C.f. dangling else or
;;        use separate token (list $1 $3 $5))

      ;; u command, most have its own token to not have its argument confused
      ;; with a m,t line address
      ((UNDO-CMD NUM)			
       (list 'u (string-to-number $2)))
      ((UNDO-CMD)			
       (list 'u 1))
      ((operation address)		; m,t commands
       (list $1 $2))
;;       ((operation STR)	         	; I/O cmds, file cmds
;;        (list $1 $2))
      ;; Loops and conditionals
      ((operation ?/ STR ?/ command)	; x, y, X, Y, g, v
       (list $1 $3 $5))
;;       ((operation command)              ; loop command with defaults, set in the commands
;; 			                ; function
;;        (list $1 $2))
      ;; Group of commands
      ((?{ sequence ?})
       (append (list '{) $2))
      )

     (sequence
      ((command)			; it seems like sam allows full
					; expressions here, and that the dot
					; used in commands are independant
       $1)
      ((sequence ?\n command)
       (list $1 $3))
      )

     (operation
      ((STR)
       (intern $1))
      )

     (address		
      ((address ?, address)	
       (list 'COMMA $1 $3))
      ((address ?,)
       (list 'COMMA $1 '(EOF)))
      ((?, address)
       (list 'COMMA (LINE 0) $2))
      ((?,)
       '(COMMA (LINE 0) (EOF)))
      ((address ?\; address)
       (list 'SEMICOLON $1 $3))
      ((address ?+ address)
       (list 'PLUS $1 $3))
      ;; can all these default be specified in a more efficient way?
      ((address ?+)
       `(PLUS ,$1 (LINE 1)))
      ((?+ address)
       `(PLUS (DOT) ,$2))
      ((?+)
       '(PLUS (DOT) (LINE 1)))
      ((address ?- address)
       (list 'MINUS $1 $3))
      ((address ?- )
       `(MINUS ,$1 (LINE 1)))
      ((?- address)
       `(MINUS (DOT) ,$2))
      ((?-)
       '(MINUS (DOT) (LINE 1)))
      ((simple-address))
      )			

     (simple-address
      ((?# NUM)
       (list 'CHAR (string-to-number $2)))
      ((NUM)
       (list 'LINE (string-to-number $1)))
      ((?/ STR ?/)		
       (list 'REGEX $2))
      ((?? STR ??)
       (list 'REV-REGEX $2))
      ((?$)
       '(EOF))
      ((?.)
       '(DOT))				; rename this to 'region'?
      ((?')
       '(MARK))
      ((?\" STR ?\")		       
       (list 'BUF-NAME-REGEX $2)))
     )
   'nil)				; no %start declarations
  "Parser automaton for Sam.

The parse tree consists of (<ADDRESS> <COMMAND> [ARGS]).
Currently doesn't handle escaped slashes in regexpes (which are
matched as strings by the lexer), spaces in strings etc...")

(defconst sep-number-re
  (rx (one-or-more digit))
  "Regex matching number.")

(defun sep-text-re (delimiter)
  "Regex lazily matching everything up to the DELIMITER character." 
  (rx (group (*? anything)) (eval delimiter)))

(defconst sep-op-re
  (rx (char ?# ?/ ?\" ?? ?$ ?. ?' ?+ ?- ?, ?\; ?{ ?} ?\n))
  "Regex matching operators used with commands.")

(defconst sep-command-re
  (rx (group (char alpha ?= ?< ?> ?| ?!)) (or (any blank ?/) eos eol))
  "Regex matching the commands.")

;; assumes match data is set
(defun sep-lex-advance-and-return (token &optional return step)
  "Move forward and return the token as appropriate for parser.
This function assumes that the match-data are set appropriately.

RETURN is the number of the submatch which determines parts of
the value returned. It defaults to 0. STEP is the submatch to
whose end point we will move, it defaults to the value of RETURN."
  (or return (setq return 0))
  (goto-char (match-end (or step return))) 
  (let ((str (match-string-no-properties return)))
    (list token str)))

(defun sep-next-token (prev-token)
  "Sam language tokenizer"
  (skip-chars-forward "\x20") 
  (cond 
   ((looking-at (rx eos))
    (list wisent-eoi-term))
   ;; operator?
   ((looking-at sep-op-re)
    ;; convert string to symbol for use when parsing
    (sep-lex-advance-and-return (string-to-char (match-string-no-properties 0))))
   ;; number?
   ((looking-at sep-number-re)
    (sep-lex-advance-and-return 'NUM))
   ;; undo command? (needs special treatment in order to not confuse parser)
   ((looking-at (rx (group (char ?u)) (or (any blank ?/) eos eol))) 
    (sep-lex-advance-and-return 'UNDO-CMD 1))
   ;; command? (would not catch the 'cd' command yet)
   ;; A single character followed by something that is not a command character (e.g. ?/ eol)
   ;; Should make a special command class for the command characters
   ((looking-at sep-command-re) 
    (sep-lex-advance-and-return 'STR 1))
   ;; ["?/]-delimited string? In sam, every character except an alphanumeric
   ;; would do. Make a new token class to handle that, called something like
   ;; DELIM
   ((and (memq prev-token '(?/ ?\" ??))
	 (looking-at (sep-text-re prev-token)))
      (sep-lex-advance-and-return 'STR 1))
   (t
      (error "Could not reckognize token!"))))

(setq prev-token nil)
(defun print-next-token ()
  ;; call sep-next-token until we are finished instead?
  (interactive)
  (message (format "%S"
		   (funcall
		      (lambda ()
			(let ((token (sep-next-token prev-token)))
			  (setq prev-token (car token))
			  token)))))) 

(defun parse-line ()
  (interactive)
  (message (format "%S"
		   (let (last-token)
		     (wisent-parse sep-table 
				   (lambda ()
				     (let ((token (sep-next-token last-token)))
				       (setq last-token (car token))
				       token)))))))

;;;; Tests
(defmacro sep-assert (expr)
  `(unless ,expr
     (error "Test failed: %S" ',expr)))

(defun parse-str (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (last-token)
      (wisent-parse sep-table
		    (lambda ()
		      (let ((token (sep-next-token last-token)))
			(setq last-token (car token))
			token))))))

(defun sep-test-lex-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (last-token list)
      (while (not (eobp))
	(let ((token (sep-next-token last-token)))
	  (setq last-token (car token))
	  (push token list)))
      (nreverse list))))
					
