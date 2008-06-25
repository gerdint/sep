;;; sep-addr-eval.el - Functionality for evaluating addresses

;; address ADT
(defalias 'make-addr 'cons)
(defalias 'addr-left 'car)
(defalias 'addr-right 'cdr)

(defun eval-regex-addr (pos dir re)
  (save-excursion
    ;; Regex addresses are relative to point by
    ;; default, so we only move if we should.
    (unless (zerop pos)			; 0 is the default position
      (goto-char pos))
    ;; todo: these searches should wrap!
    (cond
     ((= dir 1)
      (re-search-forward re))
     ((= dir -1)
      (re-search-backward re)))
    (make-addr (match-beginning 0)
	       (match-end 0))))

(defconst addr-op-table
  '(
    ;;; Simple addresses 

    ;; Sam #N means point should fall AFTER character N, whereas (goto-char N)
    ;; will leave the mark BEFORE character N. So we add 1.
    (CHAR . (lambda (pos dir n) (let ((point (+ pos (* dir (1+ n)))))
				  (make-addr point point))))
    ;; Line 0 should be interpreted as the empty string at the start of the buffer
    (LINE . (lambda (pos dir n) (save-excursion
				  (goto-char pos)
				  (forward-line (* dir 
						   (if (zerop pos)
						       (1- n) ; if not relative,
							      ; we are already
							      ; on line 1
						       n)))
				  (make-addr (line-beginning-position)
					     ;; do we want to include LF as
					     ;; well?  when deleting lines, they
					     ;; should go away, but when
					     ;; appending to a line it should
					     ;; end up at the end of the line
					     (line-end-position)))))
    (REGEX . eval-regex-addr)
    (REV-REGEX . (lambda (pos dir re) (eval-regex-addr pos (* -1 dir) re))) 
    (EOF . (lambda (pos dir) (make-addr (point-max) (point-max))))
    (DOT . (lambda (pos dir) (make-addr (region-beginning) (region-end))))
    (MARK . (lambda (pos dir) sep-address-mark))
    (BUF-NAME-REGEX . not-implemented)

    ;;; Compound addresses
    (PLUS . (lambda (pos dir a1 a2) (eval-address a2 (addr-right (eval-address a1)))))
    (MINUS . (lambda (pos dir a1 a2) (eval-address a2 (addr-left (eval-address a1)) -1)))
    (COMMA . (lambda (pos dir a1 a2) (make-addr (addr-left (eval-address a1))
						(addr-right (eval-address a2)))))
    (SEMICOLON . not-implemented)	; don't understand how this one is
					; supposed to work
    )
  "A mapping from address operator symbol to its evaluation.")
 
;; compile address to buffer start-end positions
(defun eval-address (expr &optional pos dir)
  "Computes the address (in terms of buffer positions) described by EXPR.

The expression is evaluated relative to POS (default: 0) in the
direction of the 1-dimensional vector DIR (default: 1, meaning
forward towards the end of the buffer)."
  (defalias 'addr-op 'car)
  (defalias 'addr-arg 'cdr)
  (apply (lookup-operation (addr-op expr) addr-op-table)
	 (or pos 0) (or dir 1) (addr-arg expr)))
