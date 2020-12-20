(require 'rx)
(require 'font-lock)

(defcustom number-separator "," "Separator between integer value.") 
(defcustom number-separator-interval 3 "How to group numbers.")
(defcustom number-separator-ignore-threshold 4 "How many digits should there be before adding separators?")
(defcustom number-separator-decimal-char "." "Character separating integer from decimal value.")

(defvar number-separator--font-lock-keyword
  `((,"\\.?[[:digit:]]+"
     (0 (list
	 'face nil
	 'display (number-separator)))))
  "Font lock keyword to find decimal numbers.")

(defun number-separator ()  
  "Fix the number and return a new string."
  (let ((number (match-string 0)))
    (if (or (string-prefix-p "." number)
	    ;; Don't check a fractional alone,
	    ;; and don't check if not enough digits
	    (<= (length number) number-separator-ignore-threshold))
	nil ;; Don't set the display property 
      (let* ((separator (if (string= "." number-separator-decimal-char)
			    ;; `split-string' needs a regexp as a separator
			    "\\."
			  number-separator-decimal-char))
	     (split-number (save-match-data
			     ;; `split-string' messes with match-data
			     ;; Not sure if it's necessary to save it.
  			     (split-string number separator)))
	     (integer (car split-number))
	     (fractional (cadr split-number)))
	(when (> (length integer) number-separator-ignore-threshold)
	  (concat 
	   (cl-loop for x from (- (length integer) number-separator-interval)
		    downto 1
		    by number-separator-interval
		    do (setq integer
			     (concat (substring integer 0 x)
				     number-separator
				     (substring integer x (length integer))))
		    finally return integer)
	   (when fractional
	     (concat number-separator-decimal-char
		     fractional))))))))

(define-minor-mode number-separator-mode
  "Separate long numbers."
  nil
  " numsep"
  nil  
  (if number-separator-mode
      (progn 
	(push 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil number-separator--font-lock-keyword))
    (font-lock-remove-keywords nil number-separator--font-lock-keyword)))

(provide 'number-separator)




