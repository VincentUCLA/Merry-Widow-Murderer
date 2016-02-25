(setq EP-MEM '(
    (UNCLE-OF AGENT (CHARLES-1) 
              OBJECT (CHARLOTTE-1))
    
    (MYSTERIOUS AGENT (CHARLES-1))
    
    (STORY OBJECT (PRINTED-1)
           ABOUT (CRIME-1))
    
    (SEE AGENT (CHARLOTTE-1)
         OBJECT (THROW AGENT (CHARLES-1)
                       OBJECT (PRINTED-1)
                       INTO (TRASH-1)
                       MANNER (SECRETLY))
         LOC (HOME-1))
    
    (IDENTITY AGENT (UNKNOWN)
              OF (CRIME-1))
    
    ; Your test entries here! Feel free to modify
    ; this variable as I'll replace it with my own during
    ; tests
))

(defun VAR (x)
	(if (and (equal (length x) 2)
			 (equal (first x) 'V)
			 (symbolp (second x))) T
		nil))

(defun frame (x)
	(if (and (or (oddp (length x))
			(equal x nil))
			(not (listp (first x)))) t
			nil))

(defun listof (x)
	(if (listp (first x)) t nil))

(defun front-slot (frame)
    (second frame))

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame)))

(defun rm-slot (slot frame)
    (cond
        ; Base case: no slots left, so we're done
        ((<= (length frame) 1) frame)
        ; Base case: front slot matches, so just pop it
        ((equal (front-slot frame) slot) (pop-slot frame))
        ; Recursive case: front slot doesn't match, so keep looking
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))))

(defun SEAR (phrase sent &optional (i 0))
	(cond 	((> (+ i 1) (length sent)) -1)
			((equal (nth i sent) phrase) i)
			(t (SEAR phrase sent (+ i 1)))))

(defun SEARV (sent &optional (i 0))
	(cond 	((> (+ i 1) (length sent)) -1)
			((listp (nth i sent))
			(cond ((equal (first (nth i sent)) 'V) i)))
			(t (SEARV sent (+ i 1)))))

(defun SF-UNIFY  (farg1 farg2 &optional (beta '(T)))
	(cond 
		((equal beta nil) nil)
		;if var
		((var farg1) unify-var(x, y, beta))
		((var farg2) unify-var(y, x, beta))
		;if frame
		((equal (frame farg1) t)
			(if (equal (frame farg2) t)
			  (let ((i (sear (front-slot farg1) farg2)))
			  (if (equal i -1) nil
			  	(let ((arg1 (front-filler farg1))
			  		  (arg2 (nth (+ i 1) farg2)))
			  	(cond
				  	;if simply an atom	
			  		((= (length arg1) 1)
			  		(if (not (equal arg1 arg2)) nil))
			  		;if one is frame
			  		((equal (frame arg1) t)
			  			;if another one is also frame
			  			(if	(and (equal (frame arg2) t)
			  					 (not (equal (length arg2 1)))
			  				(SF-UNIFY arg1 arg2 beta)
			  				nil))
			  		;if var
			  		((var arg1)
			  			(if (var arg2)
			  				(SF-UNIFY arg1 arg2 beta)
			  				nil))))
			  	(let ((newfarg1 (rm-slot (front-slot farg1) farg1))
			  		(newfarg2 (rm-slot (nth i farg2) farg2)))
			  		(SF-UNIFY newfarg1 newfarg2 beta)))))
			  nil))))
			  

(setq FRM-0 
    '(SEE AGENT (A)
          OBJECT (THROW AGENT (B)
                        OBJECT (D)))
)

(setq FRM-1
    '(SEE AGENT (A)
          LOC (HOME-1)
          OBJECT (THROW OBJECT (D)
                        AGENT (B)))
)