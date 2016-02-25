;;; CS 161 Spring 2014: Example Test Framework
;;; Skeleton by Evan Lloyd; modified by Andrew Forney

; Load your solution here!
(load "hw-1-solution.lsp")

; C1: "Teenager Charlotte Newton feels excited."
(setq C1 '(STATE TYPE (EMOTION SENTIM (POS)
                               SCALE (>NORM)) 
                 AGENT (HUMAN F-NAME (CHARLOTTE)
                              L-NAME (NEWTON)
                              GENDER (FEMALE)
                              AGE (RANGE FROM (13)
                                         TO (19)
                                         UNIT (YEAR)))))

(setq F-NAME001 '(FRANK))

(setq HUM001 '(HUMAN F-NAME F-NAME001 GENDER (MALE)))

; [!] TODO: Function to check if frames are equal
; Left as an exercise! I can't do everything :)
(defun fr-equal (fr1 fr2)
    (cond
        ; Base case: empty frames match
        ((and (null fr1) (null fr2)) t)
      
        ; ???
        ; More cases will have to go here for completing the function!
        
        ; Obviously this case isn't correct :)
        (t nil)
    )
)

; Test function; make sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
    (cond
        ((equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; [!] TODO: Test function, with equality replaced with frame-equality
; Requires completion of fr-equal above to be functional
(defun test-case-fr (actual expected case-name)
    (cond
        ((not (and (listp actual) (listp expected))) t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
        ((fr-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; -----------------------------------------------------------------------------

; Test cases: Function 1 (FILLER)
(format t "Testing FILLER...~%")

(test-case (FILLER 'SRC C1) NIL "filler_ex_1")
(test-case (FILLER 'L-NAME '(HUMAN F-NAME (CHARLES)
                             L-NAME ( ) 
                             GENDER (MALE))) NIL "filler_ex_2")
(test-case (FILLER 'F-NAME HUM001) 'F-NAME001 "filler_ex_3")

(format t "===========================~%")

; -----------------------------------------------------------------------------

; ... OTHER UNIT TESTS HERE ...

