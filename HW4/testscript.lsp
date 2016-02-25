
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

(setq FRM-2
    '(SEE AGENT (CHARLOTTE-1)
          OBJECT (THROW AGENT (CHARLES-1)
                        OBJECT (V Y1)
                        INTO (TRASH-1))
          LOC (V LOC1))
)

(setq FRM-3
    '(SEE AGENT (V X1)
          LOC (HOME-1)
          OBJECT (THROW AGENT (CHARLES-1)
                        OBJECT (PRINTED-1)
                        INTO (TRASH-1)
                        MANNER (SECRETLY)))
)

(setq FRM-4 '(V X))

(setq FRM-5
    '(KILLS AGENT (V Z1)
            OBJECT (V Z1))
)

(setq FRM-6
    '(KILLS AGENT (EMMA-1)
            OBJECT (V Z2))
)

(setq L-FRMS-1
    '((SEE AGENT (V AGG1)
           OBJECT (THROW AGENT (V AGG2)
                         OBJECT (V OBJJ1)
                         INTO (V INTOO1)
                         MANNER (SECRETLY))
           LOC (V LOCC1))
      
      (MYSTERIOUS AGENT (V AGG2))
      
      (STORY OBJECT (V OBJJ1) 
             ABOUT (CRIME)))
)

(setq L-FRMS-2
    '((SEE AGENT (CHARLOTTE-1)
           OBJECT (THROW AGENT (CHARLES-1)
                         OBJECT (PRINTED-1)
                         INTO (TRASH-1)
                         MANNER (SECRETLY))
           LOC (HOME-1))
      
      (STORY OBJECT (PRINTED-1) 
             ABOUT (CRIME))
      
      (MYSTERIOUS AGENT (CHARLES-1))  
      
      (UNCLE-OF AGENT (CHARLES-1)
                OBJECT (CHARLOTTE-1)))
)

(defun VAR (x)
    (if (and (equal (length x) 2)
             (equal (first x) 'V)
             (symbolp (second x))) T
        nil))

(defun framep (x)
    (if (and (or (and (oddp (length x)) (> (length x) 1))
                 (equal x nil))
                 (not (listp (first x)))) t nil))

(defun atomp (x)
    (if (and (listp x) (= (length x) 1) (symbolp (first x))) t
        nil))

(defun listof (x)
    (if (listp (first x)) t nil))

(defun front-slot (frame)
    (second frame))
    
(defun front-filler (frame)
    (third frame))

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
    (cond ((> (+ i 1) (length sent)) -1)
          ((equal (nth i sent) phrase) i)
          (t (SEAR phrase sent (+ i 1)))))
       
(defun SEARC (phrase sent &optional (i 0))
    (cond ((> (+ i 1) (length sent)) -1)
          ((equal (first (nth i sent)) phrase) i)
          (t (SEARC phrase sent (+ i 1)))))

(defun SEARV (sent i)
    (cond ((> (+ i 1) (length sent)) -1)
          ((listp (nth i sent))
          (cond ((equal (first (nth i sent)) 'V) i)))
          (t (SEARV sent (+ i 1)))))

(defun appen (beta res)
    (let ((result beta))
    (loop for v in res do
    (cond ((= (searc (first v) result 1) -1) (setf result (append result (list v))))))
    result))
          
(defun SF-UNIFY  (farg1 farg2 &optional (beta '(T)))
    (cond 
        ((equal beta nil) nil)
        ;if var
        ((equal farg1 farg2) beta)
        ((var farg1) (SF-UNIVAR farg1 farg2 beta))
        ((var farg2) (SF-UNIVAR farg2 farg1 beta))
        ;if frame
        ((and (frame farg1) (frame farg2))
            (let ((i (sear (front-slot farg1) farg2)))
                (cond ((= i -1) nil)
                    (t (let* ((arg1 (front-filler farg1))
                              (arg2 (nth (+ i 1) farg2))
                              (res (SF-UNIFY arg1 arg2 beta)))
                    (cond ((equal res nil) nil)
                          (t (let ((newarg1 (rm-slot (front-slot farg1) farg1))
                                   (newarg2 (rm-slot (front-slot farg1) farg2)))
                             (if (atomp newarg1) (appen beta (rest res))
                             (sf-unify newarg1 newarg2 (appen beta (rest res))))))))))))))
                             
(defun SF-UNIVAR (var x beta)
  (let* ((data (rest beta))
         (var1 (second var))
         (var2 (second x))
         (a (searc var1 beta 1))
         (b (searc var2 beta 1)))
    (cond
        ((not (equal a -1)) (SF-UNIFY (second (nth a beta)) x beta))
        ((not (equal b -1)) (SF-UNIFY var (second (nth b beta)) beta))
        (t (append beta (list (list var1 x)))))))

(defun nthcar (n origin)
  (cond 
    ((= n 1)
      (list (first origin)))
    ((> n 1)
      (cons (first origin) (nthcar (- n 1) (rest origin))))))

(defun dele (frame data)
  (let ((i (sear data frame)))
    (append (nthcar i frame) (nthcdr (+ i 1) frame))))

(defun repl (frame data1 data2)
  (let ((i (sear data1 frame)))
    (append (nthcar i frame) (list data2) (nthcdr (+ i 1) frame))))


(defun SF-SUBST (frame beta)
  (let ((bet (rest beta)))
     (loop for v in frame collect
       (cond ((listp v)
         (cond ((framep v) (sf-subst v (append (list t) bet)))
               ((var v) 
                  (let ((i (searc (second v) bet)))
                     (cond ((not (equal i -1))
                            (let ((ith (nth i bet)))
                            (setf bet (dele bet ith))
                            (second ith)))
                           (t v))))(t v)))(t v)))))


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

(defun set-dif (set1 set2)
  (let ((result set1))
  (loop for v in set1 do
    (loop for u in set2 do
    (cond ((equal v u) (setf result (dele result v))))))
  result))

(defun FORWARD1 (rules)
    (let ((new-facts nil))
       (loop for v in rules do
          (let ((new-fact (sf-infer v ep-mem)))
          (cond ((not (equal new-fact nil))(setf new-facts (append new-facts (list new-fact)))))))
       (let ((gamma (set-dif new-facts ep-mem)))
       (cond ((equal (length gamma) 0) nil)
             (t (setq ep-mem (append ep-mem gamma))
                (append gamma (forward1 rules)))))))

(set-difference '((BELIEVES AGENT (CHARLOTTE-1) OBJECT
  (COMMITTED AGENT (CHARLES-1) OBJECT (CRIME-1)))
 (FEELS AGENT (CHARLOTTE-1) STATE (CONFLICT))
 (RELATIVE-OF AGENT (CHARLES-1) OBJECT (CHARLOTTE-1)))9
 '((UNCLE-OF AGENT (CHARLES-1) OBJECT (CHARLOTTE-1))
 (MYSTERIOUS AGENT (CHARLES-1)) (STORY OBJECT (PRINTED-1) ABOUT (CRIME-1))
 (SEE AGENT (CHARLOTTE-1) OBJECT
  (THROW AGENT (CHARLES-1) OBJECT (PRINTED-1) INTO (TRASH-1) MANNER (SECRETLY))
  LOC (HOME-1))
 (IDENTITY AGENT (UNKNOWN) OF (CRIME-1))
 (BELIEVES AGENT (CHARLOTTE-1) OBJECT
  (COMMITTED AGENT (CHARLES-1) OBJECT (CRIME-1)))
 (RELATIVE-OF AGENT (CHARLES-1) OBJECT (CHARLOTTE-1)))
 )
 
(c-gen '(SEE AGENT (CHARLOTTE-1)
           OBJECT (THROW AGENT (CHARLES-1)
                         OBJECT (PRINTED-1)
                         INTO (TRASH)
                         MANNER (SECRETLY))
           LOC (HOME-1))
           eng-pats1
)