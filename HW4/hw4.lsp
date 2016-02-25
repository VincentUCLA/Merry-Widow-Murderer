; CS 161 Spring 2014: HW4 skeleton

; Any functions you use from hw 1 - 3 should be imported
; in your test script rather than this file (for grading
; sake rather than good practice!)

; File structure:
;
; Section 1: Inference Engine:
;   -- PROBLEM 1.1: SF-UNIFY
;   -- PROBLEM 1.2: SF-UNIVAR
;   -- PROBLEM 2:   SF-SUBST
;   -- PROBLEM 3:   SF-INFER
;   -- PROBLEM 4:   FORWARD1
;
; Section 2: QA Engine:
;   -- PROBLEM 5:   BACKWARD1
;   -- PROBLEM 6:   C-GEN
;   -- PROBLEM 7:   C-GENS


; Episodic memory, which is a list of frames designating
; our story Facts
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

; ****** BEGIN INFERENCE ENGINE ******

; -----------------------------------------------------------------------------

; PROBLEM 1.1: SF-UNIFY

; FUNCTION: SF-UNIFY
; PURPOSE:  Perform unification over the two inputs, based on the pseudocode
;           from Fig 9.1, pg. 328 of the textbook. Description also available
;           online at http://aima.cs.berkeley.edu/algorithms.pdf, page 23.
;           Each input is either a variable, frame, or list of frames. As noted
;           in the homework PDF, semantics of frame unification are slightly 
;           different, so your code will not match the pseudocode exactly. 
; OUTPUT:   A binding-list, of the form
;           (T (atom_1 binding_1) (atom_2 binding_2) ...),
;           where each atom_i is a *variable name* (as in, the second part of a
;           filler that looks like "(V name)") and each binding is a frame, 
;           (variable value) pair, or gap.
; INPUTS:   farg1 - variable (V var), frame, or list of frames
;           farg2 - variable (V var), frame, or list of frames
;           beta  - a list of bindings, same format as output
;           (where (T) represents an initial empty binding list)
;             [!] Optional, with default value '(T), so assume a binding
;                 exists unless we find evidence otherwise

(defun VAR (x)
    (if (and (equal (length x) 2)
             (equal (first x) 'V)
             (symbolp (second x))) T
        nil))

(defun framep (x)
    (if (and (or (and (oddp (length x)) (> (length x) 1) (listp (first (last x))))
                 (equal x nil))
                 (not (listp (first x)))) t nil))

(defun atomp (x)
    (if (and (listp x) (= (length x) 1) (symbolp (first x))) t
        nil))

(defun listof (x)
    (if (and (listp (first x)) (not (equal (first x) nil))) t nil))

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
        ((equal farg1 nil) beta)
        ((equal farg1 farg2) beta)
        ((var farg1) (SF-UNIVAR farg1 farg2 beta))
        ((var farg2) (SF-UNIVAR farg2 farg1 beta))
        ;if frame
        ((and (framep farg1) (framep farg2))
            (let ((i (sear (front-slot farg1) farg2)))
                (cond ((= i -1) nil)
                    (t (let* ((arg1 (front-filler farg1))
                              (arg2 (nth (+ i 1) farg2))
                              (res (SF-UNIFY arg1 arg2 beta)))
                    (cond ((equal res nil) nil)
                          (t (let ((newarg1 (rm-slot (front-slot farg1) farg1))
                                   (newarg2 (rm-slot (front-slot farg1) farg2)))
                             (if (atomp newarg1) (appen beta (rest res))
                             (sf-unify newarg1 newarg2 (appen beta (rest res))))))))))))
        ((and (listof farg1) (listof farg2))
            (let ((i (searc (first (first farg1)) farg2)))
                (cond ((= i -1) nil)
                      (t (let ((ith (nth i farg2)))
                      (cond ;((equal (rest farg1) nil) beta)
                      (t (sf-unify (rest farg1) (dele farg2 ith) (sf-unify (first farg1) ith beta)))))))))
        (t nil)))
; -----------------------------------------------------------------------------

; PROBLEM 1.2: SF-UNIVAR

; FUNCTION: SF-UNIVAR
; PURPOSE:  Helper function for SF-UNIFY. See pseudo-code from the textbook
;           algorithm UNIFY-VAR, from the same page as UNIFY referenced in
;           SF-UNIFY. You should not implement or call OCCUR-CHECK?
; OUTPUT:   A binding-list (beta)
; INPUTS:   var  - a variable (list formatted as (V var))
;           x    - frame, variable, or frame list, as in SF-UNIFY
;           beta - a binding list, same format as in SF-UNIFY


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

; -----------------------------------------------------------------------------

; PROBLEM 2: SF-SUBST

; FUNCTION: SF-SUBST
; PURPOSE:  Replace all variables in the given frame with their bindings given
;           in the binding-list beta.
; OUTPUT:   Instantiated frame, where variables have been replaced
; INPUTS:   frame - frame to replace variables in
;           beta  - binding list, of the form from SF-UNIFY
;                   (T (var1 binding1) (var2 binding2) ...)

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

; -----------------------------------------------------------------------------

; PROBLEM 3: SF-INFER

; FUNCTION: SF-INFER
; PURPOSE:  This function takes a single rule and a list of facts, attempts 
;           to unify the premises in the IF-part of rule with a subset of the 
;           facts, and if successful, returns the SF-SUBST of the THEN-part, 
;           using the bindings created by SF-UNIFY.
; OUTPUT:   Consequent frame if conditions met, nil otherwise
; INPUTS:   rule  - of the format:
;             (IF r_1-premise_1 r_1-premise_2 ... r_1-premise_n_1 THEN r_1-conclusion)
;
;           That is, for each rule, the first element is always a literal "IF",
;           the second-to-last element is always a literal "THEN", signifying 
;           logical implication, the last element is the consequent of the 
;           implication, and the premises are the antecedants of the implication
;           (assumed to be conjoined).
;
;           facts - list of 0 or more frames

(defun ruls (rule)
  (if (equal (second rule) 'then) (list (first rule))
    (append (list (first rule)) (ruls (rest rule)))))

(defun SF-INFER (rule facts)
  (let* ((rules (ruls (rest rule)))
         (beta (sf-unify rules facts)))
    (cond ((equal beta nil) nil)
          (t (sf-subst (first (last rule)) beta)))))

; -----------------------------------------------------------------------------

; PROBLEM 4: FORWARD1

; FUNCTION: FORWARD1
; PURPOSE:  This function is a restricted version of forward chaining. FORWARD1 
;           accesses a global variable EP-MEM that contains a list of story 
;           facts. For each rule in rules, FORWARD1 calls on SF-INFER once to 
;           infer a conclusion for that rule. If a conclusion is inferred (or 
;           if SF-INFER returns NIL) then FORWARD1 will go on to try the next 
;           rule, until all rules have been tried.   FORWARD1 will store all 
;           the conclusions in a local variable. Call this variable NEW-FACTS.
;           If NEW-FACTS is not NIL, then FORWARD1 will append NEW-FACTS to the 
;           front of EP-MEM and recursively call itself until no NEW-FACTS are 
;           returned.
;
;           See also FOL-FC-ASK from the text
;           (figure 9.3 on p332, online at http://aima.cs.berkeley.edu/algorithms.pdf
;           page 24) although there are differences, as stated above
; OUTPUT:   List of frames
; INPUTS:   rules - list of rules, with the following format:
;           (
;             (IF r_1-premise_1 r_1-premise_2 ... r_1-premise_n_1 THEN r_1-conclusion)
;             (IF r_2-premise_1 r_2-premise_2 ... r_2-premise_n_1 THEN r_2-conclusion)
;             ...
;           )
;           That is, for each rule, the first element is always a literal "IF",
;           the second-to-last element is always a literal "THEN", signifying 
;           logical implication, the last element is the consequent of the 
;           implication, and the premises are the antecedants of the implication
;           (assumed to be conjoined).
;
;           [!] Although not explicitly an input, FORWARD1 uses the global
;               variable EP-MEM, containing all of our story facts as the facts
;               to use during forward chaining. EP-MEM is in the format of a
;               list of frames

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
             (t (setq ep-mem (append gamma ep-mem))
                (append gamma (forward1 rules)))))))

; -----------------------------------------------------------------------------

; ****** END INFERENCE ENGINE ******
          
; ****** BEGIN QA ENGINE ******

; -----------------------------------------------------------------------------

; PROBLEM 5: BACKWARD1

; FUNCTION: BACKWARD1
; PURPOSE:  This function takes frame as a query. BACKWARD1 looks for the 
;           *first* rule in rules whose THEN-part SF-UNIFYs with query. If 
;           successful, it returns a list of frames, from the IF-part of that 
;           rule, with any variables in the IF-part replaced by their bindings 
;           (via SF-SUBST)
;
;           See also FOL-BC-ASK from the text
;           (figure 9.6 on pg. 338, online at http://aima.cs.berkeley.edu/algorithms.pdf
;           page 24) although there are differences, as stated above and in the
;           HW4 spec
; OUTPUT:   List of frames
; INPUTS:   query - single frame that we treat as our first goal in backward
;                   chaining; attempt to unify it to the consequent of a rule
;           rules - list of rules, with the following format:
;           (
;             (IF r_1-premise_1 r_1-premise_2 ... r_1-premise_n_1 THEN r_1-conclusion)
;             (IF r_2-premise_1 r_2-premise_2 ... r_2-premise_n_1 THEN r_2-conclusion)
;             ...
;           )

(defun BACKWARD1 (query rules)
    (let* ((result (first (last (first rules))))
           (res (sf-unify query result)))
        (cond ((equal res nil)
               (cond ((equal (rest rules) nil) nil)
                     (t (backward1 query (rest rules)))))
               (t (let ((preds (rest (ruls (first rules)))))
                     (loop for v in preds collect (sf-subst v res)))))))

; -----------------------------------------------------------------------------

; PROBLEM 6: C-GEN

; FUNCTION: C-GEN
; PURPOSE:  This function takes a frame and a list of associated English 
;           patterns eng-pats and uses those to generate each frame element
;           into English.
; OUTPUT:   List of words representing the frame's English translation
; INPUTS:   frame    - single frame to translate
;           eng-pats - an association list where each frame predicate has a 
;                      list of slots and interleaved English phrases associated
;                      with it. Each phrase is preceded by PHR. C-GEN 
;                      recursively calls upon itself to generate each slot 
;                      specified and returns a list of English words. If a 
;                      predicate to a frame is not in eng-pats, then C-GEN does
;                      not attempt to generate its slots and returns the
;                      predicate itself.
(defun word (x)
     (cond ((and (listp x) (equal (first x) 'phr)) (rest x))
           (t nil)))
    
(defun sent-gen1 (frame sent &optional (i 1))
  (let ((newsent (repl sent (nth i frame) (nth (+ i 1) frame))))
  (cond ((< (+ i 2) (length frame)) (sent-gen1 frame newsent (+ i 2)))
        (t newsent))))

(defun sent-gen2 (sent eng-pats)
  (loop for v in sent collect
    (cond ((not (equal (word v) nil)) (word v))
          ((framep v) (c-gen v eng-pats))
          (t v))))

(defun sent-gen3 (sent eng-pats &optional (i 0))
  (cond ((>= i (length sent)) sent)
        ((framep (nth i sent)) (sent-gen3 sent eng-pats (+ i 1)))
        (t 
  (let ((j (searc (first (nth i sent)) eng-pats)))
    (cond ((= j -1) (sent-gen3 sent eng-pats (+ i 1)))
          (t (cond ((not (equal (word (first (second (nth j eng-pats)))) nil)) 
                    (sent-gen3 (repl sent (list (first (nth j eng-pats))) (first (second (nth j eng-pats)))) eng-pats (+ i 1)))
                   (t (sent-gen3 (repl sent (list (first (nth j eng-pats))) (second (nth j eng-pats))) eng-pats (+ i 1))))))))))

(defun sent-gen4 (sent)
    (cond ((= (length sent) 1) (first sent))
          (t (append (first sent) (sent-gen4 (rest sent))))))

(defun C-GEN (frame eng-pats)
  (let ((i (searc (first frame) eng-pats)))
    (cond ((= i -1) (first frame))
      (t (let* ((sent1 (nth i eng-pats))
                (sent2 (sent-gen1 frame (second sent1)))
                (sent3 (sent-gen3 sent2 eng-pats))
                (sent4 (sent-gen2 sent3 eng-pats)))
         (sent-gen4 sent4))))))

; -----------------------------------------------------------------------------

; PROBLEM 7: C-GENS

; FUNCTION: C-GENS
; PURPOSE:  This simple function calls on C-GEN to generate a sentence for each
;           frame in frames, separated by the word AND.
; OUTPUT:   List of lists of words generated by C-GEN separated by the literal
;           AND
; INPUTS:   frames   - a list of frames to translate
;           eng-pats - an association list where each frame predicate has a 
;                      list of slots and interleaved English phrases associated
;                      with it. Each phrase is preceded by PHR. C-GEN 
;                      recursively calls upon itself to generate each slot 
;                      specified and returns a list of English words. If a 
;                      predicate to a frame is not in eng-pats, then C-GEN does
;                      not attempt to generate its slots and returns the
;                      predicate itself.

(defun C-GENS (frames eng-pats)
    (let ((sent (c-gen (first frames) eng-pats)))
    (cond ((= (length frames) 1) (list sent))
          (t (append (list sent) (list 'and) (c-gens (rest frames) eng-pats))))))

; -----------------------------------------------------------------------------

; ****** END QA ENGINE ******
