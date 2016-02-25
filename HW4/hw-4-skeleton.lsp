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

(defun SF-UNIFY  (farg1 farg2 &optional (beta '(T)))
    'UNIMPLEMENTED
)

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

(defun VAR (x)
	(if (and (equal (length x) 2)
			 (equal (first x) 'V)
			 (symbolp (second x))) T
		nil))

(defun SF-UNIVAR (var x beta)
	(cond 
		((equal beta nil) nil)
		(())
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 2: SF-SUBST

; FUNCTION: SF-SUBST
; PURPOSE:  Replace all variables in the given frame with their bindings given
;           in the binding-list beta.
; OUTPUT:   Instantiated frame, where variables have been replaced
; INPUTS:   frame - frame to replace variables in
;           beta  - binding list, of the form from SF-UNIFY
;                   (T (var1 binding1) (var2 binding2) ...)

(defun SF-SUBST (frame beta)
    'UNIMPLEMENTED
)

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

(defun SF-INFER (rule facts)
    'UNIMPLEMENTED
)

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

(defun FORWARD1 (rules)
    'UNIMPLEMENTED
)

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
    'UNIMPLEMENTED
)

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

(defun C-GEN (frame eng-pats)
    'UNIMPLEMENTED
)

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
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

; ****** END QA ENGINE ******
