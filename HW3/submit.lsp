; CS 161 Spring 2014: HW3 skeleton

; Suppresses function redefinition warnings (for
; problems 3 - 5)
(setq CUSTOM:*SUPPRESS-CHECK-REDEFINITION* t)

; Any functions you use from hw 1 - 2 should be imported
; in your test script rather than this file (for grading
; sake rather than good practice!)

; File structure:
;
; Section 1: Utility functions:
;   -- PROBLEM 1: ISA
;   -- PROBLEM 2: INSERT-SL
;   -- PROBLEM 3: SRCH
;   -- PROBLEM 4: TOP-CON
;   -- PROBLEM 5: C-ANALYZER
;
; Section 2: Demons:
;   -- PROBLEM 6: DM-MODIF
;   -- PROBLEM 7: DM-LNAME
;   -- PROBLEM 8: DM-DISAMB-FEELS
;   -- PROBLEM 9: DM-FNAME
;   -- PROBLEM 10: DM-FINDHER
;   -- PROBLEM 11: DM-MODIF2
;   -- PROBLEM 12: DM-HIM-FNAME
;   -- PROBLEM 13: DM-USED


; ****** BEGIN UTILITY FUNCTIONS ******

(defun nthcar (n origin)
  (cond 
    ((= n 1)
      (list (first origin)))
    ((> n 1)
      (cons (first origin) (nthcar (- n 1) (rest origin))))))
      
(defun SEAR (phrase sent)
  (cond 
      ((= (length sent) 0) nil)
      ((equal (second (first sent)) phrase) (append (list (first sent)) (sear phrase (rest sent))))
      (t (sear phrase (rest sent)))))
      
(defun gett (phrase)
  (cond
    ((equal (length phrase) 0) nil)
    ((listp (first phrase)) (append (gett (first phrase)) (gett (rest phrase))))
    (t (append (list (first phrase)) (gett (rest phrase))))))
    
(defun front-slot (frame)
    (second frame))

(defun front-filler (frame)
    (third frame))

(defun SEARC (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first sent)) (length sent))
      (t (SEARC phrase (rest sent)))))

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame)))

(defun DELE (phrase sent)
  (let ((appe (searc phrase sent)))
        (append (subseq sent 0 (- (length sent) appe))
                (subseq sent (- (length sent) appe -1)))))

; -----------------------------------------------------------------------------

; PROBLEM 1: ISA

; PURPOSE: This utility function returns T if atom ATM1 recursively ISA ATM2 
;          when using the ISA memory parameter isam; else ISA returns NIL.
; INPUTS:  atm1: atom to compare
;          atm2: atom to compare
;          isam: ISA ontology (in the format of the global ISAMEM)
; OUTPUTS: T if atm1 ISA atm2 using ISA ontology isam

(defun ISA (atm1 atm2 isam)
  (let* ((cur (sear atm1 isam))
         (curr (loop for v in cur collect (third v)))
         (result (cond ((equal atm1 atm2) t)
                       ((equal (length curr) 0) nil)
                       ((not (equal (member atm2 curr) nil)) t)
                       (t (loop for v in curr collect (isa v atm2 isam))))))
  (cond ((equal result t) t)
        ((not (equal (member t (gett result)) nil)) t)
        (t nil))))
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------

; FUNCTION: ADD-SF-EXEC
; PURPOSE:  (Workhorse Helper for ADD-SF)
;           Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; OUTPUT:   Copy of FRAME with the top-level SLOT slot filled with filler
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           frame: FRAME (frame to be modified)

(defun ADD-SF-EXEC (slot filler frame)
    (cond
        ; Base case: single predicate, so add the slot
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ; Base case: If first slot is target, replace the filler, keep rest slots
        ((equal slot (front-slot frame)) (cons (first frame)
                                               (append
                                                    (append (list slot) (list filler)
                                                    (nthcdr 3 frame))))
        )
        ; Recursive case: First slot not target, so pop and recurse
        (t (append (ADD-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))))

; PROBLEM 2: INSERT-SL

; PURPOSE:  Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           conatm: bound concept atom (gap (possibly nested) or frame to be modified)
; OUTPUT:   conatm that was modified by the insertion (the atom representing it)

(defun INSERT-SL (slot filler conatm)
    (set conatm (ADD-SF-EXEC slot filler (EVAL CONATM)))
    (setq USEDMEM (append USEDMEM (list conatm))))

; -----------------------------------------------------------------------------

; START-SRCH is a helper for SRCH (and some demons). It locates a particular
; atom inside atmlst, and returns the remainder of atmlst in the given direction.
(defun START-SRCH (atmlst myatm dir)
    (cond
        ; Why have two cases when we can have one? Just do a forward search in
        ; the reversed list for a BEF search.
        ((equal dir 'BEF) (START-SRCH (reverse atmlst) myatm 'AFT))
        ; We can even condense our "immediately before" case into an 'IM-AFT
        ((equal dir 'IM-BEF) (START-SRCH (reverse atmlst) myatm 'IM-AFT))
        ; Base case: searched everything
        ((null atmlst) nil)
        ; Base case: found what we were looking for, so just return the rest
        ; if we had an AFT dir, or simply the next one in line if we had an
        ; IM-AFT dir
        ((equal (first atmlst) myatm) (if (equal dir 'AFT) (rest atmlst) (list (second atmlst))))
        ; Recursive case: no dice, so keep looking
        (t (START-SRCH (rest atmlst) myatm dir))))
; PROBLEM 3: SRCH

; PURPOSE: SRCH searches through a list of atoms, which is structured as a list
;          of CON atoms that evaluate to frames. It should start its search at 
;          the atom ATMLST that matches MYATM, moving either immediately or 
;          iteratively forward or backward in ATMLST looking for a frame whose
;          top-level predicate ISA pred.
; INPUTS:  atmlst - list of atoms that eval to frames
;          myatm  - atom to start at
;          dir    - direction to search which can be:
;                   AFT -> forward
;                   IM-AFT -> immediately following
;                   BEF -> backward
;                   IM-BEF -> immediately preceding
;          pred   - pred to search for
; OUTPUT:  frame-reference atom if successful, NIL otherwise

(defun SRCH (atmlst myatm dir pred &optional found)
    (cond
        ; As a first step, find our starting CON. Flag that we found it
        ; by setting optional parameter "found"
        ((not found) (SRCH (START-SRCH atmlst myatm dir) myatm dir pred t))
        
        ; Base case: searched everything
        ((null atmlst) nil)
        
        ; We already found the start atom, so start looking at predicates
        (t
             ; See if pred matches (have to expand the first atm in atmlst
             ; into its frame representation first)
             (if (ISA (first (eval (first atmlst))) pred ISAMEM)
                 ; A match, so return it
                 (first atmlst)
                 ; Else, no match; remove and try again
                 ; (add optional argument "found" since we did find con)
                 (SRCH (rest atmlst) myatm dir pred t)
             ))))

; -----------------------------------------------------------------------------

; PROBLEM 4: TOP-CON

; PURPOSE: TOP-CON goes through the atoms in WKM and returns a list of all 
;          atoms that do NOT appear in USED AND that are not null.
; INPUTS:  wkm (list)  - working memory atoms
;          used (list) - used atoms
; OUTPUT:  List of atoms in wkm that do not appear in USED AND are not null
    
(defun TOP-CON (wkm used)
    (let ((wrking (set-difference wkm used)))
       (loop for v in wrking do (if (equal (ungap v) nil) (setf wrking (dele v wrking))))
       wrking))
       
; -----------------------------------------------------------------------------

; PROBLEM 5: C-ANALYZER

; PURPOSE: C-ANALYZER is the top-most level function, that will return a frame
;          representation of a sentence represented as a sequence of words. 
;          It loops through attempting to match the words of SENT to words 
;          or phrases in the lexicon, LEXIC, using NEXT-PH. For each 
;          recognized phrase, it INSTAN-CONs the associated frame (also 
;          adding the CON atom of that frame to working memory within 
;          INSTAN-CON), SPAWNs the associated demon list, and invokes 
;          POLL-DEMS on the global list DEMEM. This process repeats until 
;          SENT is empty. C-ANALYZE then returns the UNGAP of the 
;          TOP-CON of working memory as it stands after loading in the 
;          complete sentence.
;          Additionally, as a side effect, it sets USEDMEM = WKMEM.
; INPUTS:  sent (list)  - list of words (as atoms) representing a sentence
;          lexic (list) - a conceptual lexicon (see problem 1)
; OUTPUT:  List of UNGAPPED TOP-CON of WKMEM and USEDMEM
; SIDE-EFFECT: USEDMEM = WKMEM

(defun C-ANALYZER (sent lexic)
    (let* ((pfdr (NEXT-PH sent lexic))
           (triplet (first pfdr))
           (frame (second triplet))
           (demons (third triplet))
           (nextsent (rest pfdr)))
    (cond ((null sent)
             (setq DEMEM (reverse DEMEM))
             (poll-dems)
             (let ((resu (loop for fr in (TOP-CON WKMEM USEDMEM) collect (UNGAP fr))))
             (setq USEDMEM WKMEM)
             resu))
          (t (progn
             (SPAWN demons (first (last (INSTAN-CON frame WKMEM))))
             (C-ANALYZER nextsent lexic)))
    ))
)

; -----------------------------------------------------------------------------

; ****** END UTILITY FUNCTIONS ******



; ****** BEGIN DEMON SECTION ******

; -----------------------------------------------------------------------------

; PROBLEM 6: DM-MODIF

; PURPOSE: Starting at mycon, this demon uses the SRCH function (as modified 
;          above) to look in direction dir for a CON atom with predicate that 
;          ISA pred and if found, uses the utility function INSERT-SL to 
;          insert pair: 
;          [ slot  filler ] into found 's frame and to add found to USEDMEM.
; INPUTS:  mycon (atom) - concept atom this demon works for
;          pred (atom)  - predicate to search for
;          dir (atom)   - direction in which to search
;          slot (atom)  - slot to add to found's frame
;          filler       - FILLER to add at slot in found's frame
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-MODIF (mycon pred dir slot filler)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
         ((equal found nil) nil)
         (t (INSERT-SL slot filler found)
            (setq USEDMEM (append USEDMEM (LIST mycon)))
            '(DIE)))))

; -----------------------------------------------------------------------------

; PROBLEM 7: DM-LNAME

; PURPOSE: This demon uses the SRCH function to look immediately after mycon 
;          for the predicate UNKNOWN with slot-name WORD and if its filler is 
;          found, it sets the gap of myslot to have that filler value. It also 
;          adds the CON atom of UNKNOWN to USEDMEM.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          myslot (atom) - slot name in mycon frame to set to found slot WORD
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds the con atom of UNKNOWN to USEDMEM

(defun DM-LNAME (mycon myslot)
   (let* ((found (SRCH WKMEM mycon 'IM-AFT 'UNKNOWN)))
      (cond 
         ((equal found nil) nil)
         ((equal (second (ungap found)) 'WORD) 
            (let* ((k (filler myslot (eval mycon))))
               (set k (third (ungap found)))
               (setq USEDMEM (append USEDMEM (LIST FOUND)))
               '(DIE)))
         (t nil))))
; -----------------------------------------------------------------------------

; PROBLEM 8: DM-DISAMB-FEELS

; PURPOSE: This demon attempts to disambiguate the input word. This demon uses 
;          SRCH to look immediately after MYCON for the predicate THAT and if 
;          found uses UNIQUE-GAPS (from HW2) to instantiate a frame:
;          (BELIEVE AGENT AG OBJECT OBJ) as the value of mycon and then uses 
;          the function SPAWN (from HW2) to spawn two DM-EXP demons 
;          (one for AGENT, the other for OBJECT) else it selects: 
;          (STATE TYPE TYP AGENT AG) as the frame for the word FEELS and 
;          spawns two DM-EXP demons (one for TYPE and one for AGENT).
; INPUTS:  mycon (atom) - concept atom this demon works for
; OUTPUTS: (DIE) if search successful, nil otherwise

;BY ME
(defun DM-DISAMB-FEELS (mycon)
   (let ((found (SRCH WKMEM mycon 'IM-AFT 'THAT)))
      (let ((k (searc mycon wkmem)))
         (cond ((equal k 1) nil)))
      (cond 
         ((equal found nil)
            (let ((ycon mycon))
            (set ycon (UNIQUE-GAPS '(STATE TYPE TYP AGENT AG))))
            (SPAWN '((DM-EXP HUMAN BEF AGENT) (DM-EXP EMOTION AFT TYPE)) MYCON)
            '(DIE))
         (t 
            (setq USEDMEM (append USEDMEM (LIST FOUND)))
            (let ((ycon mycon))
            (set ycon (UNIQUE-GAPS '(BELIEVE AGENT AG OBJECT OBJ))))
            (SPAWN '((DM-EXP HUMAN BEF AGENT) (DM-EXP ACT AFT OBJECT)) MYCON)
            '(DIE)))))
; -----------------------------------------------------------------------------

; PROBLEM 9: DM-FNAME

; PURPOSE: Looks for pred in direction dir with a slot-name myslot and gets 
;          its filler (this is found). If found, it binds the gap of myslot 
;          (within the frame represented by mycon) with found and adds the CON 
;          atom of the frame it found to USEDMEM.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          pred (atom)   - predicate to search for
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot name containing gap within mycon frame to bind
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds found CON atom to USEDMEM

;BY ME
(defun fnd (mycon pred dir myslot)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
        ((OR (equal (filler myslot (ungap found)) nil)
             (NOT (LISTP (filler myslot (ungap found)))))
          (if (OR (equal dir 'IM-AFT)
                  (equal dir 'IM-BEF)
                  (equal found nil)) nil
          (fnd found pred dir myslot)))
        (t found))))

;BY ME
(defun DM-FNAME (mycon pred dir myslot)
   (let ((fund (fnd mycon pred dir myslot)))
      (cond 
         ((equal fund nil) nil)
         (t (let ((found (filler myslot (ungap fund))))
            (set (filler myslot (eval mycon)) found)
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE))))))

; -----------------------------------------------------------------------------

; PROBLEM 10: DM-FINDHER

; PURPOSE: Looks for a CON atom in direction dir whose frame has top-level 
;          predicate pred with slot slot and filler filler. If found, 
;          DM-FINDHER binds found to the gap of myslot. 
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          pred (atom)   - predicate to search for
;          slot (atom)   - slot within query frame
;          filler        - FILLER to match within slot of query frame
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot name containing gap within mycon frame to bind
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds found CON atom to USEDMEM

;BY ME -- TESTED
(defun fd (mycon pred dir myslot filler)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
        ((not (equal (filler myslot (ungap found)) filler))
          (if (OR (equal dir 'IM-AFT) 
                  (equal dir 'IM-BEF)
                  (equal found nil)) nil
            (fd found pred dir myslot filler)))
        (t found))))

;BY ME -- TESTED
(defun DM-FINDHER (mycon pred slot filler dir myslot)
   (let ((fund (fd mycon pred dir slot filler)))
      (cond 
         ((equal fund nil) nil)
         (t (let ((fler (filler myslot (eval mycon))))
            (set fler fund)
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE))))))
; -----------------------------------------------------------------------------

; PROBLEM 11: DM-MODIF2

; PURPOSE: Starting at mycon, this demon looks in direction dir for a CON atom 
;          with a pred that isa mypred and if found, uses INSERT-SL to insert 
;          myslot with the filler being the gap of myslot into the top level 
;          of the frame associated with found.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          mypred (atom) - predicate to search for
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot to add/change in query frame to insert gap to
; OUTPUTS: (DIE) if search successful, nil otherwise

;BY ME
(defun DM-MODIF2 (mycon mypred dir myslot)
   (let ((found (SRCH WKMEM mycon dir mypred)))
      (cond 
         ((equal found nil) nil)
         (t (LET ((FLER (filler myslot (UNGAP mycon))))
            (INSERT-SL myslot FLER found))
            '(DIE)))))

; -----------------------------------------------------------------------------

; PROBLEM 12: DM-HIM-FNAME

; PURPOSE: This demon looks for a CON atom in direction dir that has a 
;          top-level pred with slot slot having filler value filler. If found, 
;          DM-HIM-FNAME looks to see if found has a slot myslot. If so, it sets 
;          the value of myslot in the found frame to be the value of myslot in 
;          the frame of that CON atom.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          myslot (atom) - slot belonging to mycon frame
;          dir (atom)    - direction in which to search
;          pred (atom)   - predicate to search for
;          slot (atom)   - slot belonging to query frame
;          filler        - FILLER belonging to slot of query frame
; OUTPUTS: (DIE) if search successful, nil otherwise

;BY ME
(defun DM-HIM-FNAME (mycon myslot dir pred slot filler)
   (let ((fund (fd mycon pred dir slot filler)))
      (cond 
         ((equal fund nil) nil)
         ((not (equal (filler myslot (ungap fund)) nil))
            (let ((fler (filler myslot (eval mycon))))
            (set fler (filler myslot (ungap fund)))
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE))))))

; -----------------------------------------------------------------------------

; PROBLEM 13: DM-USED

; PURPOSE: This demon simply adds MYCON to USEDMEM.
; INPUTS:  mycon (atom) - concept atom this demon works for
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-USED (mycon)
   (setq USEDMEM (append USEDMEM (list mycon))))

; -----------------------------------------------------------------------------

; ****** END DEMON SECTION ******
