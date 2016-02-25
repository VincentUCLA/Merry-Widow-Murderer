; CS 161 Spring 2014: HW2 solution
; hw-2-solution.lsp

; Use some utility functions from HW1 solution set
;(load "../hw1/hw-1-solution.lsp")

; File structure:
; Some convenience functions have been provided for you. They will be
; labeled GIVEN, to indicate you don't need to modify them as part of the
; assignment.
;
; Section 1: Utility functions:
;   -- CLEAR-GLOBALS (GIVEN): clears the global variables for the project
;   -- RELOAD (GIVEN): clears global variables and reloads your source file
;
;   -- NEXT-PH (PROBLEM 2): splits input phrase into first-rest using lexicon
;   -- NEWATM (PROBLEM 3): generates a unique atom from the input one
;   -- UNIQUE-GAPS (PROBLEM 4): replaces GAPs with unique atoms
;   -- SRCH (PROBLEM 7): searches a list of atoms for a predicate in a direction
;   -- BIND (PROBLEM 8): binds a GAP to a frame and updates global USEDMEM list
;
; Section 2: Main functions:
;   -- ADD-LEX (PROBLEM 1): adds information to the conceptual lexicon
;   -- INSTAN-CON (PROBLEM 5): adds an instantiated frame to working memory
;   -- SPAWN (PROBLEM 6): instantiates a list of demons and adds to ACTV-DEMONS
;   -- POLL-DEMS (PROBLEM 10): repeatedly calls demons in DEMEM until
;                              either they all succeed or an entire round
;                              passes with no demon succeeding
;   -- TOP-CON (PROBLEM 11): Looks through input CON list and returns those that
;                            have yet to be used
;   -- C-ANALYZER (PROBLEM 12): top-level function to analyze a sentence
;
; Section 3: Demons:
;   -- DM-EXP (PROBLEM 9)


; ****** BEGIN SECTION 1: UTILITY FUNCTIONS ******

; Resets global variables, THEN reloads your code. This means you can initialize the
; globals in your source file for testing purposes.
(defun RELOAD ()
    (clear-globals)
    (load "hw-2-solution.lsp") ; Replace with the name of this file
    ; Feel free to load additional files for testing etc here
)

; Resets the global variables used in the assignment.
(defun CLEAR-GLOBALS ()
    (setq LEXMEM NIL)
    (setq WKMEM NIL)
    (setq DEMEM NIL)
    (setq USEDMEM NIL)
)

; ****** END GIVEN UTILITY FUNCTIONS ******


; ****** BEGIN PROBLEM SKELETONS ******

; -----------------------------------------------------------------------------

; PROBLEM 2: NEXT-PH

; MATCH-PH: helper for NEXT-PH
; Returns length of lexical index (word/phrase) if a match with the start of word,
; 0 otherwise.

(defun MATCH-PH (words lex)
    (let* ((len (length lex)))
        (cond
            ; Not enough words left, so no dice
            ((< (length words) len) 0)
            ; First len entries of words indeed match, so return len
            ((equal (subseq words 0 len) lex) len)
            ; Otherwise, return 0
            (t 0)
        )
    )
)

; NEXT-PH takes a list of words WRDLST and a lexicon LEXIC (the same structure
; as our global variable LEXMEM) and returns a list with the structure 
; (phrase rest)
; ...where phrase is a list of the words that appear at the very
; front of WRDLST and that matches the LONGEST phrase found in LEXIC.
; ...and where rest is the rest of WRDLST with phrase removed
; INPUTS: WRDLST - a phrase to dissect into (phrase rest)
;         LEXIC  - a lexicon with formatting identical to the global LEXMEM
; OUTPUT: List - (phrase rest) as described above

(defun NEXT-PH (wrdlst lexic)
    (cond
        ; Base cases: return nil if either params null
        ((null wrdlst) nil)
        ((null lexic) nil)
        
        ; Got to end of lexic, so return the best match, or special unknown
        ; word frame
        ((= (length lexic) 1)
            (let* (
                (lex (first lexic))
                (match-len (MATCH-PH wrdlst (first lex)))
            )
            (if (> match-len 0)
                ; If match, build list of phrase, frame, demons as the first
                ; element, with the rest of the sentence tacked on after
                (cons (list (subseq wrdlst 0 match-len) (second lex) (third lex))
                      (nthcdr match-len wrdlst))
                
                ; Ff no match, special return value
                (cons (list
                    ; Unknown word goes as its own list in front
                    (list (first wrdlst))
                    ; Build the UNKNOWN frame
                    (list 'UNKNOWN 'WORD (list (first wrdlst)))
                    ; No demons
                    NIL)   
                    ; Remove first word
                    (rest wrdlst))
            ))
        )
        
        ; Recursive case: find best match between first two entries. Keep
        ; first entry if neither match.
        (t (let* (
                (match1 (MATCH-PH wrdlst (first (first lexic))))
                (match2 (MATCH-PH wrdlst (first (second lexic))))
            )
            ; Remove the worst match of the first 2 entries
            (if (>= match1 match2)
                (NEXT-PH wrdlst (cons (first lexic) (nthcdr 2 lexic)))
                (NEXT-PH wrdlst (rest lexic)))
            )
        )
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 3: NEWATM

; NEWATM takes the name of a symbol symName and generates a fresh, 
; unbound symbol with a unique name based upon symName. The numbering will
; begin with 1 and be incremented, using gensym with each newatm created

; param symName - a symbol name to prefix the name of the symbol returned
; returns       - a fresh, unbound symbol with a unique name consisting of:
;                 symName#, where symName was the input symbol turned string
;                           with a unique integer appended where # is
;
; Examples:
; > (NEWATM 'AGENT)
; AGENT1
;
; > (boundp (NEWATM 'AGENT))
; NIL
;
; Now, with another call, (NEWATM 'AGENT) should return:
;  AGENT2

; Begin by setting the gensym counter to start at 1; this will start all
; numbering using gensym with 1, as intended
(setq *gensym-counter* 1)


(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 4: UNIQUE-GAPS

; FUNCTION: UNIQUE-GAPSLOTS (helper for UNIQUE-GAPS)
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching
;           UNIQUE-GAPS on each filler.
; OUTPUT:   List of slot-filler pairs with any gaps replaced by NEWATMs
; INPUTS:   sf: list of (slot filler) pairs

(defun UNIQUE-GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch TOKENIZE on our first filler
        (t (append (append (list (first sf))                  ; rebuild our first slot-filler pair
                           (list (UNIQUE-GAPS (second sf))))  ; dispatch UNIQUE-GAPS on the filler
                           (UNIQUE-GAPSLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; UNIQUE-GAPS should recursively go through a frame, replacing each gap it finds
; with a NEWATM version of that gap. If called on a gap, it should replace
; the gap with a NEWATM version.
; INPUTS: frame (concept or gap)
; OUTPUT: frame instance (all gaps unique), or unique gap

(defun UNIQUE-GAPS (frame)
    (cond
        ; Base case: we got an atom, so uniquify it, and return it
        ((not (listp frame)) (let* ((gap (NEWATM frame))) gap))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Main case: dispatch UNIQUE-GAPSLOTS on our slot-filler list
        (t (cons (first frame) (UNIQUE-GAPSLOTS (rest frame))))
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 7: SRCH

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
        (t (START-SRCH (rest atmlst) myatm dir))
    )
)

; SRCH searches through a list of atoms, which is structured as a list of CON
; atoms that evaluate to frames. It should start its search at the atom ATMLST
; that matches MYATM, moving either immediately or iteratively forward or backward
; in ATMLST looking for a frame whose top-level predicate matches pred.
; INPUTS: atmlst - list of atoms that eval to frames
;         myatm  - atom to start at
;         dir    - direction to search which can be:
;                  AFT -> forward
;                  IM-AFT -> immediately following
;                  BEF -> backward
;                  IM-BEF -> immediately preceding
;         pred   - pred to search for
;         found  - bool flag of whether we've found myatm yet
; OUTPUT: frame-reference atom if successful, NIL otherwise

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
             (if (equal (first (eval (first atmlst))) pred)
                 ; A match, so return it
                 (first atmlst)
                 ; Else, no match; remove and try again
                 ; (add optional argument "found" since we did find con)
                 (SRCH (rest atmlst) myatm dir pred t)
             )
        )
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 8: BIND

; BIND takes a gap and a con-atom found, and uses SET to set gap = found. It 
; should also update the global variable USEDMEM, by adding the found atom to 
; its end.
; (USEDMEM is merely a list of CON-atoms on which BIND has been called)
; INPUT: gap (atom)   - gap to be bound
;        found (atom) - concept atom to be bound
; OUTPUT: found
; SIDE-EFFECT: binds gap to found, and adds found to the global variable USEDMEM

(defun BIND (gap found)
    ; Atom that gap represents is now bound to found
    (set gap found)
    ; Remember to update the USEDMEM global
    (setq USEDMEM (append USEDMEM (list found)))
    found
)

; -----------------------------------------------------------------------------

; ****** END SECTION 1 ******


; ****** BEGIN SECTION 2: MAIN FUNCTIONS ******

; PROBLEM 1: ADD-LEX

; ADD-LEX adds an entry to the global variable LEXMEM, which is a representation
; of a conceptual lexicon. A conceptual lexicon is a list of lists, where each
; internal list has three parts: a list containing one or more words (phrase),
; a frame, and a list of zero or more demons.
; INPUT: phrase (list) - list of one or more words (as atoms), which 
;                        sentences will be matched against
;        frame (frame) - frame to store in lexicon
;        demons (list) - list of demons (function calls stored as data)
; OUTPUT: (whatever you want, irrelevant to functionality)
; SIDE-EFFECT: Updates global LEXMEM by appending the lexicon entry defined  
;              by the input arguments.

; Helper function for ADD-LEX that checks to make sure a phrase isn't already
; within the lexicon, and if so, replaces it with the input definition
(defun ADD-LEX-EXEC (phrase frame demons lex)
    (cond
        ; Base case: lex is null so just insert
        ((null lex) (list (list phrase frame demons)))
        ; Base case: we found a phrase match at the current front
        ; of lex, so replace with our new definition
        ((equal (first (first lex)) phrase) (cons (list phrase frame demons) (rest lex)))
        ; Recursive case: haven't found our matching phrase in lex yet, so keep
        ; looking at the front and recursing
        (t (append (list (first lex)) (ADD-LEX-EXEC phrase frame demons (rest lex))))
    )
)

; Main function to be called for ADD-LEX that uses ADD-LEX-EXEC for the
; workhorse
(defun ADD-LEX (phrase frame demons)
    (setq LEXMEM (ADD-LEX-EXEC phrase frame demons LEXMEM))
)

; -----------------------------------------------------------------------------

; PROBLEM 5: INSTAN-CON

; INSTAN-CON instantiates an input frame by making any gaps in it unique (using
; UNIQUE-GAPS), creating a new unique CON atom with the instantiated frame as
; its value, and then adding that new CON atom to the end of working memory
; input WKM. As a side effect, WKMEM = WKM at the end of the function.
; INPUT: frame - frame to uniquely gap and then bind the new CON atom to
;        wkm   - working memory input (list of CON atoms)
; OUTPUT: wkm with new CON atom added
; SIDE-EFFECT: Adds generated CON atom to the end of the global list WKMEM

(defun INSTAN-CON (frame wkm)
    (let* ((mem (NEWATM 'CON)))
        ; Bind to frame instance
        (set mem (UNIQUE-GAPS frame))
        ; Add to working memory
        (setq WKMEM (append WKMEM (list mem)))
        ; Return frame instance
        WKMEM
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 6: SPAWN

; SPAWN takes in a list of partial demon instances PARTIAL-DEMS and completes
; each partial demon instance by adding the atom MYCON as a first argument to
; each instance. Spawn returns a completed version of PARTIAL-DEMS, and as a
; side effect, adds these completed demon-instances to the front of the global
; variable DEMEM.
; INPUT: partial-dems (list) - list of demons (partial function calls) from 
;                              lexicon
;        mycon (atom)        - atom to add as first argument to each demon
;                              partial function call in partial-dems
; OUTPUT: list of completed demon instances
; SIDE-EFFECT: adds each demon instance created as above to the global list
;              DEMEM.

(defun SPAWN (partial-dems mycon)
    (cond
        ((null partial-dems) nil)
        (t
            (let* (
                (dem (first partial-dems))
                ; construct new demon as ((first dem) mycon (rest dem))
                (newdemon (append (list (first dem)) (list mycon) (rest dem) )))
                ; Update global DEMEM
                (setq DEMEM (append (list newdemon) DEMEM))
                ; Recurse on any remaining demons
                (cons newdemon (SPAWN (rest partial-dems) mycon))
            )
        )
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 10: POLL-DEMS

; POLL-DEMS-EXEC goes through a list of demon instances in DEMLST and polls them 
; (i.e. executes each one). When a demon is successful, it will return (DIE) 
; to POLL-DEMS-EXEC, which will cause POLL-DEMS to remove that demon instance from 
; DEMLST. If any demon returns (DIE) then POLL-DEMS-EXEC will re-poll the remaining 
; demons. It does this until no demon returns (DIE). That is, POLL-DEMS-EXEC polls 
; demons repeatedly until they are all quiescent (they all return NIL). 
; POLL-DEMS returns demlst.
; INPUT: demlst (list): list of demon instances
;        wkm (list): list of CON-atoms (which evaluate to frames)
; OUTPUT: list of demons still remaining after quiescence is reached
; SIDE-EFFECT: POLL-DEMS sets DEMEM equal to the returned demlst

; Helper workhorse function to POLL-DEMS
(defun POLL-DEMS-EXEC (demlst &optional last-len)
    (cond
        ; Base case: nothing to do, or all demons successful
        ((null demlst) nil)
        ; Length didn't change since last call => quiescence reached
        ((equal (length demlst) last-len) demlst)
        ; gather all unsuccessful demons
        (t (POLL-DEMS-EXEC
            (loop for dem in demlst append
                ; call demon
                (if (apply (first dem) (rest dem))
                    nil ; If success, don't add back
                    (list dem) ; On failure, do add back
                )
            )
            (length demlst) 
            ; recursive call will check whether we
            ; removed anything
            )
        )
    )
)

; Main POLL-DEMS interface that is called by users; will
(defun POLL-DEMS (demlst)
    (setq DEMEM (POLL-DEMS-EXEC demlst))
)

; -----------------------------------------------------------------------------

; PROBLEM 11: TOP-CON

; TOP-CON goes through the atoms in WKM and returns a list of all atoms that do
; NOT appear in USED.
; INPUT: wkm (list)  - working memory atoms
;        used (list) - used atoms
; OUTPUT: List of atoms in wkm that do not appear in USED

(defun TOP-CON (wkm used)
    (set-difference wkm used)
)

; -----------------------------------------------------------------------------

; PROBLEM 12: C-ANALYZER

; C-ANALYZER is the top-most level function, that will return a frame representation
; of a sentence represented as a sequence of words. It loops through attempting
; to match the words of SENT to words or phrases in the lexicon, LEXIC, using
; NEXT-PH. For each recognized phrase, it INSTAN-CONs the associated frame (also
; adding the CON atom of that frame to working memory within INSTAN-CON), SPAWNs 
; the associated demon list, and invokes POLL-DEMS on the global list DEMEM.
; This process repeats until SENT is empty.
; C-ANALYZE then returns the UNGAP of the TOP-CON of working memory as it stands
; after loading in the complete sentence.
; INPUT: sent (list)  - list of words (as atoms) representing a sentence
;        lexic (list) - a conceptual lexicon (see problem 1)

(defun C-ANALYZER (sent lexic)
    ; pfdr = (((phrase) frame (demons*)) rest)
    (let* ((pfdr (NEXT-PH sent lexic))
           ; triplet = ((phrase) frame (demons*))
           (triplet (first pfdr))
           (frame (second triplet))
           (demons (third triplet))
           (nextsent (rest pfdr)))
    
    ; When we've exhausted the sentence, return the UNGAP of TOP-CON
    (if (null sent)
        (loop for fr in (TOP-CON WKMEM USEDMEM) collect (UNGAP fr))
        ; Else, process frame/demons, then recurse
        (progn
            ; Need to spawn demons for the con atom we just added, which
            ; will be the last element of the WKMEM, which we can get using
            ; last, which returns a list, so grab the first element
            (SPAWN demons (first (last (INSTAN-CON frame WKMEM))))
            ; Poll the active demons, which will update the DEMEM after
            ; reaching quiescence
            (POLL-DEMS DEMEM)
            ; Finally, recurse: next part of sentence after removing
            ; matched phrase
            (C-ANALYZER nextsent lexic)
        )
    ))
)

; -----------------------------------------------------------------------------

; ****** END SECTION 2 ******


; ****** BEGIN SECTION 3: DEMONS ******

; Demons are short functions to BIND gaps in different frames of working memory
; to each other. Any given invocation of a demon may be successful (it found
; the things it was looking for and BIND'd them), or if it failed to find what
; it was looking for, it does nothing, waiting for later invocations. (It might
; succeed later when more of the sentence has been loaded into working memory.)
;
; Every demon gets as its first argument the CON it is working for (a particular
; element of working memory), which anchors searches for other frames, and will
; typically have gaps that get bound by the demon. Other arguments vary depending
; on the demon's function.
;
; The return value of a demon indicates whether it successfully
; did its operation: (DIE) value on success, or NIL on failure.

; -----------------------------------------------------------------------------

; PROBLEM 9: DM-EXP

; DM-EXP looks for a CON atom in the global variable WKMEM with top predicate 
; PRED. If it finds this PRED, then let's call that CON atom found. If it finds
; found then this DM-EXP instance binds found to the gap associated with MYSLOT
; in MYCON (using the BIND function you developed in problem #8). It returns 
; (DIE) and (as a side-effect of having used the BIND function from problem #8) 
; now found has been added to the global variable USEDMEM. When looking for 
; found, DM-EXP starts searching at MYCON and looks in the global variable WKMEM
; in direction DIR. If DM-EXP is unsuccessful it returns NIL.
; INPUT: mycon (atom)  - demon's frame that it works for in working memory
;        pred (atom)   - top-level predicate to find in WKMEM
;        dir (atom)    - direction of search to perform
;        myslot (atom) - slot in mycon to bind
; OUTPUT: (DIE) if successfully executed and bound, nil otherwise

(defun DM-EXP (mycon pred dir myslot)
    ; Unpack the concept represented by mycon atom, and then perform the
    ; designated SRCH within our WKMEM global
    (let* ((frame (eval mycon))
           (gap (PATH-SL (list myslot) frame))
           (found (SRCH WKMEM mycon dir pred)))
        ; If we find a binding from our SRCH result, then BIND gap to found,
        ; and return '(DIE) for having successfully completed; otherwise nil
        (if found (progn (BIND gap found) '(DIE)) nil)
    )
)

; ****** END SECTION 3 ******
