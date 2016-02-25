; CS 161 Spring 2014: HW1 Solution
; hw-1-solution.lsp

; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: front-slot
; PURPOSE:  Return the name of the first SLOT in FRAME
; INPUT:    FRAME
; OUTPUT:   atom: name of first SLOT

(defun front-slot (frame)
    (second frame)
)

; FUNCTION: front-filler
; PURPOSE:  Return the FILLER of the first SLOT in FRAME
; INPUT:    FRAME
; OUTPUT:   FRAME/GAP: filler of first SLOT in FRAME

(defun front-filler (frame)
    (third frame)
)

; FUNCTION: pop-slot
; PURPOSE:  Return a copy of FRAME with its first slot removed
; INPUT:    FRAME
; OUTPUT:   FRAME (with first slot removed)

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)

; Function to retrieve predicate of a FILLER (or just the symbol if it's a symbol)
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

; Function to safely check length of FILLER in case it isn't a list
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

; FUNCTION: rm-slot
; PURPOSE:  Return a copy of FRAME, but with a single slot removed
; INPUT:    frame -- frame to remove slot from
;           slot  -- slot to be removed
; OUTPUT:   frame

(defun rm-slot (slot frame)
    (cond
        ; Base case: no slots left, so we're done
        ((<= (length frame) 1) frame)
        ; Base case: front slot matches, so just pop it
        ((equal (front-slot frame) slot) (pop-slot frame))
        ; Recursive case: front slot doesn't match, so keep looking
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; -----------------------------------------------------------------------------
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: FILLER
; PURPOSE:  Return the FILLER of the given SLOT in the given FRAME
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
; INPUTS:   slot: atom
;           frame: FRAME

(defun FILLER (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler. (first (second ... gets the
        ; slot name, while (second (second ... gets its filler
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (FILLER slot (pop-slot frame)))
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: PATH-SL
; PURPOSE:  Return a FILLER accessed by following a path of named SLOTS within
;           the given CONCEPT
; OUTPUT:   FILLER of the CONCEPT at the given path, or NIL if not present
; INPUTS:   slots: list (path of SLOTs to follow)
;           concept: FRAME

(defun PATH-SL (slots concept)
    (cond
        ; Base case: got to the last slot, so stop recursing
        ((null slots) concept)
        ; Base case: null concept
        ((null concept) nil)
        ; If our concept is a gap to expand, we need to recurse on the frame
        ; Return nil if it's a gap but not bound
        ((atom concept) (if (boundp concept) (PATH-SL slots (eval concept)) nil))
        ; Recursive case: continue following path on sub-frame matched by filler
        ; of current path element
        (t (PATH-SL (rest slots) (FILLER (first slots) concept)))
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: GAPSLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching UNGAP
;           on each filler.
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
; INPUTS:   sf: list of (slot filler) pairs

(defun GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch UNGAP on our first filler
        (t (append (append (list (first sf))            ; rebuild our first slot-filler pair
                           (list (UNGAP (second sf))))  ; dispatch UNGAP on the filler
                           (GAPSLOTS (nthcdr 2 sf))))   ; recurse on rest of sf
    )
)

; FUNCTION: UNGAP
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, except that
;           any GAPs have been replaced by their values
; OUTPUT:   FRAME, with all GAPs replaced
; INPUTS:   frame: FRAME to evaluate GAPs in
; HELPERS:  GAPSLOTS, to go through all of the elements of our frame's slot-fillers

(defun UNGAP (frame)
    (cond
        ; Base case: we got a non-NIL atom, so evaluate it if bound
        ((not (listp frame)) (if (boundp frame) (UNGAP (eval frame)) frame))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Main case: dispatch GAPSLOTS on our slot-filler list
        (t (cons (first frame) (GAPSLOTS (rest frame))))
    )
)

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
        (t (append (ADD-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; Top-level ADD-SF function that simply evaluates frame if it was input as
; a gap (and passes to the workhorse helper ADD-SF-EXEC)

(defun ADD-SF (slot filler frame)
    (if (atom frame) 
        (if (boundp frame) (ADD-SF-EXEC slot filler (eval frame)) frame)
        (ADD-SF-EXEC slot filler frame)
    )
)

; -----------------------------------------------------------------------------

; Function to retrieve predicate of a FILLER (or just the symbol if it's a symbol)
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

; Function to safely check length of FILLER in case it isn't a list
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

; FUNCTION: SAME-SF-COMP
; PURPOSE:  Helper function that performs actual workhorse of frame comparison
;           on UNGAPped frame inputs
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: [UNGAPPED] FRAME (first frame to compare)
;           frame2: [UNGAPPED] FRAME (second frame to compare)

(defun SAME-SF-COMP (frame1 frame2)
    (cond
        ; Base case: empty frames match
        ((and (null frame1) (null frame2)) t)
        ; Base case: frames with different preds do not match
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ; Base case: frames of different lengths do not match
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ; Base case: bare predicates (or matching symbols) match
        ((<= (f-length frame1) 1) t)
        ; Base case: variables match iff they have the same name
        ; [!] Not required for HW1, will not be tested
        ((equal (first frame1) 'V) (equal frame1 frame2))
        
        ; Recursive case: check frame1's front slot, then remove and recurse to
        ; check the rest of the slots.
        (t (let ((front (front-slot frame1))) 
            (and (SAME-SF-COMP (FILLER front frame1) (FILLER front frame2))
                 (SAME-SF-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

; FUNCTION: SAME-SF
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)

(defun SAME-SF (frame1 frame2)
    (let ((UG-frame1 (UNGAP frame1)) (UG-frame2 (UNGAP frame2)))
        (SAME-SF-COMP UG-frame1 UG-frame2)
    )
)

; -----------------------------------------------------------------------------

; Mark as included for examples script
; (this is not necessary for solution correctness, but avoids some warnings)

(setq HW-1-SOLN-INCLUDED t)