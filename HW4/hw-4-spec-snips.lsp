; CS 161 Spring 2014: HW4 Spec Snips
; All of your copy-pasta needs from the spec, now in
; .lsp form!

; -----------------------------------------------------------------------------
; PROBLEM 1 EXAMPLES
; -----------------------------------------------------------------------------

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


; -----------------------------------------------------------------------------
; PROBLEM 3-4 EXAMPLES
; -----------------------------------------------------------------------------

(setq RULE-1
    '(IF 
         
         (SEE AGENT (V AGG1)
              OBJECT (THROW AGENT (V AGG2)
                            OBJECT (V OBJJ1)
                            INTO (V INTOO1)
                            MANNER (SECRETLY))
              LOC (V LOCC1))
         
         (MYSTERIOUS AGENT (V AGG2))
         
         (STORY OBJECT (V OBJJ1) 
                ABOUT (CRIME-1))
         
         (IDENTITY AGENT (UNKNOWN)
                   OF (CRIME-1))
         
     THEN
         
         (BELIEVES AGENT (V AGG1)
                   OBJECT (COMMITTED AGENT (V AGG2)
                                     OBJECT (CRIME-1)))
     )
)

(setq RULE-2
    '(IF 
         
         (BELIEVES AGENT (V AGG4)
                   OBJECT (COMMITTED AGENT (V AGG5)
                                     OBJECT (CRIME-1)))
         
         (RELATIVE-OF AGENT (V AGG5)
                      OBJECT (V AGG4))
         
     THEN
         
         (FEELS AGENT (V AGG4)
                STATE (CONFLICT))
     )
)

(setq RULE-3
    '(IF 
         
         (UNCLE-OF AGENT (V AGG6)
                   OBJECT (V AGG7))
         
     THEN
         
         (RELATIVE-OF AGENT (V AGG6)
                      OBJECT (V AGG7)))
)

(setq CSK-RULES (list RULE-1 RULE-2 RULE-3))

(setq FACTS1 '(
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
))

; -----------------------------------------------------------------------------
; PROBLEM 5 EXAMPLES
; -----------------------------------------------------------------------------

(setq BFRAME
    '(BELIEVES AGENT (CHARLOTTE-1)
               OBJECT (COMMITTED AGENT (CHARLES-1)
                                 OBJECT (CRIME-1)))
)

; -----------------------------------------------------------------------------
; PROBLEM 6 EXAMPLES
; -----------------------------------------------------------------------------

(setq ENG-PATS1
    '((SEE (AGENT (PHR SAW) OBJECT AT (PHR AT) LOC))
      (MYSTERIOUS (AGENT (PHR HAS AN UNKNOWN PAST)))
      (CRIME-1 ((PHR THE MURDER OF SAMANTHA)))
      (THROW (AGENT MANNER (PHR THROW) OBJECT (PHR INTO) INTO))
      (STORY ((PHR THE STORY TOLD IN) OBJECT (PHR IS ABOUT) ABOUT))
      (CHARLES-1 ((PHR UNCLE CHARLES)))
      (TRASH ((PHR A TRASH CAN)))
      (PRINTED-1 ((PHR A NEWSPAPER CLIPPING)))
      (CHARLOTTE-1 ((PHR CHARLOTTE)))
      (HOME-1 ((PHR HOME)))
      (IDENTITY ((PHR THE IDENTITY OF PERSON INVOLVED IN) OF (PHR IS) AGENT))
      (RELATIVE-OF (AGENT (PHR IS A RELATIVE OF) OBJECT))
      (BELIEVES (AGENT (PHR BELIEVES THAT) OBJECT))
      (COMMITTED (AGENT (PHR COMMITTED) OBJECT)))
)

; -----------------------------------------------------------------------------
; PROBLEM 7 EXAMPLES
; -----------------------------------------------------------------------------

(setq C-ANS-1
    '((SEE AGENT (CHARLOTTE-1)
           OBJECT (THROW AGENT (CHARLES-1)
                         OBJECT (PRINTED-1)
                         INTO (TRASH)
                         MANNER (SECRETLY))
           LOC (HOME-1))
      
      (MYSTERIOUS AGENT (CHARLES-1))
      
      (STORY OBJECT (PRINTED-1) 
             ABOUT (CRIME-1))
      
      (IDENTITY AGENT (UNKNOWN)
                OF (CRIME-1)))
)

(setq C-ANS-2
    '((BELIEVES AGENT (CHARLOTTE-1)
                OBJECT (COMMITTED AGENT (CHARLES-1)
                                  OBJECT (CRIME-1)))
      
      (RELATIVE-OF AGENT (CHARLOTTE-1)
                   OBJECT (CHARLES-1)))
)
