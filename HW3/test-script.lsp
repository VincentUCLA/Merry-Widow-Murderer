(defun front-slot (frame)
    (second frame)
)

(defun front-filler (frame)
    (third frame)
)

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)

(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

(defun rm-slot (slot frame)
    (cond
        ((<= (length frame) 1) frame)
        ((equal (front-slot frame) slot) (pop-slot frame))
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

;BY TA
(defun FILLER (slot frame)
    (cond
        ((<= (length frame) 1) nil)
        ((equal slot (front-slot frame)) (front-filler frame))
        (t (FILLER slot (pop-slot frame)))
    )
)

;BY TA
(defun PATH-SL (slots concept)
    (cond
        ((null slots) concept)
        ((null concept) nil)
        ((atom concept) (if (boundp concept) (PATH-SL slots (eval concept)) nil))
        (t (PATH-SL (rest slots) (FILLER (first slots) concept)))
    )
)

;BY TA
(defun GAPSLOTS (sf)
    (cond
        ((null sf) nil)
        (t (append (append (list (first sf))            ; rebuild our first slot-filler pair
                           (list (UNGAP (second sf))))  ; dispatch UNGAP on the filler
                           (GAPSLOTS (nthcdr 2 sf))))   ; recurse on rest of sf
    )
)

;BY TA
(defun UNGAP (frame)
    (cond
        ((not (listp frame)) (if (boundp frame) (UNGAP (eval frame)) frame))
        ((<= (length frame) 1) frame)
        (t (cons (first frame) (GAPSLOTS (rest frame))))
    )
)

;BY TA
(defun ADD-SF-EXEC (slot filler frame)
    (cond
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ((equal slot (front-slot frame)) (cons (first frame)
                                               (append
                                                    (append (list slot) (list filler)
                                                    (nthcdr 3 frame))))
        )
        (t (append (ADD-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

;BY TA
(defun ADD-SF (slot filler frame)
    (if (atom frame) 
        (if (boundp frame) (ADD-SF-EXEC slot filler (eval frame)) frame)
        (ADD-SF-EXEC slot filler frame)
    )
)

;BY TA
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

;BY TA
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

;BY TA
(defun SAME-SF-COMP (frame1 frame2)
    (cond
        ((and (null frame1) (null frame2)) t)
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ((<= (f-length frame1) 1) t)
        ((equal (first frame1) 'V) (equal frame1 frame2))
        (t (let ((front (front-slot frame1))) 
            (and (SAME-SF-COMP (FILLER front frame1) (FILLER front frame2))
                 (SAME-SF-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

;BY TA
(defun SAME-SF (frame1 frame2)
    (let ((UG-frame1 (UNGAP frame1)) (UG-frame2 (UNGAP frame2)))
        (SAME-SF-COMP UG-frame1 UG-frame2)
    )
)


;BY TA
(defun CLEAR-GLOBALS ()
    (setq LEXMEM NIL)
    (setq WKMEM NIL)
    (setq DEMEM NIL)
    (setq USEDMEM NIL)
)


;BY TA
(defun MATCH-PH (words lex)
    (let* ((len (length lex)))
        (cond
            ((< (length words) len) 0)
            ((equal (subseq words 0 len) lex) len)
            (t 0)
        )
    )
)

;BY TA
(defun NEXT-PH (wrdlst lexic)
    (cond
        ((null wrdlst) nil)
        ((null lexic) nil)
        
        ((= (length lexic) 1)
            (let* (
                (lex (first lexic))
                (match-len (MATCH-PH wrdlst (first lex)))
            )
            (if (> match-len 0)
                (cons (list (subseq wrdlst 0 match-len) (second lex) (third lex))
                      (nthcdr match-len wrdlst))
                
                (cons (list
                    (list (first wrdlst))
                    (list 'UNKNOWN 'WORD (list (first wrdlst)))
                    NIL)   
                    (rest wrdlst))
            ))
        )
        
        (t (let* (
                (match1 (MATCH-PH wrdlst (first (first lexic))))
                (match2 (MATCH-PH wrdlst (first (second lexic))))
            )
            (if (>= match1 match2)
                (NEXT-PH wrdlst (cons (first lexic) (nthcdr 2 lexic)))
                (NEXT-PH wrdlst (rest lexic)))
            )
        )
    )
)

;BY TA
(setq *gensym-counter* 1)


(defun NEWATM (symName)
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)

;BY TA
(defun UNIQUE-GAPSLOTS (sf)
    (cond
        ((null sf) nil)
        (t (append (append (list (first sf))                  ; rebuild our first slot-filler pair
                           (list (UNIQUE-GAPS (second sf))))  ; dispatch UNIQUE-GAPS on the filler
                           (UNIQUE-GAPSLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

;BY TA
(defun UNIQUE-GAPS (frame)
    (cond
        ((not (listp frame)) (let* ((gap (NEWATM frame))) (set gap NIL) gap))
        ((<= (length frame) 1) frame)
        (t (cons (first frame) (UNIQUE-GAPSLOTS (rest frame))))
    )
)

;BY TA
(defun START-SRCH (atmlst myatm dir)
    (cond
        ((equal dir 'BEF) (START-SRCH (reverse atmlst) myatm 'AFT))
        ((equal dir 'IM-BEF) (START-SRCH (reverse atmlst) myatm 'IM-AFT))
        ((null atmlst) nil)
        ((equal (first atmlst) myatm) (if (equal dir 'AFT) (rest atmlst) (list (second atmlst))))
        (t (START-SRCH (rest atmlst) myatm dir))
    )
)

;BY TA
(defun BIND (gap found)
    (set gap found)
    (setq USEDMEM (append USEDMEM (list found)))
    found
)

;BY TA
(defun ADD-LEX-EXEC (phrase frame demons lex)
    (cond
        ((null lex) (list (list phrase frame demons)))
        ((equal (first (first lex)) phrase) (cons (list phrase frame demons) (rest lex)))
        (t (append (list (first lex)) (ADD-LEX-EXEC phrase frame demons (rest lex))))
    )
)

;BY TA
(defun ADD-LEX (phrase frame demons)
    (setq LEXMEM (ADD-LEX-EXEC phrase frame demons LEXMEM))
)


;BY TA
(defun INSTAN-CON (frame wkm)
    (let* ((mem (NEWATM 'CON)))
        (set mem (UNIQUE-GAPS frame))
        (setq WKMEM (append WKMEM (list mem)))
        WKMEM
    )
)

;BY TA
(defun SPAWN (partial-dems mycon)
    (cond
        ((null partial-dems) nil)
        (t
            (let* (
                (dem (first partial-dems))
                (newdemon (append (list (first dem)) (list mycon) (rest dem) )))
                (setq DEMEM (append (list newdemon) DEMEM))
                (cons newdemon (SPAWN (rest partial-dems) mycon))
            )
        )
    )
)


;BY TA
(defun POLL-DEMS (&optional no-change)
    (cond
        ((null demem) nil)
        ((not (null no-change)) demem)
        (t (POLL-DEMS
               (progn
                   (let* (
                       (old-dems DEMEM)
                       (collected (loop for dem in demem append
                           (cond ((equal (apply (first dem) (rest dem)) '(die))
                               (print dem)
                               nil) ; If success, don't add back
                               (t (list dem)) ; On failure, do add back
                       )))
                       (new-dems (set-difference demem old-dems)))
                       (setq DEMEM (union new-dems collected))
                       (and (subsetp old-dems DEMEM) (subsetp DEMEM old-dems))
                   )
               ))
        )
    )
)

;BY TA
(defun DM-EXP (mycon pred dir myslot)
    (let* ((frame (eval mycon))
           (gap (PATH-SL (list myslot) frame))
           (found (SRCH WKMEM mycon dir pred)))
        (if found (progn (BIND gap found) '(DIE)) nil)
    )
)

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
    
(defun SEARC (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first sent)) (length sent))
      (t (SEARC phrase (rest sent)))))

(defun DELE (phrase sent)
  (let ((appe (searc phrase sent)))
        (append (subseq sent 0 (- (length sent) appe))
                (subseq sent (- (length sent) appe -1)))))
                
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

;BY ME
(defun INSERT-SL (slot filler conatm)
    (set conatm (ADD-SF-EXEC slot filler (EVAL CONATM)))
    (setq USEDMEM (append USEDMEM (list conatm))))

;BY TA
(defun SRCH (atmlst myatm dir pred &optional found)
    (cond
        ((not found) (SRCH (START-SRCH atmlst myatm dir) myatm dir pred t))
        ((null atmlst) nil)
        (t
             (if (ISA (first (eval (first atmlst))) pred ISAMEM)
                 (first atmlst)
                 (SRCH (rest atmlst) myatm dir pred t)))))

;BY ME
(defun TOP-CON (wkm used)
    (let ((wrking (set-difference wkm used)))
       (loop for v in wrking do (if (equal (ungap v) nil) (setf wrking (dele v wrking))))
       wrking))
       
;BY TA
;(defun poll-comp ()
;    (poll-dems)
;    (cond ((null demem) t)
;          (t (poll-comp))))

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

;BY ME -- TESTED
(defun DM-MODIF (mycon pred dir slot filler)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
         ((equal found nil) nil)
         (t (INSERT-SL slot filler found)
            (setq USEDMEM (append USEDMEM (LIST mycon)))
            '(DIE)))))


;BY ME -- TESTED
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

;BY ME -- TESTED
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

;BY ME
(defun DM-MODIF2 (mycon mypred dir myslot)
   (let ((found (SRCH WKMEM mycon dir mypred)))
      (cond 
         ((equal found nil) nil)
         (t (LET ((FLER (filler myslot (UNGAP mycon))))
            (INSERT-SL myslot FLER found))
            '(DIE)))))

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

;BY ME
(defun DM-USED (mycon)
   (setq USEDMEM (append USEDMEM (list mycon))))

(setq ISAMEM '(
(ISA   HOME   LOCATION)
(ISA   HOME   BUILDING)
(ISA   BUILDING   INANIMATE)
(ISA   KILL   VIOLENT-ACT)
(ISA   VIOLENT-ACT   ACT)
(ISA   READ   VISUAL-ACT)
(ISA   SEE  VISUAL-ACT)
(ISA   SEEK   VISUAL-ACT)
(ISA   VISUAL-ACT   ACT)
(ISA   THROW   ACT)
(ISA   EAT   ACT)
(ISA   CO-HABITATE   ACT)
(ISA   HUMAN   ANIMATE)
(ISA   RING   INANIMATE)
(ISA   PRINTED-MATTER   PHYS-OBJ)
(ISA   ANIMATE   PHYS-OBJ)
(ISA   INANIMATE   PHYS-OBJ)
(ISA   RING   WEAR-OBJ)
(ISA   WEAR-OJB   PHYS-OBJ)
))

(CLEAR-GLOBALS)

(setq *gensym-counter* 1)

    (setq SENT-1 '(TEENAGER CHARLOTTE NEWTON FEELS EXCITED))
    
    
    (setq LEXMEM 
        '(((TEENAGER)
               (MODIF AGE (RANGE FROM (13) TO (19) UNIT (YEAR)))
               ((DM-MODIF HUMAN AFT AGE (RANGE FROM (13) 
                                               TO (19)
                                               UNIT (YEAR)))))
          ((CHARLOTTE)
               (HUMAN F-NAME (CHARLOTTE) L-NAME LNM GENDER (FEMALE))
               ((DM-LNAME L-NAME)))
          ((NEWTON)
               (UNKNOWN WORD (NEWTON)) 
               ())
          ((FEELS)
               ()
               ((DM-DISAMB-FEELS)))
          ((EXCITED)
               (EMOTION SENTIM (POS) SCALE (>NORM))
               ())
         ))
         
(C-ANALYZER SENT-1 LEXMEM)


(setq USEDMEM WKMEM)

    (setq SENT-2 '(MYSTERIOUS UNCLE CHARLES OAKLEY WILL MOVE IN WITH HER MOTHER EMMA))
    (setq LEXMEM (append LEXMEM
        '(((MYSTERIOUS)
               (MODIF HISTORY (UNKNOWN))
               ((DM-MODIF HUMAN AFT HISTORY (UNKNOWN))))
          ((UNCLE CHARLES)
               (HUMAN F-NAME (CHARLES) L-NAME LNM GENDER (MALE) UNCLE-OF UNC)
               ((DM-EXP HUMAN BEF UNCLE-OF)
                (DM-LNAME L-NAME)))
          ((WILL)
               (MODIF TIME (FUTURE))
               ((DM-MODIF ACT AFT TIME (FUTURE))))
          ((MOVE IN WITH)
               (CO-HABITATE AGENT AG OBJECT OBJ)
               ((DM-EXP HUMAN BEF AGENT)
                (DM-EXP HUMAN AFT OBJECT)))
          ((HER MOTHER)
               (HUMAN F-NAME FNM GENDER (FEMALE) MOTHER-OF MOF)
               ((DM-FNAME HUMAN IM-AFT F-NAME)
                (DM-FINDHER HUMAN GENDER (FEMALE) BEF MOTHER-OF)))
          ((EMMA)
               (HUMAN F-NAME (EMMA) GENDER (FEMALE))
               ())
         )))
         
(c-analyzer sent-2 lexmem)


(setq USEDMEM WKMEM)


    (setq SENT-3 '(AT CHARLOTTE-S HOME CHARLES SECRETLY THROWS A NEWSPAPER STORY IN THE TRASH CAN BUT CHARLOTTE SEES HIM))
    
    
    
    
    (setq LEXMEM (append LEXMEM
        '(((AT)
               (MODIF LOC LC)
               ((DM-EXP LOCATION AFT LOC)
                (DM-MODIF2 ACT AFT LOC)))
          ((CHARLOTTE-S)
               ()
               ((DM-MODIF INANIMATE AFT OWNER (HUMAN F-NAME (CHARLOTTE) GENDER (FEMALE)))))
          ((HOME)
               (HOME)
               ())
          ((CHARLES)
               (HUMAN F-NAME (CHARLES) GENDER (MALE) L-NAME LNM)
               ((DM-LNAME L-NAME)))
          ((SECRETLY)
               (MODIF MANNER (SECRETLY))
               ((DM-MODIF ACT AFT MANNER (SECRETLY))))
          ((THROWS)
               (THROW AGENT AG OBJECT OBJ)
               ((DM-EXP HUMAN BEF AGENT)
                (DM-EXP PHYS-OBJ AFT OBJECT)))
          ((A)
               (MODIF REF (INDEF))
               ((DM-MODIF PHYS-OBJ AFT REF (INDEF))))
          ((NEWSPAPER STORY)
               (PRINTED-MATTER TYPE (NEWSPAPER MATERIAL (PAPER)) INFO (STORY))
               ())
          ((IN THE TRASH CAN)
               (MODIF INTO (CONTAINER SHAPE (CYLINDRICAL) FOR (TRASH) REF (DEF)))
               ((DM-MODIF ACT BEF INTO (CONTAINER SHAPE (CYLINDRICAL) FOR (TRASH) REF (DEF)))))
          ((BUT)
               (MODIF EXP-VIOL EXPV)
               ((DM-EXP ACT AFT EXP-VIOL)
                (DM-MODIF2 ACT BEF EXP-VIOL)))
          ((SEES)
               (SEE AGENT AG OBJECT OBJ)
               ((DM-EXP HUMAN BEF AGENT)
                (DM-EXP PHYS-OBJ AFT OBJECT)))
          ((HIM)
               (HUMAN F-NAME FNM L-NAME LNM GENDER (MALE))
               ((DM-HIM-FNAME F-NAME BEF HUMAN GENDER (MALE))))
    )))
    
    
(c-analyzer sent-3 lexmem)