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
;p1
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

;p2

(defun ADD-SF-EXEC (slot filler frame)
    (cond
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ((equal slot (front-slot frame)) (cons (first frame)
                                               (append (append (list slot) (list filler) (nthcdr 3 frame)))))
        (t (append (ADD-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))))

(defun INSERT-SL (slot filler conatm)
    (setq USEDMEM (append USEDMEM (list conatm)))
    (let* ((wrking (ungap conatm)))
           (ADD-SF-EXEC slot (ungap filler) wrking)))
;p3
(defun START-SRCH (atmlst myatm dir)
    (cond
        ((equal dir 'BEF) (START-SRCH (reverse atmlst) myatm 'AFT))
        ((equal dir 'IM-BEF) (START-SRCH (reverse atmlst) myatm 'IM-AFT))
        ((null atmlst) nil)
        ((equal (first atmlst) myatm) (if (equal dir 'AFT) (rest atmlst) (list (second atmlst))))
        (t (START-SRCH (rest atmlst) myatm dir))))

(defun SRCH (atmlst myatm dir pred &optional found)
    (cond
        ((not found) (SRCH (START-SRCH atmlst myatm dir) myatm dir pred t))
        ((null atmlst) nil)
        (t (if (ISA (first (eval (first atmlst))) pred ISAMEM) (first atmlst)
               (SRCH (rest atmlst) myatm dir pred t)))))
;p4
(defun TOP-CON (wkm used)
    (let ((wrking (set-difference wkm used)))
       (loop for v in wrking do (if (equal (ungap v) nil) (setf wrking (dele v wrking))))
       wrking))

;p5
(defun analyze-sent (sent lexic)
  (let* ((next-sent (next-ph sent lexic))
         (lexic-word (first next-sent))
         (demonlst (nth 2 lexic-word))
         (word (first (first lexic-word))))
    (if (not (equal (first lexic-word) nil))
    (instan-con (second lexic-word) WKMEM))
    (cond ((equal (first (first demonlst)) 'dm-exp)
          (setq *gensym-counter* (- *gensym-counter* 1))
          (spawn demonlst (newatm 'con))))
    (cond ((equal (rest sent) nil) nil)
          (t (analyze-sent (rest next-sent) lexic)))))

(defun C-ANALYZER (sent lexic)
  (analyze-sent sent lexic)
  (poll-dems DEMEM)
  (let ((memory (top-con WKMEM USEDMEM)))
    (loop for v in memory collect (ungap v))
    (setq USEDMEM WKMEM)))

;p6
(defun DM-MODIF (mycon pred dir slot filler)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
         ((equal found nil) nil)
         (t (INSERT-SL slot filler found)
            (setq USEDMEM (append USEDMEM FOUND))
            '(DIE)))))

;p7
(defun DM-LNAME (mycon myslot)
   (let ((found (SRCH WKMEM mycon 'IM-AFT 'UNKNOWN)))
      (cond 
         ((equal found nil) nil)
         ((equal (second (ungap found)) 'WORD) 
            (let* ((k (searc myslot (ungap mycon)))
                   (n (- (length (ungap mycon)) k))
                   )
            (set (nth (+ n 1) (ungap mycon)) (third (ungap found)))
            (setq USEDMEM (append USEDMEM FOUND))
            '(DIE)))
         (t nil))))

;p8
(defun DM-DISAMB-FEELS (mycon)
   (let ((found (SRCH WKMEM mycon 'IM-AFT 'THAT)))
      (let ((k (searc mycon wkmem)))
         (cond ((equal k 1) nil)))
      (cond 
         ((equal found nil)
            (set mycon (UNIQUE-GAPS '(STATE TYPE TYP AGENT AG)))
            (SPAWN '((DM-EXP HUMAN BEF TYPE) (DM-EXP EMOTION AFT AGENT)) MYCON)
            '(DIE))
         (t 
            (set mycon (UNIQUE-GAPS '(BELIEVE AGENT AG OBJECT OBJ)))
            (SPAWN '((DM-EXP HUMAN BEF AGENT) (DM-EXP ACT AFT OBJECT)) MYCON)
            '(DIE)))))

;p9
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

(defun DM-FNAME (mycon pred dir myslot)
   (let ((fund (fnd mycon pred dir myslot)))
      (cond 
         ((equal fund nil) nil)
         (t (let ((found (filler myslot (ungap fund))))
            (set (filler myslot (ungap mycon)) found)
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE))))))

;p10
(defun fd (mycon pred dir myslot filler)
   (let ((found (SRCH WKMEM mycon dir pred)))
      (cond 
        ((not (equal (filler myslot (ungap found)) filler))
          (if (OR (equal dir 'IM-AFT) 
                  (equal dir 'IM-BEF)
                  (equal found nil)) nil
            (fd found pred dir myslot filler)))
        (t found))))

(defun DM-FINDHER (mycon pred slot filler dir myslot)
   (let ((fund (fd mycon pred dir slot filler)))
      (cond 
         ((equal fund nil) nil)
         (t (set (filler myslot (ungap mycon)) fund)
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE)))))

;p11
(defun DM-MODIF2 (mycon mypred dir myslot)
   (let ((found (SRCH WKMEM mycon dir mypred)))
      (cond 
         ((equal found nil) nil)
         (t (INSERT-SL myslot (filler myslot (ungap mycon)) found)
            '(DIE)))))

;p12
(defun DM-HIM-FNAME (mycon myslot dir pred slot filler)
   (let ((fund (fd mycon pred dir slot filler)))
      (cond 
         ((equal fund nil) nil)
         ((not (equal (filler myslot (ungap fund)) nil))
            (set (filler myslot (ungap mycon)) (filler myslot (ungap fund)))
            (setq USEDMEM (append USEDMEM (list fund)))
            '(DIE)))))

;p13
(defun DM-USED (mycon)
   (setq USEDMEM (append USEDMEM (list mycon))))