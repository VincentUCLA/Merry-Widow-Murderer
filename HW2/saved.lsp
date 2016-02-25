(defun nthcar (n origin)
  (cond 
    ((= n 1)
      (list (first origin)))
    ((> n 1)
      (cons (first origin) (nthcar (- n 1) (rest origin))))))

(defun CHECK-EXISTANCE (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first (first sent))) (length sent))
      (t (CHECK-EXISTANCE phrase (rest sent)))))
      

(defun SEAR (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first sent)) (length sent))
      (t (SEAR phrase (rest sent)))))

(defun SEARC (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first (eval (first sent)))) (length sent))
      (t (SEARC phrase (rest sent)))))

(defun SEACR (phrase sent)
  (cond 
      ((= (length sent) 0) 0)
      ((equal phrase (first (eval (first (last sent))))) (length sent))
      (t (SEACR phrase (nthcar (- (length sent) 1) sent)))))

      
(defun CHECK (phrase sent)
  (cond ((= (length sent) 0) 0)
        ((= (length phrase) 0) 0)
        ((not (= (check-existance phrase sent) 0))
         (check-existance phrase sent))
        (t (check (nthcar (- (length phrase) 1) phrase) sent))))
        
(defun DEL (phrase sent)
  (let ((appe (CHECK-EXISTANCE phrase sent)))
        (append (subseq sent 0 (- (length sent) appe))
                (subseq sent (- (length sent) appe -1)))))
                
(defun repl (phrase1 phrase2 sent)
  (let ((sublist (member phrase1 sent)))
    (append (subseq sent 0 (- (length sent) (length sublist))) 
            (list phrase2)
            (rest sublist))))

(defun front-filler (frame)
    (third frame))

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame)))

(defun ins (word frame)
  (append (list (first frame)) (list word) (rest frame)))    

(defun helper (wkm used)
  (if (subsetp (list (first wkm)) used) (rest wkm)
      wkm))
;p2
(defun NEXT-PH (WRDLST LEXIC)
  (let* ((appe (check WRDLST LEXIC))
         (except (- (length LEXIC) appe))
         (dict (first (subseq LEXIC except))))
    (cond ((= appe 0) 
            (append (list (list (list (first WRDLST)) (list 'UNKNOWN 'WORD (list (first WRDLST))) nil)) (rest WRDLST)))
          (t
            (append (list dict) (rest WRDLST))))))
;p3
(setq *gensym-counter* 1)
(defun NEWATM (symName)
    ; Replace 'UNIMPLEMENTED with your symbol generation
    (let* ( (new-sym (gensym (string symName))) )
        ; This line included to make your life easy without diving into
        ; what it means! Leave it alone 
        (intern (string new-sym))))
;p4
(defun UNIQUE-GAPS (frame)
    (cond ((<= (length frame) 1) frame)
          (t 
            (cond 
                  ((listp (front-filler frame)) 
                    (append (nthcar 3 (repl (front-filler frame) (unique-gaps (front-filler frame)) frame))
                    (rest (unique-gaps (pop-slot frame)))))
                  ((atom (front-filler frame)) 
                    (append (nthcar 3 (repl (front-filler frame) (newatm (front-filler frame)) frame))
                    (rest (unique-gaps (pop-slot frame)))))))))

;p7
(defun SRCH (atmlst myatm dir pred)
  (let ((n (- (length atmlst) (sear myatm atmlst))))
    (cond 
      ((equal dir 'IM-AFT) 
       (if (equal (first (eval (nth (+ n 1) atmlst))) pred)
           (nth (+ n 1) atmlst)
           nil))
      ((equal dir 'IM-BEF)
       (if (equal (first (eval (nth (- n 1) atmlst))) pred)
           (nth (- n 1) atmlst)
           nil))
      ((equal dir 'AFT)
        (let* ((sublist (subseq atmlst n))
               (m (- (length sublist) (searc pred sublist))))
          (if (equal (searc pred sublist) 0) nil
              (nth m sublist))))
      ((equal dir 'BEF)
        (let* ((sublist (subseq atmlst 0 n))
               (m (seacr pred sublist)))
          (if (equal m 0) nil
              (nth (- m 1) sublist)))))))

;p8
(defun BIND (gap found)
  (set gap found)
  (setq USEDMEM (append USEDMEM (list FOUND)))
  found)

;p1
(defun ADD-LEX (phrase frame demons)
  (let ((sent (list phrase frame demons))
        (appe (check-existance phrase LEXMEM)))
    (cond ((not (= appe 0))
           (cond ((equal (del phrase LEXMEM) '(nil)) 
                 (setq LEXMEM (list (list sent))))
                 (t 
                 (setq LEXMEM (append (list sent) (del phrase LEXMEM))))))
          ((= appe 0)
           (setq LEXMEM (append LEXMEM (list sent)))))))

;p5
(defun INSTAN-CON (frame wkm)
  (let ((newCON (newatm 'CON)))
    (set newCON frame)
    (setq WKMEM (append wkm (list newCON)))))

;p6
(defun SPAWN (partial-dems mycon)
  (cond ((<= (length partial-dems) 1) 
         (setq demem (append (list (ins mycon (first partial-dems))) DEMEM))
         (list (ins mycon (first partial-dems))))
        (t 
         (setq demem (append (list (ins mycon (first partial-dems))) DEMEM))
         (append (list (ins mycon (first partial-dems))) (spawn (rest partial-dems) mycon)))))

;p10
(defun POLL-DEMS (demlist &optional N)
  (if (not N) (setf n 0))
  (cond ((= n (length demlist))
          (setq DEMEM demlist)
          demlist)
         (t (let ((result (apply 'dm-exp (rest (nth n demlist)))))
            (cond ((equal result '(DIE)) 
                   (setq demlist (dele (nth n demlist) demlist))
                   (poll-dems demlist N))
                  ((equal result nil)
                   (poll-dems demlist (+ N 1))))))))

;p11
(defun TOP-CON (wkm used)
  (cond ((= (length wkm) 1) (helper wkm used))
        (t 
          (append (helper (list (first wkm)) used)
                  (top-con (rest wkm) used)))))

;p12
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
    (loop for v in memory collect (ungap v))))

;p9
(defun DM-EXP (mycon pred dir myslot)
  (let ((found (SRCH WKMEM (first (list MYCON)) (first (list DIR)) (first (list PRED)))))
    (if (equal found nil) nil
      (cond ((subsetp (list myslot) (eval mycon)) 
             (let ((n (- (length (eval mycon)) (sear myslot (eval mycon)))))
               (bind (nth (+ n 1) (eval mycon)) found)) '(die))
             (t nil)))))