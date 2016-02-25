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
    (if (and (listp (first x)) (not (equal (first x) nil))) t nil))

(defun front-slot (frame)
    (second frame))
    
(defun front-filler (frame)
    (third frame))

(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame)))

(defun rm-slot (slot frame)
    (cond
        ((<= (length frame) 1) frame)
        ((equal (front-slot frame) slot) (pop-slot frame))
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
                      (cond 
                      (t (sf-unify (rest farg1) (dele farg2 ith) (sf-unify (first farg1) ith beta)))))))))
        (t nil)))
        
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
                           
(defun ruls (rule)
  (if (equal (second rule) 'then) (list (first rule))
    (append (list (first rule)) (ruls (rest rule)))))

(defun SF-INFER (rule facts)
  (let* ((rules (ruls (rest rule)))
         (beta (sf-unify rules facts)))
    (cond ((equal beta nil) nil)
          (t (sf-subst (first (last rule)) beta)))))

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
                
(defun BACKWARD1 (query rules)
    (let* ((result (first (last (first rules))))
           (res (sf-unify query result)))
        (cond ((equal res nil)
               (cond ((equal (rest rules) nil) nil)
                     (t (backward1 query (rest rules)))))
               (t (let ((preds (rest (ruls (first rules)))))
                     (loop for v in preds collect (sf-subst v res)))))))

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
         
(defun C-GENS (frames eng-pats)
    (let ((sent (c-gen (first frames) eng-pats)))
    (cond ((= (length frames) 1) (list sent))
          (t (append (list sent) (list 'and) (c-gens (rest frames) eng-pats))))))