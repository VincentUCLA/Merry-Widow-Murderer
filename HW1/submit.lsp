(defun FILLER (slot frame)
  ; search from the 0th slot or the 1th element of the list "frame"
  (let ((n 1))
    (cond
      ; if slot (n-1)/2 == slot, return the filler
      ((equal (nth n frame) slot) 
        (nth (+ n 1) frame))
      ; if slot (n-1)/2+1 == nil, return nil
      ((equal (nth (+ n 2) frame) nil)
        nil)
      ; if slot (n-1)/2+1 not nil, then turn to the next slot
      ((not (equal (nth (+ n 2) frame) nil))
        (FILLER slot (nthcdr 2 frame))))))
     
        
        
(defun PATH-SL (slots concept)
  ; begin with the 0th slot
  (let ((n 1))
    (cond
      ; if slot (n-1)/2 == slot, return the filler for the next step
      ((equal (nth n concept) (first slots)) 
        (cond
          ; if the slot just have 1 element, return the filler
          ((equal (second slots) nil)
            (nth (+ n 1) concept))
          ; else search the next one.
          ((not (equal (second slots) nil))
            (PATH-SL (nthcdr 1 slots) (nth (+ n 1) concept)))))
      ; if slot (n-1)/2+1 == nil, return nil
      ((equal (nth (+ n 2) concept) nil)
        nil)
      ; if slot (n-1)/2+1 not nil, then turn to the next slot
      ((not (equal (nth (+ n 2) concept) nil))
        (PATH-SL slots (nthcdr 2 concept))))))

        
        
(defun nthcar (n origin)
  (cond 
    ((= n 1)
      (first origin))
    ((> n 1)
      (cons (first origin) (nthcar (- n 1) (rest origin))))))
      
      
      
(defun ungap (atom)
  (cond
    ((equal atom nil)
    nil)
    ((symbolp atom)
    (cond
      ((boundp atom)
        (ungap (eval atom)))))
    ((listp atom)
      (cond
        ((symbolp (first atom))
        (cond
          ((boundp (first atom))
            (cons (ungap (first atom)) (ungap (rest atom))))
          ((not (boundp (first atom)))
            (cons (first atom) (ungap (rest atom))))))
        ((atom (first atom))
          (cons (first atom) (ungap (rest atom))))
        ((listp (first atom))
          (cons (ungap (first atom)) (ungap (rest atom))))))))
          
          
          
(defun ADD-SF (slot filler frame)
  ; search from the 0th slot or the 1th element of the list "frame"
  (let ((n 1))
    (cond
      ; if slot (n-1)/2 == slot, update the existing filler
      ((equal (nth n frame) slot)
        (cons (first frame) (cons (second frame) (cons filler (nthcdr (+ n 2) frame)))))
      ; if slot (n-1)/2+1 == nil, return nil
      ((equal (nth (+ n 2) frame) nil)
        (append frame (list slot filler)))
      ; if slot (n-1)/2+1 not nil, then turn to the next slot
      ((not (equal (nth (+ n 2) frame) nil))
        (cons (first frame) (cons (second frame) (ADD-SF slot filler (nthcdr 2 frame))))))))
        
        
        
; to search a word in a frame
(defun SEAR (n frame word)
  (cond
    ;
    ((equal (first frame) word)
      n)
    ((equal (nth 1 frame) nil)
      -1)
    ((NOT (equal (first frame) word))
      (SEAR (+ n 1) (rest frame) word))))

      
      
(defun SAME-SF (frame1 frame2)
  (let ((n (SEAR 0 frame2 (second frame1))))
    (cond
      ; if not found.
      ((equal n -1)
        nil)
      ; otherwise
      ((not (equal n -1))
        (cond
          ; if both of them run out, return true.
          ((AND (equal (nth (+ n 2) frame1) nil)
                (equal (nth (+ n 2) frame2) nil))
            (cond
            ; if both of them have children
            ((AND (listp (nth (+ n 1) frame1))
                  (> (length (nth (+ n 1) frame1)) 2)
                  (listp (nth (+ n 1) frame2))
                  (> (length (nth (+ n 1) frame2)) 2))
              (SAME-SF (nth (+ n 1) frame1) 
                       (nth (+ n 1) frame2)))
            ; if one of them have children but one of them not
            ((OR (AND (NOT (listp (nth (+ n 1) frame1)))
                           (listp (nth (+ n 1) frame2)))
                 (AND (NOT (listp (nth (+ n 1) frame2)))
                           (listp (nth (+ n 1) frame1)))) nil))
            T)
          ; if one of them run out, return nil
          ((OR (AND (NOT (equal (nth (+ n 2) frame1) nil))
                         (equal (nth (+ n 2) frame2) nil))
               (AND (NOT (equal (nth (+ n 2) frame2) nil))
                         (equal (nth (+ n 2) frame1) nil)))
            nil)            
          ; both of them not run out
          ((AND (NOT (equal (nth (+ n 2) frame1) nil))
                (NOT (equal (nth (+ n 2) frame2) nil)))
            (cond
            ; if both of them have children
            ((AND (listp (nth (+ n 1) frame1))
                  (> (length (nth (+ n 1) frame1)) 2)
                  (listp (nth (+ n 1) frame2))
                  (> (length (nth (+ n 1) frame2)) 2))
              (SAME-SF (nth (+ n 1) frame1) 
                       (nth (+ n 1) frame2)))
            ; if one of them have children but one of them not
            ((OR (AND (NOT (listp (nth (+ n 1) frame1)))
                           (listp (nth (+ n 1) frame2)))
                 (AND (NOT (listp (nth (+ n 1) frame2)))
                           (listp (nth (+ n 1) frame1)))) nil))
            ; search for next element in the 1st list
            (SAME-SF (nthcdr 2 frame1) 
                     (append (butlast frame2 (- (length frame2) (- n 1))) (nthcdr (+ n 1) frame2)))))))))