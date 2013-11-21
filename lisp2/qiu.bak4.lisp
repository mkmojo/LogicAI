;;This is the right answer
;;the problem it has is that
;;the prune-unifiers from last
;;assignment is really slow
;;it is a performance killer
;;
(defun distinct-bindings (var_list const_list)
  (if (equal var_list nil) nil
      (let
	  ((H_ONE (make-binding (first var_list) const_list))
	   (H_TWO (distinct-bindings (rest var_list) const_list) ))
	(prune-unifiers (Put H_ONE H_TWO))
	)
      )
  )

(defun make-binding (var const_list)
  (cond
   ((equal const_list nil) nil)
   (t (cons (list var (first const_list)) 
	    (make-binding var (rest const_list)))))
  )


;;pair is varialbe-constant pair
;;unifier_list is a list of unifiers
;;each item in the list is a unifier
;; If the unifier_list is nil
;; This function will return nil
;; the reason being 
(defun insert-to-each (pair unifier_list)
  (cond
    ((equal unifier_list nil) nil)
    ((equal pair nil)  nil)
    (t 
     (let
	 ((H_ONE (cons pair (car unifier_list)))
	  (H_TWO (insert-to-each pair (rest unifier_list))))
       (cons H_ONE H_TWO))
     ))
  )

(defun Combine (u_list1 u_list2)
  (cond
    ((equal u_list1 nil) u_list2)
    (t (cons (first u_list1) 
	     (Combine (rest u_list1) u_list2))))
  )


(defun p_list-to-u_list (p_list)
  (cond 
   ((equal p_list nil) nil)
   (t (cons (list (first p_list)) 
	    (p_list-to-u_list (rest p_list))))
   )
  )


(defun Put (p_list u_list)
  (if (equal p_list nil) (return-from Put nil))
  (if (equal u_list nil) (return-from Put (p_list-to-u_list p_list)))
  (let ((H_ONE (insert-to-each (first p_list) u_list))
	(H_TWO (Put (rest p_list) u_list)))
    (Combine H_ONE H_TWO))
  )


(defun is-conflict (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) 
	  t 
	  (is-conflict item (rest inlist)))))


(defun is-binded-to-same-constant (item1 item2)
  (if (equal (second item1) (second item2)) 
      t 
      nil))

(defun has-another-pair (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) 
	  t 
	  (is-conflict item (rest inlist)))))

(defun prune-one-unifier (pair-list)
  (if (equal pair-list nil) nil
   
      (if (has-another-pair (first pair-list) (prune-one-unifier (rest pair-list))) 
	  nil
	  (cons (first pair-list) (prune-one-unifier (rest pair-list)))
	  )))

(defun prune-unifiers (unifier-list)
  (if (equal unifier-list nil) nil
      (let ((result (prune-unifiers (rest unifier-list)))
	    (cnt_unifier (first unifier-list)))
	(if (prune-one-unifier cnt_unifier) 
	    (cons cnt_unifier result)
	    (prune-unifiers (rest unifier-list))))
      )
)
