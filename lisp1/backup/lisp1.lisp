;;Problem 1
(defun make-mypair (item1 item2)
  (list item1 item2))

(defun check-this-pair (pair inlist)
  (cond
    ;;if the inlist is empty, the end of comaprision ---> gives back 1
    ((equal inlist nil) 1)
    ;;if the first element of each pair equals
    ((equal (car pair) (caar inlist)) 
     (cond
       ;;The Second element does not equal ---> gives back 2
       ((not (equal (cdr pair) (cdr (car inlist)))) 2)
       ;;Both are equal ---> gives back 3
       (t 3)))
    (t (check-this-pair pair (rest inlist)))))

;;return 1 if first element is the same the second
;;return 2 if first does not equal second
;;return 3 if first is varialbe
;;Check if given pair is consisting of two constant or
;;if this is a value constant pair
(defun check-constant (pair)
  (let ((first-name (symbol-name (first pair)))
	(second-name (symbol-name (second pair))))
    (cond
      ;;If the first element of pair starts with '?', 
      ;;then it is a varialbe constant pair.
      ((CHAR= (char first-name 0) #\?) 3)
      ((not (equal first-name second-name)) 2)
      ((STRING= first-name second-name) 1))))

(defun unifier (list1 list2)
  (let ((list3 nil))
    (if (eq (list-length list1) (list-length list2)) 
	(continue)
	(return-from unifier nil))
    (loop while list1 do
	 (let* ((new-pair (make-mypair (pop list1) (pop list2)))
		(isconstant (check-constant new-pair))
		(pair-type (check-this-pair new-pair list3)))
	   (cond
	     ;;if both are constants and equal --> yield t and continue loop
	     ((eq isconstant 1) t)
	     ;;if both are constanst but not equal --> return nil
	     ((eq isconstant 2) (return-from unifier nil))
	     ;;if the first one is a varialbe and the second one is a constant
	     ;;check to see if should insert
	     ((eq isconstant 3) (cond
				  ((eq pair-type 3) nil )
				  ((eq pair-type 2) (return-from unifier nil))
				  ((eq pair-type 1) (push new-pair list3)))))))
    (if (equal list3 '()) 
	t 
	(reverse list3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 2
(defun apply-unifier (unifier literal)
  (cond 
    ((and (atom (first unifier))
	   (atom (second unifier))) 
     ;;substitute constant for variable in the given literal
     (nsubst (second unifier) (first unifier) literal)) 

    (t (apply-unifier (car unifier) literal) 
       (apply-unifier (rest unifier) literal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 3
;;Part a
(defun is-all-atom (inlist)
  (if (atom inlist) t 
      (and (atom (car inlist)) 
	   (is-all-atom (rest inlist)))))

(defun get-all-literal (literal-list)
  (if (is-all-atom literal-list)
      (if (atom literal-list) nil (rest literal-list))
      (append (get-all-literal (car literal-list)) 
	      (get-all-literal (rest literal-list)))))

(defun is-variable (item)
  (let ( (item-name (symbol-name item)) )
    (if (CHAR= (char item-name 0) #\?) t nil)))

(defun remove-variable (inlist)
  (if (atom inlist)
      ;;case atom
      (if (is-variable inlist) nil inlist) 
      ;;case list
      (if (is-variable (first inlist)) 
	  (remove-variable (rest inlist))
	  (cons (first inlist) (remove-variable (rest inlist))))))

(defun extract-constants (literal-list)
  (setq list-of-all-literal (get-all-literal literal-list))
  (remove-variable (remove-duplicates list-of-all-literal)))


;;Part b
(defun is-binded-to-same-constant (item1 item2)
  (if (equal (second item1) (second item2)) 
      t 
      nil))

(defun is-conflict (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) 
	  t 
	  (is-conflict item (rest inlist)))))

(defun rm-item (item inlist)
  (remove item inlist :test #'equal))

(defun prune-unifiers (unifier-list)
  (let ((result-list nil))
    (loop for x in unifier-list do
	 (if (is-conflict x result-list)
	     (setq result-list (rm-item x result-list))
	     (push x result-list)))
    (reverse result-list)))



(defun has-another-unifier (list1 list2)
  (not (set-exclusive-or list1 list2 :test #'equal))
)

(defun rm-item (item inlist)
  (remove item inlist :test #'equal))

(defun prune-unifiers-bak (unifier-list)
  (let ((result-list nil))
    (loop for x in unifier-list do
	 (if (is-conflict x result-list)
	     (setq result-list (rm-item x result-list))
	     (push x result-list)))
    (reverse result-list)))


(defun rm-unifier (unifier unifier-list)
  (remove unifier unifier-list :test #'has-another-unifier)
)

(defun prune-unifiers-bak-bak (unifier-list)
  (let ((result nil))
    (loop for cnt_unifier in unifier-list do
	 (if (has-another-unifier cnt_unifier result)
	     (setq result (rm-unifier cnt_unifier result))
	     (if result (setq result (list cnt_unifier result)) (continue))))
    (reverse result)))
