;;Problem 1
(defun make-mypair (item1 item2)
  (list item1 item2))

;;check if the input pair is in the given inlist or not
(defun check-this-pair (pair inlist)
  (cond
    ;;the end of comaprision, pair is not in inlist ---> gives back 1
    ((equal inlist nil) 1)
    ((equal (car pair) (caar inlist)) 
     (cond
       ;;The second element does not equal to an already existing pair in inlist ---> gives back 2
       ((not (equal (cdr pair) (cdr (car inlist)))) 2)
       ;;Pair found in the inlist ---> gives back 3
       (t 3)))
    (t (check-this-pair pair (rest inlist)))))

;;Check if given pair is consisting of two constant or
;;if this is a value-constant pair
;;return 1 if the first element is the same as the second element and both are constant
;;return 2 if the first element does not equal to the second element
;;return 3 if first element of the pair is a varialbe
(defun check-constant (pair)
  (let ((first-name (symbol-name (first pair)))
	(second-name (symbol-name (second pair))))
    (cond
      ;;If the first element of pair starts with '?', 
      ;;then it is a varialbe-constant pair.
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
                  ;;constant-constant pair, ignore
				  ((eq pair-type 3) nil )
                  ;;constant-constant pair, mismatch return nil
				  ((eq pair-type 2) (return-from unifier nil))
                  ;;varialble-constant pair, insert it to result
				  ((eq pair-type 1) (push new-pair list3)))))))
    (if (equal list3 '()) 
	t 
	(reverse list3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 2

;;This function has a side effect that it will change the
;;original literal.
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

(defun has-another-pair (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) 
	  t 
	  (has-another-pair item (rest inlist)))))

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
