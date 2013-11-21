(defun is-all-atom (inlist)
  (if (atom inlist) t 
      (and (atom (car inlist)) (is-all-atom (rest inlist))))
)
 
(defun get-all-literal (literal-list)
  (if (is-all-atom literal-list) (if (atom literal-list) nil (rest literal-list))
      (append (get-all-literal (car literal-list)) (get-all-literal (rest literal-list))))
)

(defun is-variable (item)
  (let ( (item-name (symbol-name item)) )
    (if (CHAR= (char item-name 0) #\?) t nil))
)

(defun remove-variable (inlist)
  (if (atom inlist)
      ;;case atom
      (if (is-variable inlist) nil inlist) 
      ;;case list
      (if (is-variable (first inlist)) 
	  (remove-variable (rest inlist))
	  (cons (first inlist) (remove-variable (rest inlist)))))
)

;;Problem a
(defun extract-constants (literal-list)
  (setq list-of-all-literal (get-all-literal literal-list))
  (remove-variable (remove-duplicates list-of-all-literal))
)

;; test cases for problem a
;; (extract-constants '((On Table A) (On Table B) (On C B)) )
;; (extract-constants '((Clear ?x) (Clear ?y) (not (Sleeping Robbie))  (Empty-handed Robbie) (At Robbie Table)))

;;Problem b
(defun is-binded-to-same-constant (item1 item2)
  (if (equal (second item1) (second item2)) t nil) 
)

(defun is-conflict (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) t (is-conflict item (rest inlist))))
)

(defun rm-item (item inlist)
 (remove item inlist :test #'equal)
)

(defun prune-unifiers (unifier-list)
  (let ((result-list nil))
    (loop for x in unifier-list do
	 (if (is-conflict x result-list) (setq result-list (rm-item x result-list)) (push x result-list))
    )
    (reverse result-list)
    )
)
;;  (prune-unifiers '( (?x a) (?y c) (?t b)) )
;;  (prune-unifiers '(((?x a) (?y c)) ((?x a) (?y c))))