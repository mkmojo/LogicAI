;;Reduced the problem by reducing the var_list
;;Each var is expanded to a list of bindings
;;with the const_list. This resulting list
;;is then denoted as H_ONE
;;This H_ONE is later combined with the rest
;;of the distinc-bindings as the final answer
(defun distinct-bindings (var_list const_list)
  (if (equal var_list nil) nil
    (let
	((H_ONE (make-binding (first var_list) const_list))
	 (H_TWO (distinct-bindings (rest var_list) const_list) ))
      (Put H_ONE H_TWO)
      ))
  )

;;Bind a variable with a constant list
;;This is used to construct H_ONE
;;in distinct-bindings function
;;Input
;;'?x '(Cat Dog Table)
;;Output
;;((?X CAT) (?X DOG) (?X TABLE))
(defun make-binding (var const_list)
  (cond
   ((equal const_list nil) nil)
   (t (cons (list var (first const_list)) 
	    (make-binding var (rest const_list))))))

;;pair is varialbe-constant pair
;;unifier_list is a list of unifiers
;;each item in the list is a unifier
;;This function will "expand" existing
;;unifers. For each unifier privided 
;;by the input unifier_list, it will
;;add current pair into each of the
;;unifier in the unifier_list if this
;;expansion operation does not cause 
;;conflict. If it cuase conflict, this
;;function will just remove the unifer
;;from the unifier_list
(defun insert-to-each (pair unifier_list)
  (cond
   ((equal unifier_list nil) nil)
    ((equal pair nil)  nil)
    (t 
     (let
	 ((H_ONE (cons pair (car unifier_list)))
	  (H_TWO (insert-to-each pair (rest unifier_list))))
       (cond
	 ;;Bad unifier, leave it out	
	 ((has-another-pair pair (car unifier_list) ) H_TWO) 
	 ;;Good unifier, should keep it as part of result
	 (t (cons H_ONE H_TWO))
	)
       ))
    ))

;;p_list is a list of var-constant pair
;;u_list is a list of unifiers
;;Put will take out the first pair in the
;;p_list and "exapnd" the given u_list with that
;;pair. 
(defun Put (p_list u_list)
  (if (equal p_list nil) (return-from Put nil))
  (if (equal u_list nil) (return-from Put (p_list-to-u_list p_list)))
  
  (let ((H_ONE (insert-to-each (first p_list) u_list))
	(H_TWO (Put (rest p_list) u_list)))
    (Combine H_ONE H_TWO))
  )

;;used specificlly for Put
;;because cons is not going
;;to squeeze all unifiers in
;;u_list1 to u_list2
;;This will take out all unifiers
;;in int u_list1 to u_list2
(defun Combine (u_list1 u_list2)
  (cond
    ((equal u_list1 nil) u_list2)
    (t (cons (first u_list1) 
	     (Combine (rest u_list1) u_list2))))
  )

;;This is used secifically for Put when 
;;the u_list is nil.
;;When this happens, the first pair list
;;should be turn into a u_list with each
;;unifier being a unifier with only one
;;var-constant binding.
(defun p_list-to-u_list (p_list)
  (cond 
   ((equal p_list nil) nil)
   (t (cons (list (first p_list)) 
	    (p_list-to-u_list (rest p_list))))
   )
  )

(defun is-binded-to-same-constant (item1 item2)
  (if (equal (second item1) (second item2)) 
      t 
      nil))

(defun has-another-pair (item inlist)
  (if (equal inlist nil) nil 
      (if (is-binded-to-same-constant item (car inlist)) 
	  t 
	  (has-another-pair item (rest inlist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Problem 2

(defun literal-unifiers (literal state const_list)
  (cond
    ((is-pos-const-literal literal) (do-pos-const-pre literal state const_list))
    ((is-neg-const-literal literal) (do-neg-const-pre literal state const_list))
    ((is-pos-var-literal literal) (do-pos-var-pre literal state const_list))
    (t (do-neg-var-pre literal state const_list)))
  )
;;This will check if input literal
;;looks like (P arg1 ... argK)
(defun is-pos-var-literal (literal)
  (cond
    ((is-neg-literal literal) nil)
    ((and (not (is-neg-literal literal)) 
	  (not (is-all-const literal))) t)
    (t nil))
    )
;;This will check if input literal
;;looks like (P C1 ... CK)
(defun is-pos-const-literal (literal)
  (cond
    ((and (not (is-neg-literal literal)) 
	  (is-all-const literal)) t)
    (t nil))
  )
;;This will check if input literal
;;looks like (not (P C1 ... Ck))
(defun is-neg-const-literal (literal)
  (cond
   ((not (is-neg-literal literal )) nil)
   ((and 
     (is-neg-literal literal) 
     (is-all-const (first (rest literal)))))
   (t nil))
  )
;;Check if this predicate is in state
(defun do-pos-const-pre (literal state const_list)
  (let ((predicate literal))
      (has-pre-in-state predicate state)
      )
  )
;;Check if this predicate's negation is in the 
;;state
(defun do-neg-const-pre (literal state const_list)
  (let ((predicate (first (rest literal))))
    (not (has-pre-in-state predicate state))
    )
  )
;;This will first generate all possible distinct bindings
;;by calling (distinct-bindings) then it will provide 
;;the resulting distinct bindings to resolute funciton, 
;;which does all the hard work in remove those unifier
;;that does not show up in the state
(defun do-pos-var-pre (literal state const_list)
  (let* 
      ((predicate literal)
       (var_list (get-var-list predicate))
       (u_list (distinct-bindings var_list const_list)))
    (resolute u_list predicate state)
    )
)
(defun do-neg-var-pre (literal state const_list)
  (let* 
      ((predicate (first (rest literal)))
       (var_list (get-var-list predicate))
       (u_list (distinct-bindings var_list const_list)))
    (neg-resolute u_list predicate state)
    )
)
;;This function does the hard work in checking 
;;every unifier in a unifier list.
;;It takes out the first one and try to find
;;that first one in state
;;if it finds it, this unifier will be included
;;in the final return result
;;otherwise will be ignored and left out of the 
;;final returned list.
(defun resolute (u_list predicate state)
  (let*
      ((this_unifier (first u_list))
       (predicate_inst (apply-unifier this_unifier predicate)))
    (cond
      ((equal u_list nil) nil)
      (t (if (has-pre-in-state predicate_inst state) 
	     (cons this_unifier (resolute (rest u_list) predicate state))
	     (resolute (rest u_list) predicate state))))
    )
  )

;;This function does the complement of the previous
;;function. It will leave out when there is a match
;;in the state but keep the unifier that does not have
;;a match in the state
(defun neg-resolute (u_list predicate state)
  (let*
      ((this_unifier (first u_list))
       (predicate_inst (apply-unifier this_unifier predicate)))
    (cond
      ((equal u_list nil) nil)
      (t (if (not (has-pre-in-state predicate_inst state))
	     (cons this_unifier (neg-resolute (rest u_list) predicate state))
	     (neg-resolute (rest u_list) predicate state))))
    )
  )

;;try to find predicate in a given state
;;if there is a match, then return Ture
;;otherwise return False
(defun has-pre-in-state (predicate state)
  (cond 
    ((equal state nil) nil)
    ((equal predicate (first state)) t)
    (t (has-pre-in-state predicate (rest state)))
    )
  )

;;extract variables from a predicate
;;collect all of them and return the 
;;resulting list of all variables 
;;in a given predicate 
(defun get-var-list (predicate)
  (let ((item (first predicate)))
      (cond
	((equal predicate nil)  nil)
	((not (is-variable item))  (get-var-list (rest predicate)))
	(t (cons item (get-var-list (rest predicate))))
	)
      )
  )

;;check to see if a given literal 
;;is a negation of some other literal
(defun is-neg-literal (literal)
  (equal (first literal) 'not)
  )
;;check to see if a given literal is consisting of all 
;;constants
(defun is-all-const (literal)
  (if (equal literal nil) (return-from is-all-const t))
  (and 
   (not (is-variable (first literal))) 
   (is-all-const (rest literal)))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This chk and apply-unifeir is taken from 
;;Sharanyan Srikanthan 
;;The reason being my version has the side
;;effect that it changes input literal

(defun chk (lst build)
  (let ((iter 0) (retval 1))
    (loop                    
       (when (= iter (length build)) (return nil))
       (if (equal (car (nth iter build)) lst)
	   (return (cadr (nth iter build))))
       (incf iter)
       )
    ))

;;This is taken from
;;Sharanyan Srikanthan 
(defun apply-unifier (list1 list2)
  (let ((iter 0) (build (list (car list2))) (tempvar nil) (list3 (cdr list2)))
    (loop
       (setf tempvar (chk (nth iter list3) list1))
       (if (not (equal tempvar nil))
	   (setf build (append build (list tempvar)))
	   (setf build (append build (list (nth iter list3))))
	   )
       (incf iter)
       (when (= iter (length list3)) (return build))
       ))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem #3
(defstruct op_t
  var-list
  preconds
  effects)
;;This function takes out preconditions and effects from operator
;;combine these with states
;;constants are extracted from this resulting list above.
;;The hard work is done by preconds-instances
(defun operator-instances (op state)
  (let* 
      ((preconds (op_t-preconds op))
       (effects (op_t-effects op))
       (literal (Combine effects (Combine state preconds)))
       (constants (extract-constants literal)))
    (preconds-instances preconds state constants)
    )
  )
;;This function first extract out all variables in preconds and states
;;then generate all possible distinct bindings for all these variables and 
;;constants.  These bindings are candidates for the final answer.
;;
;;The do-preconds-instances does the hard work to get rid of bindings that
;;are not true in given state. In this finding process, ALL_DISTINCT is 
;;carried on kept as the basecase for elimination; var_list is kept for 
;;creating ALL_DISTINCT for its subroutines
(defun preconds-instances (preconds state constants)
  (let* ((var_list (extract-variables preconds))
	 (ALL_DISTINCT (distinct-bindings var_list constants)))
    (do-preconds-instances preconds state constants ALL_DISTINCT var_list)
    )
  )
;;This function calls modified-literal-unifiers recursively to get a 
;;series of lists. Each of these list is a answer that is true for a 
;;specific precond in preconds. 
;;keep-identical-unifiers will check if anyone should be kept in the final
;;reuslt
(defun do-preconds-instances (preconds state constants BASECASE var_list)  
  (cond 
    ((equal preconds nil) BASECASE)
    (t (keep-identical-unifiers 
	(modified-literal-unifiers (first preconds) state constants 
				   var_list) 
	(do-preconds-instances (rest preconds) 
	  state constants BASECASE var_list))))
  )
;;This is a helper function that helps do-preconds-instances check
;;what are identicle in two lists. Identicle ones should be kept and
;;returned. 
(defun keep-identical-unifiers (u_list1 u_list2)
  (if (equal u_list1 t) (return-from keep-identical-unifiers u_list2))
  (let 
      ((H_ONE (first u_list1)))
    (cond
      ((equal H_ONE nil) nil)
      ((equal u_list2 nil) nil)
      ((has-unifier H_ONE u_list2)
       (cons H_ONE 
	     (keep-identical-unifiers 
	      (rest u_list1) u_list2)))
      (t (keep-identical-unifiers 
	  (rest u_list1) u_list2)))
    )
  )
;;check and see if unfier is in given 
;;unifier_list
(defun has-unifier (unifier unifier_list)
  (member unifier unifier_list :test #'equal) 
  )
 
(defun extract-constants (literal-list)
  (setq list-of-all-literal (get-all-literal literal-list))
  ;;remove-duplicates is a internal lisp function
  (remove-variable (remove-duplicates list-of-all-literal)))

(defun is-all-atom (inlist)
  (if (atom inlist) t
      (and (atom (car inlist))
	   (is-all-atom (rest inlist)))))
;;literal here is just lists of varialbe or constanst
;;does not have to be variable constant pair but just
;;any thing that is a list in lisp
(defun get-all-literal (literal-list)
  (if (is-all-atom literal-list)
      (if (atom literal-list) nil (rest literal-list))
      (append (get-all-literal (car literal-list))
	      (get-all-literal (rest literal-list)))))

;;check if a given atom is a variable or not
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


(defun extract-variables (literal-list)
  (setq list-of-all-literal (get-all-literal literal-list))
  ;;remove-duplicates is a internal lisp function
  (remove-constants (remove-duplicates list-of-all-literal)))

(defun remove-constants (inlist)
  (if (atom inlist)
      ;;case atom
      (if (not (is-variable inlist)) nil inlist)
      ;;case list
      (if (not (is-variable (first inlist)))
	  (remove-constants (rest inlist))
	  (cons (first inlist) (remove-constants (rest inlist))))))

;;modified means I will return a "full" unifier even
;;for the (clear ?x) case
;;For example, if there are ?x ?y.
;;literal-unifier will return ((?x a))
;;modified-literal-unifiers will return ((?x a) (?y b)) 
(defun modified-literal-unifiers (literal state const_list var_list)
  (cond
    ((is-pos-const-literal literal) (do-pos-const-pre literal state const_list))
    ((is-neg-const-literal literal) (do-neg-const-pre literal state const_list))
    ((is-pos-var-literal literal) (modified-do-pos-var-pre 
				   literal state const_list var_list))
    (t (modified-do-neg-var-pre 
	literal state const_list var_list)))
  )
;;This modified version keeps the var_list
;;keeping the var_list simplify the work
;;of has-unifier 
;;because this var_list consists of all varialbes in states
;;preconds and predicates
(defun modified-do-pos-var-pre (literal state const_list var_list)
  (let* 
      ((predicate literal)
       (u_list (distinct-bindings var_list const_list)))
    (resolute u_list predicate state)
    )
  )
;;This modified version keeps the var_list
;;keeping the var_list simplify the work
;;of has-unifier 
;;because this var_list consists of all varialbes in states
;;preconds and predicates
(defun modified-do-neg-var-pre (literal state const_list var_list)
  (let* 
      ((predicate (first (rest literal)))
       (u_list (distinct-bindings var_list const_list)))
    (neg-resolute u_list predicate state)
    )
)
