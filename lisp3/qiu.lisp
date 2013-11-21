;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Solutions
;; CSC 244/444, Fall 2013

;; Notes:
;;  - While these should work adequately, they are not as thoroughly documented
;;    as your code should be.
;;  - Several functions are derived from solutions by Goker Erdogan and
;;    Brian Gernhardt (but any bugs are probably mine).


;;
;; Some General Helpers
;;

;; Our operator structure. This will also give us fns make-operator,
;; operator-p.


;; Is e a variable? I.e., does it start with a question mark?
(defun varp (e)
  (unless (listp e)
    (char-equal (char (string e) 0) #\?)))

;; Is e a constant?
(defun constp (e)
  (and (symbolp e)
       (not (eql e 'not))
       (not (varp e))))

;; Is it a variable match, e.g., from a unifier? A variable match is a list
;; containing a variable and a symbol.
(defun var-matchp (l)
  (and (= (length l) 2)
       (varp (first l))
       (constp (second l))))

;; A unifier is a list of variable-matches.
(defun unifierp (l)
  (and (listp l)
       (every #'var-matchp l)))

;; Is the argument a positive literal?
(defun positive-literalp (arg)
  (and (every #'symbolp arg)
       (<= 1 (length arg))
       (not (varp (first arg)))))

;; Is the argument a positive or negative literal?
(defun literalp (arg)
  (if (and (listp arg)
           (eq (first arg) 'not))
      (and (= (length arg) 2)
           (literalp (second arg)))
    (positive-literalp arg)))


;;
;; Functions for Lisp 1
;;

;; 1 Write a function 'unifier' applicable to two simple argument lists,
;;   where the first contains symbolic variables and/or symbolic constants,
;;   and the second contains symbolic constants only. The output should
;;   be a list of pairs of elements that have been unified, where the
;;   first element of each pair is a variable and the second is a constant.
;;   If the unification fails, the result should be nil; if it succeeds
;;   trivially, i.e., the given argument lists are identical, the result
;;   should be T.
;;
;;   We'll assume that variables are distinguished by starting with
;;   a question mark, e.g., ?x, ?y, ?x1, etc.

(defun unifier (vars consts)
  (unless (or (atom vars) (atom consts)
              (not (eql (length vars) (length consts)))
              (some #'varp consts))
        (or (equal vars consts)
            (unify vars consts))))

(defun unify (vars consts)
  (let (pairs c pairedc)
    (dolist (v vars)
      (setf c (pop consts))
      (cond ((varp v)
             (setf pairedc (second (assoc v pairs)))
             (cond ((null pairedc)
                    (push (list v c) pairs))
                   ((not (equal c pairedc))
                    (return-from unify nil))))
            ((listp v)
             (setf pairs (append pairs (unify v c))))
            ((not (equal c v))
             (return-from unify nil))))
    pairs))


;; 2 Write a function 'apply-unifier' that takes (i) a unifier and
;;   (ii) a positive literal (a predicate with arguments that are
;;   variables or constants, e.g., (P ?x b ?y)) as its two inputs,
;;   and returns the literal with the substitutions made in it that
;;   are specified by the unifier (and are relevant to the literal).
;;
;;   Feel free to use the Common Lisp 'subst' function for this. (In
;;   general, in using this function pay careful attention to the :test
;;   optional parameter, so you'll apply the correct equality test. But
;;   here it shouldn't be an issue.)

(defun apply-unifier (u l)
  (dolist (vcpair u)
    (setf l (subst (cadr vcpair) (car vcpair) l)))
  l)


;; 3 A "state" in a planning domain is often specified as a list of
;;   positive ground literals, e.g.,
;;            ((On Table A) (On Table B) (On C B)).
;;   The assumption is made that if a literal is not found in a state,
;;   it is false in that state. For example, the following are false
;;   in the above state: (On Table C), (On A A), (On B C), etc.
;;
;;   Also, the preconditions of the available actions (i.e., what must
;;   be true for an action to be used) in a planning domain are typically
;;   lists of positive and negative literals; e.g., perhaps preconds for
;;   stacking up a couple of blocks (instantiating ?x and ?y) might be
;;           ((Clear ?x) (Clear ?y) (not (Sleeping Robbie))
;;            (Empty-handed Robbie) (At Robbie Table)).
;;   Effects of actions are similarly specified using positive and negative
;;   literals.

;;   a. Write a function 'extract-constants' that extracts a non-redundant
;;      list of constants from a list of literals, where these may be of
;;      the type we find in states, or the type we find in preconds.

(defun extract-constants (l)
  (unless (atom l)
    (remove-duplicates
     (remove-if #'varp (mapcan #'extract-from-literal l)))))

;; Extract constants and variables from literal l.
(defun extract-from-literal (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (mapcan #'extract-from-literal (cdr l)))))

;;   b. A rather common assumption about action specifications in planning
;;      is that distinct variables appearing in preconds are not allowed
;;      to receive identical bindings. For example, in the above preconds
;;      for stacking two blocks, we don't allow ?x and ?y to be bound to
;;      the same constant; thus we'll never consider stacking a block
;;      onto itself (which would be a challenging trick ;-).
;;
;;      Accordingly, write a function 'prune-unifiers' that takes a list
;;      of unifiers (of the type computed in Q.1) and eliminates
;;      unifiers that bind two variables to the same constant.

(defun prune-unifiers (lu)
  (unless (atom lu)
    (remove-if #'is-many-to-one-unifier lu)))

;; Does the unifier u map two distinct variables to the same constant?
(defun is-many-to-one-unifier (u)
  (let ((bindings nil))
    (dolist (pair u)
      (let ((pairedvar (second (assoc (second pair) bindings))))
        (if (null pairedvar)
            (push (reverse pair) bindings)
            (if (not (equal (car pair) pairedvar))
                (return-from is-many-to-one-unifier t)))))))


;;
;; Functions for Lisp 2
;;

;; Extract a non-redundant list of variables (a la extract-constants).
(defun extract-variables (l)
  (unless (atom l)
    (remove-duplicates
     (remove-if #'constp (mapcan #'extract-from-literal l)))))


;; 1 Write a function 'distinct-bindings' whose inputs are (i) a (small)
;;   list of variables and (ii) a (possibly fairly long) list of constants,
;;   and which returns all ways of pairing the variables with constants,
;;   in such a way that no two variables in a binding are bound to the
;;   *same* constant. Recall from Lisp1 that we'll interpret distinct
;;   variables in preconditions as requiring distinct bindings. For
;;   example, (distinct-bindings '(?x ?y) '(A B Table)) should give
;;     (((?x A) (?y B)) ((?x A) (?y Table))
;;      ((?x B) (?y A)) ((?x B) (?y Table))
;;      ((?x Table) (?y A)) ((?x Table) (?y B)))

(defun distinct-bindings (vars consts)
  (unless (or (atom vars) (atom consts)
              (not (every #'varp vars))
              (not (every #'constp consts)))
    (setf consts (remove-duplicates consts))
    (prune-unifiers (mapcar #'(lambda (x)
                                (mapcar #'list vars x))
                            (permutations consts (length vars))))))

;; Return all s-length permutations of the list l.
(defun permutations (l s)
  (if (eql s 1)
      ;; Base case: At length 1, its the list itself.
      (mapcar #'list l)
      ;; Recursive case: The Cartesian product of the list and shorter
      ;; permutations.
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y)
                              (cons x y))
                          (permutations l (- s 1))))
              l)))


;; 2 Write a function 'literal-unifiers' whose inputs are (i) a positive
;;   or negative literal (where arguments may be variables and/or constants),
;;   (ii) a state (in the sense defined above), and (iii) a list of constants
;;   (names of objects occurring as predicate arguments in the state, and
;;   possibly additional ones in preconds and effects), and which returns
;;   a list of all unifiers such that any of the unifiers applied to the
;;   input literal makes it true in the input state.

(defun literal-unifiers (literal state consts)
  ;; Check input
  (unless (and (literalp literal)
               (every #'(lambda (s)
                          (and (>= (length s) 1)
                               (every #'constp s))) state)
               (every #'constp consts))
    (return-from literal-unifiers nil))
  (cond
    ;; a. Positive literal of constants
    ((every #'constp literal)
     (and (member literal state :test #'equal) t))
    ;; b. Negative literal of constants
    ((and (eql 'not (car literal))
          (every #'constp (second literal)))
     (not (member (second literal) state :test #'equal)))
    ;; c. Positive matching
    ((positive-literalp literal)
     (prune-unifiers (remove nil (mapcar #'(lambda (l)
                                             (unifier literal l))
                                         state))))
    ;; d. Negative matching
    (t (remove-if #'(lambda (u)
                      (member (apply-unifier u (second literal))
                              state :test #'equal))
                  (distinct-bindings (extract-variables (list literal))
                                     consts)))))


;; 3 Write a function 'operator-instances' whose inputs are
;;   (i) an operator and (ii) a state, and which returns all applicable
;;   instances of the operator in the given state, as a set of bindings
;;   of the operator's parameters.

(defun operator-instances (op state)
  ;; Check input
  (unless (and (op_t-p op) (every #'literalp state))
    (return-from operator-instances nil))
  ;; Constants are the union of constants seen in the state, the
  ;; preconditions, and the effects.
  (let ((consts (remove-duplicates
                 (append (extract-constants state)
                         (extract-constants (op_t-preconds op))
                         (extract-constants (op_t-effects op))))))
    (reduce #'merge-unifiers
            (mapcar #'(lambda (literal)
                        (literal-unifiers literal state consts))
                    (op_t-preconds op)))))

;; Merge two lists of unifiers, returning a list of the unifiers that are
;; correct in both lists.
(defun merge-unifiers (l1 l2)
  (unless (or (null l1) (null l2))
    ;; Literal matches:
    (if (eq l1 t) (return-from merge-unifiers l2))
    (if (eq l2 t) (return-from merge-unifiers l1))
    ;; We have two good unifiers:
    (unless (or (not (every #'unifierp l1))
                (not (every #'unifierp l2)))
      (prune-unifiers
       (loop for u1 in l1 append
            (loop for u2 in l2 when
                 ;; Is the variable not set in u2 or do the values match?
                 (every #'(lambda (v1)
                            (or (not (member (car v1) u2 :key #'first))
                                (member v1 u2 :test #'equal)))
                        u1)
                 collect (union u1 u2 :test #'equal)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;structure for operators
(defstruct op_t
  name
  var-list
  preconds
  effects)
;;structure for node in *graph*
(defstruct node_t
  name
  prev
  state
  next)

;;graph
(defparameter *graph* (make-hash-table))

;;convert state to hash key
(defun state-2-key (state)
  (setf copy_state (copy-list state))
  (setf sorted_state (sort copy_state #'string-lessp :key #'car))
  (sxhash sorted_state))

;;This function does the hard job inserting node into hashtable.
;;Give the node corresponding to state r a name "node_r". This node should 
;;already exist in the global graph *graph*.
;;This function will construct a new node, named s_node, corresponding to
;;state s and insert it into the global graph *graph*.
(defun insert-node (r o s)
  ;;(print "insert new state")
  ;;fetch and modify r node
  (setf copy_r (copy-list r))
  (setf sorted_r (sort copy_r #'string-lessp :key #'car))
  ;;fetch r_node from *graph*
  (setf r_node (gethash (state-2-key sorted_r) *graph*))
  (setf (node_t-next r_node) s)
  
  ;;construct s node and insert to *graph*
  (setf copy_s (copy-list s))
  (setf sorted_s (sort copy_s #'string-lessp :key #'car))
  (setf s_node (make-node_t))
  
  (setf (node_t-prev s_node) (list sorted_r o))
  (setf (node_t-state s_node) sorted_s)
  (setf (node_t-next s_node) nil)
  
  ;;store s_node into *graph* 
  (setf (gethash (state-2-key sorted_s) *graph*) s_node)
  ;;return s on success
  sorted_s)

;;This function stores the state s into *graph* and modify the next of r to 
;;point to state s
(defun store-transition (r o s)
  ;;(print "store-transition")
  (if (not (gethash (state-2-key r) *graph*)) 
      (print "error r does not exist"))
  
  (if (gethash (state-2-key s) *graph*)
      ;;s is already in the graph, return nil
      nil
      ;;s is not in the graph, insert it 
      (insert-node r o s)))

;;effects ((AT-FERRY ?Y) (NOT (AT-FERRY ?X)))
;;unifier ((?x a) (?y c))
;;return ((AT-FERRY C) (NOT (AT-FERRY A)))
(defun instanciate-effects (effects unifier)
  (if (equal effects nil)
      nil
    (cons (first (apply-unifier unifier effects))
	  (instanciate-effects (rest effects) unifier))))

;;This function generates all successor states for a paticular state and a 
;;paticular operator. 
;;unifier is given
;;state  : 
;;op     : 
;;unifier: 
(defun successor-state (state op unifier)
  ;;(print "successor-state")
  (if (equal unifier nil) (return-from successor-state nil))
  (setf effect_instance (instanciate-effects (op_t-effects op) unifier))
  (setf s (combine state effect_instance))
  (setf r state)
  ;;(print "from successor-state to ...")
  ;;(print unifier)
  ;;(print (apply-unifier unifier (op_t-name op)))
  (setf op_instance (apply-unifier unifier (op_t-name op)))
  (store-transition r op_instance s))

;;get all possible states for all operations in op_list of a given state
(defun successor-states (state op_list)
  (let ((u_list (operator-instances (first op_list) state)))
    (cond
      ((equal op_list nil) nil)
      (t 
       (if (equal u_list nil)
	   (successor-states state (rest op_list))
	   (merge-states (do-successor-states state (first op_list) u_list) 
			 (successor-states state (rest op_list))))))))

;;state is the state of a node in graph *graph*
(defun merge-states (s_list1 s_list2)
  (cond 
    ((and (equal s_list1 nil) (equal s_list2 nil)) nil)
    ((and (equal s_list1 nil) (not (equal s_list2 nil))) s_list2)
    ((and (not (equal s_list1 nil)) (equal s_list2 nil)) s_list1)
    (t (do-merge s_list1 s_list2))))

;;s_list1 and s_list2 should not be empty when calling this function
(defun do-merge (s_list1 s_list2)
  (if (equal (first s_list1) nil) 
      s_list2
      (cons (first s_list1) (do-merge (rest s_list1) s_list2))))

;;This function does the hard job to actually gather possible successor 
;;states
(defun do-successor-states (state op u_list)
 (let ((successor_state_instance (successor-state state op (first u_list))))
  (cond 
    ((equal u_list nil)  nil)
    ((equal successor_state_instance nil) 
     (do-successor-states state op (rest u_list)))
    (t (cons successor_state_instance 
	     (do-successor-states state op (rest u_list)))))))

;;This function will look at both state and effect
;;remove the atoms in state that has a negtive in effect
;;add in atoms from effect to state 
(defun combine (state effect)
  (cond 
    ((equal effect nil) state)
    (t (cond
	 ((positive-literalp (first effect)) 
	  (combine (insert-positivelp state (first effect)) (rest effect)))
	 (t (combine (remove-negtivelp state (first effect)) (rest effect)))))))
;;This is a helper function for combine
;;This function check and see if state already has a positive literal in it
;;if there is already a same positive literal, this does not insert pos_lp to
;;state
(defun insert-positivelp (state pos_lp)
  ;;(print "insert-positivelp")
  ;;(print pos_lp)
  (if (not (has-same-poslp state pos_lp))
      (cons pos_lp state) 
      state))

(defun has-same-poslp (state pos_lp)
  (cond
    ((equal state nil) nil)
    ((equal (first state) pos_lp) t)
    (t (has-same-poslp (rest state) pos_lp))))

;;This is a helper function for combine
;;This function check and see if state already has a negtive literal in it
;;If there is a positive conterpart of neg_lp then this function will remove
;;it from the state (i.e. keep all the other literals)
(defun remove-negtivelp (state neg_lp)
  ;;(print "remove-negtivelp")
  ;;(print neg_lp)
  (let ((pos_lp (second neg_lp)))
    (cond 
      ((equal state nil) nil)
      ((equal (first state) pos_lp) (rest state))
      (t (cons (first state) (remove-negtivelp (rest state) neg_lp))))))

;;as long as goal_condition is a subset of state this function should return
;;true otherwise false
(defun check-goal (state goal_condition)
  ;;(print "state in check-goal looks like this")
  ;;(print state)
  (let ((sorted_goal (sort (copy-list goal_condition) #'string-lessp :key #'car))
	(sorted_state (sort (copy-list state) #'string-lessp :key #'car)))
    (has-set sorted_state sorted_goal)))

;;check if goal is a subset of state
(defun has-set (state goal)
  (cond
    ((equal goal nil) t)
    ((has-predicate state (first goal)) (has-set state (rest goal)))
    (t nil)))

;;check if predicate is in state
(defun has-predicate (state predicate)
  (cond
    ((equal state nil) nil)
    ((equal (first state) predicate) t)
    (t  (has-predicate (rest state) predicate))))

;;This function finds a plan for a given goal 
;;If goal is reachalbe, it returns one posibble solution. 
;;If the goal is never reached after this function exhausted all possible 
;;combination of operations, it returns nil
;;This function deploys a BFS strategy when searching for goal condition.
(defun find-plan (op_list init_state goal_condition)
  ;;(print "reset graph")
  (defparameter *graph* (make-hash-table))
  ;;instant success if goal is already reached
  (if (check-goal init_state goal_condition) 
      (return-from find-plan nil))
  (let ((*queue* nil)
	(result nil))  
    (setf copy_init_state (copy-list init_state))
    (setf sorted_init_state (sort copy_init_state #'string-lessp :key #'car))
    ;;create root node with sorted_init_state
    (setf root (make-node_t
		:name "root"
		:prev nil
		:state sorted_init_state 
		:next nil))
    ;;insert root node to *graph*
    (setf (gethash (state-2-key (node_t-state root)) *graph*) root)
    (setf *queue* (append *queue* (list sorted_init_state)))
    (while *queue*
      (let ((r_state (pop *queue*)))
	(when (check-goal r_state goal_condition) 
	  ;;(print "goal reached, print out answer....")
	  ;;DEBUG
	  ;;(print r_state)
	  (setf r_node (gethash (state-2-key r_state) *graph*))
	  (setf result (cons (second (node_t-prev r_node)) result))
	  (let* ((cnt_node (gethash (state-2-key r_state) *graph*))
		 (cnt_state (first (node_t-prev cnt_node))))   
	    (while (not (equal cnt_state nil))
	      ;;DEBUG
	      ;;(print cnt_state)
	      (setf cnt_node (gethash (state-2-key cnt_state) *graph*))
	      (if (not (equal (second (node_t-prev cnt_node)) nil))
		  (setf result (cons (second (node_t-prev cnt_node)) result)))
	      (setf cnt_state (first (node_t-prev cnt_node)))))
	  (return-from find-plan result))
	(setf *queue* (append-states (successor-states r_state op_list) *queue*))))))


;;Function for *queue* operations
;;Append states in s_list1 to the end of s_list2 this simulates the 
;;enqueue operation
(defun append-states (s_list1 s_list2)
  (cond
    ((and (equal s_list1 nil) (equal s_list2 nil)) nil)
    ((equal s_list1 nil) s_list2)
    ((equal s_list2 nil) s_list1)
    (t (do-append-states s_list1 s_list2))))

;;both s_list1 s_list2 are not empty
(defun do-append-states (s_list1 s_list2)
  (if (equal s_list1 nil) 
      s_list2
      (append (do-append-states (rest s_list1) s_list2) (list (first s_list1)))))
