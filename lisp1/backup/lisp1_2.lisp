
(defun apply-unifier (unifier predicate)
  (cond 
    ( (and (atom (first unifier)) (atom (second unifier))) (nsubst (second unifier) (first unifier) predicate)) 
    (t (apply-unifier (car unifier) predicate) (apply-unifier (rest unifier) predicate)))
)

;;(apply-unifier '((?x a) (?yb)) '(on ?x ?y)) ;; ==> (on a b)
;;(apply-unifier '((?x a)) '(on ?x ?y)) ;; ==> (on a ?y)
;;(apply-unifier '((?x a) (?y b) (?z c)) '(on ?x ?y)) ;; ==> (on a b)
;;(apply-unifier '((?x a) (?y b)) '(on ?u ?v));; ==> (on ?u ?v)
;;(apply-unifier '((?x a) (?y b)) '(P ?x b ?y));; ==> (P a b b)