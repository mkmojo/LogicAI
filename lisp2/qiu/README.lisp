;;Problem #1:
(distinct-bindings '(?x ?y ?z) '(a b c d e))


;;Problem #2
(setq state '((on a b) (on b table) (clear a)))
;;Test case #1
(literal-unifiers '(on ?y ?z) state '(a b table))
;;(((?Y A) (?Z B)) ((?Y B) (?Z TABLE)))
;;Test case #2
(literal-unifiers '(not (on ?y ?z)) state '(a b table))
;;(((?Y A) (?Z TABLE)) ((?Y B) (?Z A)) ((?Y TABLE) (?Z A))
;; ((?Y TABLE) (?Z B)))

;;Problem #3
;;Once qiu_test.lisp is loaded 
;;op_t is defined 
;;Just use the following command
;;The ferry-board is the operator 
;;The ferry-state is the state, which sholud only contain 
;;ground constanst
(operator-instances ferry-board ferry-state)
