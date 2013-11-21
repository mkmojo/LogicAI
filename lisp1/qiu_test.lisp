;;Test cases for problem 1
;;list of different length--->should return nil
(unifier '(?x ?y ?z) '(a b)) ;===> NIL
;;list of constant but does not match--->should return nil
(unifier '(a b c) '(c a d));===> NIL
;;sample test cases from assignment sheet
(unifier '(a b c) '(a b c));===>T
(unifier '(?x b ?y) '(a b c));===>((?x a) (?y c))
(unifier '(?x ?x ?y) '(a a c));===> ((?x a) (?y c))
(unifier '(?x ?x ?y) '(a b c));===> nil
(unifier '(?x ?y ?z) '(a a a));===> ((?x a) (?y a) (?z a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Test cases for problem 2
(apply-unifier '((?x a) (?y b)) '(on ?x ?y) ) ;===> (on a b)
(apply-unifier '((?x a)) '(on ?x ?y)) ;===> (on a ?y)
(apply-unifier '((?x a) (?y b) (?z c)) '(on ?x ?y));===> (on a b)
(apply-unifier '((?x a) (?y b)) '(on ?u ?v)); ===> (on ?u ?v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Test cases for problem 3 a
(extract-constants '((On Table A) (On Table B) (On C B)) )
(extract-constants '((Clear ?x) (Clear ?y) (not (Sleeping Robbie)) (Empty-handed Robbie) (At Robbie Table))) ;===> (Robbie Table)
(extract-constants '(a (b (c (d (e constant1 constant)))))) ;===>(constant1 constant2)
(extract-constants '()) ;===>nil

;;Test cases for problem 3 b
(prune-unifiers '((?x a) (?y c) (?t b))) ;;===> ((?x a) (?y c) (?t b))
(prune-unifiers '(((?x a) (?y c)) ((?x a) (?t a)))) ;;===>((?x a) (?y c))
(prune-unifiers '(((?x a) (?y c)) ((?x a) (?t a)) ((?t d) (?x a) (?y d)) )) ;;===>((?x a) (?y c))
