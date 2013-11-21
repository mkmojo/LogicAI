;;(load "qiu.lisp")
;;Problem #1
(distinct-bindings '(?x ?y ?z) '(a b c d e))
;;(((?X A) (?Y B) (?Z C)) ((?X A) (?Y B) (?Z D)) ((?X A) (?Y B) (?Z E))
;;((?X A) (?Y C) (?Z B)) ((?X A) (?Y C) (?Z D)) ((?X A) (?Y C) (?Z E))
;;((?X A) (?Y D) (?Z B)) ((?X A) (?Y D) (?Z C)) ((?X A) (?Y D) (?Z E))
;;((?X A) (?Y E) (?Z B)) ((?X A) (?Y E) (?Z C)) ((?X A) (?Y E) (?Z D))
;;((?X B) (?Y A) (?Z C)) ((?X B) (?Y A) (?Z D)) ((?X B) (?Y A) (?Z E))
;;((?X B) (?Y C) (?Z A)) ((?X B) (?Y C) (?Z D)) ((?X B) (?Y C) (?Z E))
;;((?X B) (?Y D) (?Z A)) ((?X B) (?Y D) (?Z C)) ((?X B) (?Y D) (?Z E))
;;((?X B) (?Y E) (?Z A)) ((?X B) (?Y E) (?Z C)) ((?X B) (?Y E) (?Z D))
;;((?X C) (?Y A) (?Z B)) ((?X C) (?Y A) (?Z D)) ((?X C) (?Y A) (?Z E))
;;((?X C) (?Y B) (?Z A)) ((?X C) (?Y B) (?Z D)) ((?X C) (?Y B) (?Z E))
;;((?X C) (?Y D) (?Z A)) ((?X C) (?Y D) (?Z B)) ((?X C) (?Y D) (?Z E))
;;((?X C) (?Y E) (?Z A)) ((?X C) (?Y E) (?Z B)) ((?X C) (?Y E) (?Z D))
;;((?X D) (?Y A) (?Z B)) ((?X D) (?Y A) (?Z C)) ((?X D) (?Y A) (?Z E))
;;((?X D) (?Y B) (?Z A)) ((?X D) (?Y B) (?Z C)) ((?X D) (?Y B) (?Z E))
;;((?X D) (?Y C) (?Z A)) ((?X D) (?Y C) (?Z B)) ((?X D) (?Y C) (?Z E))
;;((?X D) (?Y E) (?Z A)) ((?X D) (?Y E) (?Z B)) ((?X D) (?Y E) (?Z C))
;;((?X E) (?Y A) (?Z B)) ((?X E) (?Y A) (?Z C)) ((?X E) (?Y A) (?Z D))
;;((?X E) (?Y B) (?Z A)) ((?X E) (?Y B) (?Z C)) ((?X E) (?Y B) (?Z D))
;;((?X E) (?Y C) (?Z A)) ((?X E) (?Y C) (?Z B)) ((?X E) (?Y C) (?Z D))
;;((?X E) (?Y D) (?Z A)) ((?X E) (?Y D) (?Z B)) ((?X E) (?Y D) (?Z C)))
;;
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
(defstruct op_t
  var-list
  preconds
  effects)



(setq ferry-board (make-op_t))
(setf (op_t-var-list ferry-board) '(?x ?y))
(setf (op_t-preconds ferry-board) 
      '((auto ?x)
      (place ?y)
      (at ?x ?y)
      (at-ferry ?y)
      (empty-ferry)))
(setf (op_t-effects ferry-board) 
      '((on ?x Ferry)
      (not (at ?x ?y))
      (not (empty-ferry))))


(setq ferry-sail (make-op_t))
(setf (op_t-var-list ferry-sail) '(?x ?y))
(setf (op_t-preconds ferry-sail) 
     '((place ?x)
      (place ?y)
      (at-ferry ?x)))

(setf (op_t-effects ferry-sail) 
    '((at-ferry ?y)
      (not (at-ferry ?x))))

(setq ferry-debark (make-op_t))
(setf (op_t-var-list ferry-debark) '(?x ?y))
(setf (op_t-preconds ferry-debark) 
     '((auto ?x)
      (place ?y)
      (on ?x Ferry)
      (at-ferry ?y)))
(setf (op_t-effects ferry-debark) 
     '((not (on ?x Ferry))
      (at ?x ?y)
      (empty-ferry)))

(setq ferry-state  '((place a) (place b) (auto c1) (auto c2)
    (at c1 a) (at c2 a) (at-ferry a) (empty-ferry)))
