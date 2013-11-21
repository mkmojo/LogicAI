;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ferry Domain
(setq ferry-board (make-op_t))
(setf (op_t-name ferry-board) '(board ?x ?y))
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
(setf (op_t-name ferry-sail) '(sail ?x ?y))
(setf (op_t-var-list ferry-sail) '(?x ?y))
(setf (op_t-preconds ferry-sail) 
      '((place ?x)
	(place ?y)
	(at-ferry ?x)))

(setf (op_t-effects ferry-sail) 
      '((at-ferry ?y)
	(not (at-ferry ?x))))

(setq ferry-debark (make-op_t))
(setf (op_t-name ferry-debark) '(debark ?x ?y))
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

(setq ferry-inits  '((place a) (place b) (auto c1) (auto c2)
		     (at c1 a) (at c2 a) (at-ferry a) (empty-ferry)))

(defparameter ferry-operators (list ferry-board ferry-sail ferry-debark))

(defparameter ferry-goals
  '((at c1 b)
    (at c2 b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Hanoi Domain
(setq hanoi-move-disk (make-op_t))
(setf (op_t-name hanoi-move-disk) '(Move-Disk ?disk ?below-disk ?new-below-disk))
(setf (op_t-var-list hanoi-move-disk) '(?disk ?below-disk ?new-below-disk))
(setf (op_t-preconds hanoi-move-disk)
      '((disk ?disk)
        (smaller ?disk ?new-below-disk) 
        (on ?disk ?below-disk)
        (clear ?disk)
        (clear ?new-below-disk)))

(setf (op_t-effects hanoi-move-disk)
      '((clear ?below-disk)
        (on ?disk ?new-below-disk)
        (not (on ?disk ?below-disk))
        (not (clear ?new-below-disk))))

(setq hanoi-inits
      '((smaller D1 P1) (smaller D2 P1) (smaller D3 P1)
        (smaller D1 P2) (smaller D2 P2) (smaller D3 P2)
        (smaller D1 P3) (smaller D2 P3) (smaller D3 P3)
        (smaller D1 D2) (smaller D2 D3)
        (clear P2) (clear P3) (clear D1)
        (disk D1) (disk D2) (disk D3)
        (on D1 D2) (on D2 D3) (on D3 P1)))

;; Goal: 3 disk on P3
(defparameter hanoi-goals
  '((on D1 D2) (on D2 D3) (on D3 P3)))

(defparameter hanoi-operators (list hanoi-move-disk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Monkey Domain

(setq monkey-go-to (make-op_t))
(setf (op_t-name monkey-go-to) '(Goto ?x ?y))
(setf (op_t-var-list monkey-go-to) '(?x ?y))
(setf (op_t-preconds monkey-go-to)
      '((location ?x)
        (location ?y)
        (on-floor)
        (at Monkey ?y)))
(setf (op_t-effects monkey-go-to)
      '((at Monkey ?x)
        (not (at Monkey ?y))))

(setq monkey-climb (make-op_t))
(setf (op_t-name monkey-climb) '(Climb ?x))
(setf (op_t-var-list monkey-climb) '(?x))
(setf (op_t-preconds monkey-climb)
      '((location ?x)
        (at Box ?x)
        (at Monkey ?x)))
(setf (op_t-effects monkey-climb)
      '((on-box ?x)
        (not (on-floor))))

(setq monkey-push-box (make-op_t))
(setf (op_t-name monkey-push-box) '(Push-Box ?x ?y))
(setf (op_t-var-list monkey-push-box) '(?x ?y))
(setf (op_t-preconds monkey-push-box)
      '((location ?x)
        (location ?y)
        (at Box ?y)
        (at Monkey ?y)
        (on-floor)))
(setf (op_t-effects monkey-push-box)
      '((at Monkey ?x)
        (not (at Monkey ?y))
        (at Box ?x)
        (not (at Box ?y))))


(setq monkey-get-knife (make-op_t))
(setf (op_t-name monkey-get-knife) '(Get-Knife ?y))
(setf (op_t-var-list monkey-get-knife) '(?y))
(setf (op_t-preconds monkey-get-knife)
      '((location ?y)
        (at Knife ?y)
        (at Monkey ?y)))
(setf (op_t-effects monkey-get-knife)
      '((has-knife)
        (not (at Knife ?y))))

(setq monkey-grab-bananas (make-op_t))
(setf (op_t-name monkey-grab-bananas) '(Grab-Bananas ?y))
(setf (op_t-var-list monkey-grab-bananas) '(?y))
(setf (op_t-preconds monkey-grab-bananas)
      '((location ?y)
        (has-knife)
        (at Bananas ?y)
        (on-box ?y)))
(setf (op_t-effects monkey-grab-bananas)
      '((has-bananas)))

(setq monkey-pick-up-glass (make-op_t))
(setf (op_t-name monkey-pick-up-glass) '(Pick-up-Glass ?y))
(setf (op_t-var-list monkey-pick-up-glass) '(?y))
(setf (op_t-preconds monkey-pick-up-glass)
      '((location ?y)
        (at Glass ?y)
        (at Monkey ?y)))
(setf (op_t-effects monkey-pick-up-glass)
      '((has-glass)
	  (not (at Glass ?y))))

(setq monkey-get-water (make-op_t))
(setf (op_t-name monkey-get-water) '(Get-Water ?y))
(setf (op_t-var-list monkey-get-water) '(?y))
(setf (op_t-preconds monkey-get-water)
      '((location ?y)
	(has-glass)
	(at Waterfountain ?y)
	(at Monkey ?y)
	(on-box ?y)))
(setf (op_t-effects monkey-get-water)
      '((has-water)))

(defparameter monkey-inits-1
  '((location P1) (location P2) (location P3) (location P4)
    (at Monkey P1) (on-floor) (at Box P2) (at Bananas P3)
    (at Knife P4)))

(defparameter monkey-goals-1
  '((has-bananas)))


(defparameter monkey-inits-2
  '((location P1) (location P2) (location P3) (location P4) (location P5)
    (at Monkey P1) (on-floor) (at Box P2) (at Bananas P3) (at Knife P4)
    (at Waterfountain P3) (at Glass P5)))

(defparameter monkey-goals-2
    '((has-bananas) (has-water)))

(defparameter monkey-operators (list monkey-go-to monkey-climb monkey-push-box monkey-get-knife monkey-grab-bananas monkey-pick-up-glass monkey-get-water))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logistics Domain
(defparameter load-truck
  (make-op_t :name '(load-truck ?obj ?truck ?loc)
	     :var-list '(?obj ?truck ?loc)
	     :preconds '((object ?obj)
			 (truck ?truck)
			 (location ?loc)
			 (at ?obj ?loc)
			 (at ?truck ?loc))
	     :effects '((not (at ?obj ?loc))
			(in ?obj ?truck))))

(defparameter load-plane
  (make-op_t :name '(load-plane ?obj ?plane ?loc)
	     :var-list '(?obj ?plane ?loc)
	     :preconds '((object ?obj)
			 (airplane ?plane)
			 (airport ?loc)
			 (at ?obj ?loc)
			 (at ?plane ?loc))
	     :effects '((not (at ?obj ?loc))
			(in ?obj ?plane))))

(defparameter unload-truck
  (make-op_t :name '(unload-truck ?obj ?truck ?loc)
	     :var-list '(?obj ?truck ?loc)
	     :preconds '((object ?obj)
			 (location ?loc)
			 (truck ?truck)
			 (in ?obj ?truck)
			 (at ?truck ?loc))
	     :effects '((at ?obj ?loc)
			(not (in ?obj ?truck)))))

(defparameter unload-plane
  (make-op_t :name '(unload-plane ?obj ?plane ?loc)
	     :var-list '(?obj ?plane ?loc)
	     :preconds '((object ?obj)
			 (location ?loc)
			 (airplane ?plane)
			 (in ?obj ?plane)
			 (at ?plane ?loc))
	     :effects '((at ?obj ?loc)
			(not (in ?obj ?plane)))))

(defparameter fly
  (make-op_t :name '(fly ?plane ?source ?dest)
	     :var-list '(?plane ?source ?dest)
	     :preconds '((airplane ?plane)
			 (airport ?source)
			 (airport ?dest)
			 (at ?plane ?source))
	     :effects '((at ?plane ?dest)
			(not (at ?plane ?source)))))

(defparameter drive
  (make-op_t :name '(drive ?truck ?source ?dest ?city)
	     :var-list '(?truck ?source ?dest ?city)
	     :preconds '((truck ?truck)
			 (location ?source)
			 (location ?dest)
			 (city ?city)
			 (at ?truck ?source)
			 (in-city ?source ?city)
			 (in-city ?dest ?city))
	     :effects '((at ?truck ?dest)
			(not (at ?truck ?source)))))

(defparameter logistics-operators (list load-plane unload-plane fly
                                        load-truck unload-truck drive))
;; Alternatively, to use the symbol names as the operator names:
;; (defparameter logistics-operators '(load-plane unload-plane fly load-truck
;;                                     unload-truck drive))

;;; A three step plan works:
(defparameter logistics-inits1
  '((object package1)
    (truck pgh-truck)
    (truck bos-truck)
    (airplane airplane1)
    (location bos-po)
    (location pgh-po)
    (location bos-airport)
    (location pgh-airport)
    (airport bos-airport)
    (airport pgh-airport)
    (city pgh)
    (city bos)
    (in-city pgh-po pgh)
    (in-city pgh-airport pgh)
    (in-city bos-po bos)
    (in-city bos-airport bos)
    (at package1 pgh-airport)
    (at airplane1 pgh-airport)
    (at bos-truck bos-po)
    (at pgh-truck pgh-po)))

;; This one takes six steps:
(defparameter logistics-inits2
  '((object package1)
    (truck pgh-truck)
    (truck bos-truck)
    (airplane airplane1)
    (location bos-po)
    (location pgh-po)
    (location bos-airport)
    (location pgh-airport)
    (airport bos-airport)
    (airport pgh-airport)
    (city pgh)
    (city bos)
    (in-city pgh-po pgh)
    (in-city pgh-airport pgh)
    (in-city bos-po bos)
    (in-city bos-airport bos)
    (at package1 pgh-po)
    (at airplane1 pgh-airport)
    (at bos-truck bos-po)
    (at pgh-truck pgh-po) ))

(defparameter logistics-goals
  '((at package1 bos-airport)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


