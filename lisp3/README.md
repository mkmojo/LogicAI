```lisp
;;This file describes how to test my code for lisp assignment #3
;;To load the code and test setup please use the following command:
;;
;;(load qiu.lisp)
;;(load qiu_test.lisp)
;;
;;NOTE that the "name" field of my operator structure "op_t" have a different format from 
;;those example given online. 
;;The "name" field is '(board ?x ?y). This filed looks like a predicate. 
;;Whereas the "name" field online looks like 'board
;;For example if we later need to define a new operator for Monkey domain.
;;Let's assume the operator has a name "jump-down" and it needs a parameter ?x to denote
;;position, then it may look like this:
;;   (defparameter jump-down
;;     (make-operator 
;;            :name '(jump-down ?x) ;; <-- this part is different
;;            :parameters '(?x)
;;            :preconds '((location ?x)
;;                        (at Box ?x)
;;                        (at Monkey ?x))
;;            :effects '((on-floor ?x)
;;                        (not (on-box)))))
;;
;;I also used slightly different names for my operatos.
;;Here they are:

;;for ferry domain
(defparameter ferry-operators (list ferry-board ferry-sail ferry-debark))

;;for hanoi domain
(defparameter hanoi-operators (list hanoi-move-disk))

;;for monkey domain
(defparameter monkey-operators (list monkey-go-to monkey-climb monkey-push-box 
                                     monkey-get-knife monkey-grab-bananas 
                                     monkey-pick-up-glass monkey-get-water))

;;for logistics domain
(defparameter logistics-operators (list load-plane unload-plane fly
                                        load-truck unload-truck drive))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The following code is used to test the funcionality for each domain
(print "Hanoi")
(print "expected answer:")
(print '((MOVE-DISK D1 D2 P3) (MOVE-DISK D2 D3 P2) (MOVE-DISK D1 P3 D2)
(MOVE-DISK D3 P1 P3) (MOVE-DISK D1 D2 P1) (MOVE-DISK D2 P2 D3)
(MOVE-DISK D1 P1 D2)))
(find-plan hanoi-operators hanoi-inits hanoi-goals)

(print "Ferry")
(print "expected answer:")
(print '((BOARD C1 A) (SAIL A B) (DEBARK C1 B) (SAIL B A) (BOARD C2 A)
       (SAIL A B) (DEBARK C2 B)))
(find-plan ferry-operators ferry-inits ferry-goals)

(print "Monkey init1 goal1")
(print "expected answer:")
(print '((GOTO P2 P1) (PUSH-BOX P4 P2) (GET-KNIFE P4) (PUSH-BOX P3 P4)
       (CLIMB P3) (GRAB-BANANAS P3)))
(find-plan monkey-operators monkey-inits-1 monkey-goals-1)

(print "Monkey init2 goal2")
(print "expected answer:")
(print nil)
(find-plan monkey-operators monkey-inits-1 monkey-goals-2)

(print "Monkey init2 goal1")
(print "expected answer:")
(print '((GOTO P2 P1) (PUSH-BOX P4 P2) (GET-KNIFE P4) (PUSH-BOX P3 P4)
       (CLIMB P3) (GRAB-BANANAS P3)))
(find-plan monkey-operators monkey-inits-2 monkey-goals-1)

(print "Monkey init2 goal2")
(print "expected answer:")
(print '((GOTO P2 P1) (PUSH-BOX P5 P2) (PICK-UP-GLASS P5) (PUSH-BOX P4 P5)
       (GET-KNIFE P4) (PUSH-BOX P3 P4) (CLIMB P3) (GET-WATER P3) 
       (GRAB-BANANAS P3)))
(find-plan monkey-operators monkey-inits-2 monkey-goals-2)

(print "Logistics init1")
(print "expected answer:")
(print '((LOAD-PLANE PACKAGE1 AIRPLANE1 PGH-AIRPORT)
        (FLY AIRPLANE1 PGH-AIRPORT BOS-AIRPORT)
        (UNLOAD-PLANE PACKAGE1 AIRPLANE1 BOS-AIRPORT)))
(find-plan logistics-operators logistics-inits1 logistics-goals)

;;This command may take a little bit time to run
(print "Logistics init2")
(print "expected answer:")
(print '((LOAD-TRUCK PACKAGE1 PGH-TRUCK PGH-PO)
        (DRIVE PGH-TRUCK PGH-PO PGH-AIRPORT PGH)
        (UNLOAD-TRUCK PACKAGE1 PGH-TRUCK PGH-AIRPORT)
        (LOAD-PLANE PACKAGE1 AIRPLANE1 PGH-AIRPORT)
        (FLY AIRPLANE1 PGH-AIRPORT BOS-AIRPORT)
        (UNLOAD-PLANE PACKAGE1 AIRPLANE1 BOS-AIRPORT)))
(find-plan logistics-operators logistics-inits2 logistics-goals)
```
