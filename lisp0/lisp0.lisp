					;Problem 1
(defun CountOccur (x  lst)
  (cond       ( (null lst)    0 )
	      ( (atom (first lst))
		(cond
		 ( (equal x (first lst))
		   (+ 1 (CountOccur x (rest lst))) )
		 ( t     (CountOccur x (rest lst)) ) 
		 )
		)                             (t    (+ (CountOccur x (first lst)) (CountOccur x (rest lst))))
					      )
  )
					;Problem 2
(defun subexpr (expr1 expr2)
  (cond 
   ((equal expr1 expr2)      t)
   ((null expr2)         nil)
   ((equal expr1 (first expr2))     expr2)
   (t        (subexpr expr1 (rest expr2)))
   )
  )

					;Problem 3


					;Problem 5

(defun fib (n)
  (let p 1)
   (let q 1)
   (let cnt 1)
   (let temp 0)
   (loop 
    (if (= n 1) (return 1)) 
    (if (= n 2) (return 1))
                    (setq temp (+ p q))
                    (setq p q)
                    (setq q temp)
                    (setq cnt (+ 1 cnt))
                    (if (= cnt n)
                        (return temp))
		    )
   (print temp)
   )
