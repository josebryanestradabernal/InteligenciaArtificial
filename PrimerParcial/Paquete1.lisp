(defvar x)
(defvar y)
(defun test (x)
  (* x x)
  )
(PRINT (test 2))
(print (equal 5 6) )

(print (nth 5 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))))
(print (* 366 24 60 60) )
(setq x 10)
(setq y 12)
(print (and (<= x y)(/= x 0)))

(defun cuadratic( a b c)
  (setq x1 (/ (+ (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* a 2)  ))
  (setq x2 (/ (- (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* a 2)  ))
  (list x1 x2)
  
  )
(print (cuadratic 2 7 5))

(print (+(* 2 4) (- 6 8)))

(print (/ (+ 5 (+ -3 4) ) 
	  (+ 6 (/ 2 5))

	  ))

(print (sqrt ( / (+ (- (- -4 (/ 3 8))) 1.4502 )
		(expt -1 (expt (- 3 5) (/ 1 3)))
		)
	     )
       )
;;; parte d del ejercicio 2 

(print (expt 
         (/ (expt (/ 65.402
                     (sqrt -1)
                   
                   ) (/ 1 5)
             
             
             )
          0.17
          
          ) (/ 1 7)
         ))



;;; ejercicio 3 

;;;indique el resultado de evaluar cada una de las siguientes expresiones
(print (cdar '((one two) three four )))
(print (append (cons '(eva lisa ) '(karl sven)) '(eva lisa) '(kart event)))
(print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin )))

(print (remove 'sven '(eva sven lisa sven anna)))
(print (butlast '(karl adam nilsson gregg alisson vilma) 3))
(print  (nth 2 '(a b c d e)))       
(print (nthcdr 2 '(a b c d e)))
(print (intersection '(a b c ) '(x b z c)))
(print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))) )
;;; ejercicio 5 
( defun  RealNoCero (n)
       (and (realp n) (/= n 0)) )      

( print (RealNoCero 5 ))
(print (RealNoCero 0 ))
;;; ejercicio 4
(defvar lst)
(setq lst '( (A . 4) (B . 6 ) (C . 7)))
(print lst )
(defun Recombina (lista)
  ;;; cuerpo de la función Reconmbina
  ;;; listas impropias
  (list 
   (cons (list ( rest (first lst )) (rest (second lst)) ) (first (first lst ))) 
   (cons (list ( rest (second lst )) (rest (third lst)) ) (first (third lst )))
   (cons (list ( rest (third lst )) (rest (second lst)) (rest (first lst))) (first (second lst )))
   )
  
  )
(print (Recombina lst ))
;;;ejercicio 6
(defun Analiza(x) 
  (list  (atom x)
         (numberp x) 
         (listp x) 
         (consp x)
         (equal x '())
         )
  
)

(print (Analiza '(x 6 7) ))

;;; ejercicio 7
(defun Intercala(lst1  lst2)
  (let ( (size ( max (length lst1) ( length lst2) )) (pos 0) (lista '()))
    (if (> size 0)
        (do ((pos 0 (1+ pos)))
            ((= pos size)lista)
            (if (nth pos lst1)
                (push (nth pos lst1) lista)
                )
            (if (nth pos lst2)
                (push (nth pos lst2) lista)
                )
            )
        )
    
        (reverse lista)
    )
  )
(print (Intercala '(2 3 4 5) '(1 2 3 4 5)))
;;; ejercicio 8
(defun MismoTipo(lst1 lst2)
  (let ((response T) )
    (do ((pos 0(1+ pos)) )
        ((or (= pos (length lst1)) (not response) ) nil)
        (if (not (equal (class-of (nth pos lst1) ) (class-of (nth pos lst2)) ))
            (setq response nil )
            
            ) 
    )
    response
  )
  )
(print (MismoTipo '( (3 4 5) 1 3) '(4 5 6)))
(print (MismoTipo '(12 3 (5 6 )  ) '(4 5 (6 7) )))
;, ejercicio 9
(defun APalíndromo (str)
  ;;; regresa el palindromo del texto que se introduzca
  (concatenate 'string str (reverse str))
  )
(print (APalíndromo "Hola") )

;; ejercicio 10
(defun Bisiesto (año)
  ;;;sacodo de la poderosa documentacion de excel
  (if (or (= (MOD  año 400) 0 ) (AND (= (MOD año 4 ) 0) ( /= (MOD año 100) 0)  )  )
      "Bisiesto"
      "No Bisiesto"
      )
  )
(print ( Bisiesto 2004))




