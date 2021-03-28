;;;;formalmente a especificacion consta de tres partes
;;;; casos base 
;;;; Casos de inducción
;;;; Caso de Cerrradura
;;;; una fucion recursiva debe seguir la estructura de la especificacion por induccion 
;;;; factorial
(defun factorial (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t 
	  (* n (factorial (- n 1)))
	  )
	)
  )
(print (factorial 3 ))

(defun suma_lista (lista) 
  (cond ((null lista) 0 )
	((null (rest lista))(first lista ))
	(T (+ (first lista)(suma_lista(rest lista))))
    )
  
  )

(print (suma_lista'(2 4 6 8)))

(defun num_elementos (lista) 
  (cond ((null lista) 0 )
	( T ( + 1 (num_elementos( rest lista))))
	)
  )
(print (num_elementos '(5 4 5 6)))

(defun invertir_lista (lista )
  (cond ((null lista) nil ) 
	((null (rest lista)) lista )
	(t ( append ( invertir_lista ( rest lista)) (list (first lista))))
  )
  )
(trace invertir_lista)
(print ( invertir_lista '(a b c)))
;;;;hello como estas banda estoy progrmando desde la terminal con el poderoso vim a ver que sale si no salepues ni modoque se  --load ~/.vim/pack/plugins/start/slimv/slime/start-swank.lispace 
(print "hello")


