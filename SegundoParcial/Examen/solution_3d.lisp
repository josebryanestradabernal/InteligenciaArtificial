;;;=====================================================================
;;;  Este programa presenta una solución al 
;;;  problema de laberintos 2D como parte del examen
;;;  de la materia de Fundamentos de Inteligencia Artificial
;;;  
;;;  CREADO POR: ESTRADA  BERNAL JOSÉ BRYAN
;;;  FECHA: 26/04/2021
;;;  LENGUAJE: COMMON_LISP
;;;  COMPILADOR-INTERPRETE: SBCL
;;;======================================================================

;;; Libreria de Laberintos
(load "maze_lib.lisp")
(add-algorithm 'depth-first)
(add-algorithm 'best-first)
(add-algorithm 'a*-first)

(defparameter *open*  '())
(defparameter *memory*  '())
;;;=============MOVIMIENTOS EN EL LABERINTO QUE SE PUEDEN REALIZAR=======
;;;=============DEFINICION DE PARAMETROS Y VARIABLES GLOBALES============
(defparameter *ops*  '(
                    
                      (:Arriba 0 )
                      (:Derecha 2)
                      (:Abajo 4)
                     
                     (:Izquierda 6)
 
                       ))

(defparameter *id* -1)
(defparameter *current-ancestor* nil)
(defparameter *solucion* nil)
(defparameter *sol* nil )
(defparameter *meta* nil)

(defun dist (a b)
 (+ (abs (- (first b) (first a)) )(abs (- (second b ) (second a)) ))
)
(defun create-node (estado op &optional cost)
  
  (incf *id*)

  (if cost 
    (list *id* estado *current-ancestor*  op (+ (dist estado *meta* ) cost) cost )
    (list *id* estado *current-ancestor*  op (dist estado *meta* ) )
  )
  

  )
(defun insert-to-open (estado op metodo &optional cost)
  (let ((lst '())(node nil)(n 0) (flag 1)(nodo (create-node estado op cost)) )
    (cond 
      ((eql metodo :depth-first)
       
       (push nodo *open*)
       )
      
      ( (or (eql metodo :best-first) (eql metodo :a*-first))
       (do  ((n (1- (length *open*)) (1- n)))
            ((= n -1))
         (setq node (nth n *open*))
         (if (and (= flag 1)(<= (nth 4 node) (nth 4 nodo)))
             (progn (push nodo lst) (setq flag 0))
             
             )
         (push node lst)
         )
         (if (= flag 1) (push nodo lst))
          (setq *open* lst)
          
       )
      (T Nil))
    
    
    )
  
  )


(defun get-from-open ()
  (pop *open*)
  )
;;;====================VALIDACIÓN DEL ESTADO SIGUIENTE==============
;;; El estado es valido si se puede viajar hacie el, decir que no 
;;; existen paredes que no permitan al viajero continuar su
;;; camino
;;;=================================================================
(defun valid-operator? (op estado )
  (let* (
        (movF (first op))
        (mov (second op))
        (row (first estado ) )
        (col (second estado ))
        (wall (get-cell-walls row col)) 
        (aux nil)
        (cel2 nil)
        (rowL (1- (get-maze-rows)))
        (colL (1- (get-maze-cols)))
        )
    ;;;movimientos normales
     (cond
    ((or (equal 16 (logand wall 16))(equal 17 (logand wall 17)))
    (setq aux (equal (third estado) (up-down movF)))
    )
 
    (T 
    (setq aux T)
    )
    )
    ( if aux
    (case movF
      ( :Arriba
       (and  (= 0 (logand 1 wall)) (> row 0))
       )
       
      ( :Derecha
       (and (= 0 (logand 2 wall)) (< col colL))

       )
      
      ( :Abajo
       (and (= 0 (logand 4 wall))  (< row rowL))
       )
       
       
      ( :Izquierda
       (and  (= 0 (logand 8 wall)) (> col 0))
       )
       
       
    )
    ))
    
    )

(defun up-down(op)
  (if (or (equal op :Derecha ) (equal op :Izquierda))
  1
  0
  )
)
      
(defun apply-operator (op estado) 
  (let* 
    (
        (row (first estado ) )
        (col (second  estado ))
        (walls (get-cell-walls row col))
        (movF (first op))
        (dir nil)
        (mov (second op))
     )
         (cond
      ((equal movF :Arriba)
       (setq row (1- row))
       )
       
      ((equal movF :Derecha)
       (setq col (1+ col))
       )
       
      ((equal movF :Abajo)
       (setq row (1+ row))
       )
        
      ((equal movF :Izquierda)
       (setq col (1- col))
       )
       (T "error")
      )
      

      
       (list row col (up-down movF))

    )

  )

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when  (valid-operator?  op  estado)           ;; se valida el resultado...
			
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )

;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))

(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      
           (setq *sol* (append *sol* (list (second (fourth nodo)))))
           
           )
           )  )
           

(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq *sol* nil )
     )


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil)
    (state nil)
    )
    
      (setq state (list (aref *start* 0) (aref *start* 1)))
      (setq edo-meta (list (aref *goal* 0) (aref *goal* 1)) )
      (setq *meta* edo-meta)   
      (insert-to-open   state  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((and (equal  (first edo-meta)  (first estado)) ( equal  (second edo-meta)  (second estado) )) 
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))
           (format t "~A ~%==============~%" sucesores)
           (if (eql metodo :best-first) (setq sucesores (filter-open sucesores)) )    ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    
            (insert-to-open  (first element)  (second element)  metodo)
            
            )))))  )
(defun depth-first ()
    (blind-search *start* *goal* :depth-first)
    (pop *sol*)
    (setq *solution* *sol*)
)
(defun  remember-state-in-open?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state-in-open?  estado  (rest  lista-memoria))))  )


(defun filter-open (lista-estados-y-ops)
  (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state-in-open? (first (first  lista-estados-y-ops)) *open*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-open  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-open  (rest  lista-estados-y-ops))))) )


(defun best-first ()
    (blind-search *start* *goal* :best-first)
    (pop *sol*)
    (setq *solution* *sol*)
)

(defun a*-first()
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
    (edo-meta nil)
    (edo-inicial nil)
    (metodo :a*-first)
	  (meta-encontrada  nil)
    (cost 0)
    (state nil)
    )
    
      (setq state (list (aref *start* 0) (aref *start* 1) ))
      (setq edo-meta (list (aref *goal* 0) (aref *goal* 1) ) )
      (setq *meta* edo-meta)   
      (insert-to-open   state  nil  metodo 0)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((and (equal  (first edo-meta)  (first estado)) ( equal  (second edo-meta)  (second estado) ))  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
                (setq cost (1+ (nth 5 nodo)))
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))
            (setq sucesores (filter-open sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    
            (insert-to-open  (first element)  (second element)  metodo cost)
            
            ))))) 
            (pop *sol*)
    (setq *solution* *sol*)
)
(start-maze)

