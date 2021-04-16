;;;======================================================================================
;;;  Ranas.lisp
;;;      Resuelve el problema de las Ranas con heuristica, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         La representación de estados consta de una lista con ranas verdes y ranas rojas en los extremos
;;;         mientras que el centro hay un espacio vacio
;;;                 Estado inciial:        Estado meta:
;;;                 R R R V G G G  Pos(V)       G G G V R R R Pos(V)
;;;               ( (R R R V G G G) (3))     ((G G G V R R R) (3) )
;;;
;;;      Estrada Bernal José Bryan 
;;;      Basado en el codigo de Misioneros y Canibales del: Dr. Salvador Godoy C.
;;;  Abril 10, 2021
;;;======================================================================================

;;;-------------------------ALGORITMO----------------------------------------------------


(defparameter  *open* '())              ;; Frontera de busqueda...                                              
(defparameter  *memory* '())            ;; Memoria de intentos previos

(defparameter  *ops*  '( (:avanza-rojo 1 )
                         (:avanza-verde -1)
                         (:salta-rojo1 2)
                         (:salta-verde1 -2)
                         (:salta-rojo2 3)
                         (:salta-verde2 -3)
                         ))

(defparameter  *id*  -1)                ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil) ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)         ;;lista donde se almacenará la solución recuperada de la memoria
(defparameter *expanded-nodes* 0)       ;;; nodos expandidos
(defparameter *max-length* 0)           ;;; maxima longitud de la frontera de busqueda
(defparameter *running-time* NIL)       ;;; tiempo que se tardo el algoritmo de busqueda ya se a los profundo o a lo ancho
;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))



;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos segun el centro y los limites
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<r0> .. <vacio>..<rn>) (#vacio)],
     el operador tiene estructura : [<etiqueta-humana> <numero de celdas a saltar>]"  
  (let*  ((vacio (first (second estado)))                        
          (salto (second op) );; es lo que quiere saltar 
          (posFinal -1) ;;la posicion final en la estara la celda de vacio
          (ranas (first estado))
          )
        (cond
          ((> salto 0);; el salgo es positivo deberia haber un rojo al hacer la resta
           (setq posFinal  (- vacio salto) )
           (and (>= posFinal 0) (equal 'R (nth posFinal ranas)))
           )
          ((< salto 0);; al hacer la suma se debe encontrar un verde
           (setq posFinal (+ vacio (- salto)) )
           (and (>= posFinal 0) (equal 'G (nth posFinal ranas )))
           )
          (T "error")
          )
    )
  )


    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================


(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op] SIN VALIDACIONES"
    (let*  ((salto  (second  op))
	       (vacio (first (second estado)))   ;;es el espacio que esta vacio es decir la roca que se encuentra vacia
               (copia (copy-list(first estado))) ;;es una copia de la lista que contiene a las ranas y el espacio vacio
               (posFinal (- vacio salto))        ;; al espacio vacio se le resta el salto si el salto es positivo el 
                                                 ;; espacio vacio se recorre negativamente
               (rana (nth posFinal copia))
               )                                 ;; el operador solo intercambia donde va el vacio y donde va la rana que se intercambia
                
               (setf (elt copia posFinal) 'V)
               (setf (elt copia vacio) rana)
               (list copia (list posFinal))      ;; Regresa una lista con la forma ((R0...V....Rn)(PosV)) donde R es ranas y posv es la posicion de la roca vacia
      )
    )


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden 
 para este caso especial de las ranas a la hora de aplicar la validacion del operador tambien valida
 que el estado nuevo sera valido por lo que no se realiza valid-state?"
     (incf *expanded-nodes*)
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         
		 (when (valid-operator?  op  estado)
                        (setq nuevo-estado (apply-operator op estado ) )
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<r0> .. <vacio>..<rn>) (#vacio)],
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
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *expanded-nodes* 0)
     (setq  *max-length* 0)
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
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo)
                        (if (> (length *open*) *max-length*)
                              (setq *max-length* (length *open*))
                         )
                        )))))  )
			     
     
;;;=======================================================================================
;;; Stats
;;; Estadisticas de desempeño del algoritmo generalizado para depth and breath first search
;;;=======================================================================================
(defun stats ()
  (format t "~%Nodos creados: ~a~%" *id*)
  (format t "Nodos expandidos: ~a~%" *expanded-nodes*)
  (format t "Longitud maxima de la frontera de busqueda: ~a~%" *max-length*)
  (format t "Longitud de la solución: ~a operadores aplicados~%" (1- (length *solution*)))
  (format t "Tiempo total: ~a segundos~%" (float *running-time*))
  )

(defparameter time1 NIL)
(defparameter time2 NIL)



(format t "~%Busqueda a lo profundo DFS: ~%")
(setq time1 (get-internal-run-time))

(blind-search '((R R R V G G G) (3)) '((G G G V R R R) (3)) :depth-first)
(setq time2 (get-internal-run-time))
(setq *running-time* (/ (- time2 time1) internal-time-units-per-second))
(stats)



(format t "~%Busqueda a lo ancho: BFS: ~%")
(setq time1 (get-internal-run-time))

(blind-search '((R R R V G G G) (3)) '((G G G V R R R) (3)) :breath-first)

(setq time2 (get-internal-run-time))
(setq *running-time* (/ (- time2 time1) internal-time-units-per-second))
(stats)
