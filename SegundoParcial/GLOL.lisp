;------------------------------------------------------------
; GLOL.lisp
; Resuelve el problema del granjero que cruza el río llevando consigo su lobo, su oveja
; y sus legumbres.
;
; Representación de los estados:
;    Se tendrá una lista con dos sublistas cada una representando una orilla del río. En
;    cada una se encuentran 4 elementos: L(Lobo), O(Oveja), LE(Legumbres) y B(Barca), cada
;    elemento solo será representado con un 1 o un 0, indicando el 1, que se encuentra en
;    la orilla correspondiente.
;               Estado Inicial:               Estado meta:
;              L 0 LE B  L 0 LE B           L 0 LE B  L 0 LE B
;            ((1 1 1 1) (0 0 0 0))        ((0 0 0 0) (1 1 1 1))

; Estructura de los nodos:
;    (id-nodo id-padre estado operador)
;
; Estrada Bernal José Bryan
; Basado en el codigo de Misioneros y Canibales del: Dr. Salvador Godoy C.
;  Abril 14, 2021
;--------------------------------------------------------------

(defparameter *search-space* NIL)  ;Espacio de busqueda visible
(defparameter *memory* NIL)        ;Memoria de nodos analizados
(defparameter *operators* '(
  (:Lobo 0) 
  (:Oveja 1) 
  (:Legumbres 2) 
  (:Ninguno NIL)))
(defparameter *actual-parent* NIL) ;Nodo padre
(defparameter *node-id* -1)        ;Identificador de nodo
(defparameter *solution* NIL)      ;Lista que almacenará los nodos recorridos para llegar a la solución
(defparameter *running-time* NIL)  ;Tiempo que se tardó en encontrar la solución
(defparameter *expanded-nodes* 0)  ;Número de nodos expandidos hasta hallar la solución
(defparameter *max-length* 0)      ;Máxima longitud de la frontera de búsqueda
(defparameter *tries* 0)           ;Intentos para hallar la solución

;----------------------------------------------------------------------
; create-node(estado operador)
;   state - estado del problema
;   operator - operador aplicada al estado padre para llegar al actual
;----------------------------------------------------------------------
(defun create-node (state operator)
  (incf *node-id*)
  (list *node-id* *actual-parent* state operator))

;---------------------------------------------------------------
; insert-state y get-netx-state
;    insert-state recibe estado operador y método de búsqueda
;         :depth-first - Inserta el nodo al principio de la lista
;         :breadth-first - Inserta el nodo al final de la lista
;    get-next-state regresa el siguiente nodo de la búsqueda
;---------------------------------------------------------------
(defun insert-state (state operator method)
  (let ((node (create-node state operator)))
    (cond
      ((eql method :depth-first) (push node *search-space*))
      ((eql method :breadth-first) (setq *search-space* (append *search-space* (list node))))
      (T NIL)
      )))

(defun get-next-state ()
  (pop *search-space*))

;-------------------------------------------------------
; shore
;    Regresa la orilla en la que se encuentra la barca:
;    0 para origen, 1 para destino
;--------------------------------------------------------
(defun shore (state)
  (if (= 1 (fourth (first state))) 0 1))

(defun flip (bit)
  (boole BOOLE-XOR bit 1))

;-------------------------------------------------------------------------------------------------
; valid-operator (estado, operador)
;    Se valida al operador verificando que el personaje que se desea transportar de una orilla a
;    otra, se encuentre en la misma orilla que la barca.
;-------------------------------------------------------------------------------------------------
(defun valid-operator (state operator)
  (let* ((orilla (shore state))
         (character (second operator)))
    (cond
      ((eql (first operator) :Ninguno) T)
      (T (if (= (nth character (nth orilla state)) 1) T nil)))))

;-----------------------------------------------------------------------------------------------
; valid-state (estado)
;    Verifica que el estado sea válido es decir que no se de ninguno de
;    los siguientes casos:
;     -Sólo el lobo y la oveja están en la misma orilla, contraria a la que está la barca
;     -Sólo las legumbres y la oveja están en la misma orilla, contraria a la que está la barca
;-----------------------------------------------------------------------------------------------
(defun valid-state (state)
  (let* ((orilla (flip (shore state)))
         (lobo (first (nth orilla state)))
         (oveja (second (nth orilla state)))
         (legumbre (third (nth orilla state))))
    (cond
      ((and (= 1 lobo) (= 1 oveja) (= 0 legumbre)) NIL)
      ((and (= 1 oveja) (= 1 legumbre) (= 0 lobo)) NIL)
      (T T))))

;----------------------------------------------------------------------------
; app-operator (estado, operador)
;    Aplica el operador al estado
;    Se cambia el valor del personaje que se transporta de una orilla a otra,
;    indicando con un 1, la orilla a la que se ha transportado.
;-----------------------------------------------------------------------------
(defun app-op (state operator)
  (let* ((oper (first operator))
         (new-state (copy-tree state))
         (b1-new-val (flip (fourth (first state))))
         (b2-new-val (flip (fourth (second state)))))
    (setf (fourth (first new-state)) b1-new-val)
    (setf (fourth (second new-state)) b2-new-val)
    (if (not (eql oper :Ninguno))
      (progn
       (let ((charac1-new-val (flip (nth (second operator) (first state))))
             (charac2-new-val (flip (nth (second operator) (second state)))))
         (setf (nth (second operator) (first new-state)) charac1-new-val)
         (setf (nth (second operator) (second new-state)) charac2-new-val))))
    new-state
    ))

;----------------------------------------------------------------
; expand
;    Obtiene los estados resultantes de aplicar cada operador al
;    estado que se proporciona
;----------------------------------------------------------------
(defun expand (state)
  (incf *expanded-nodes*)
  (let ((childs NIL)
        (new-state NIL))
    (dolist (oper *operators* childs)
            (setq new-state (app-op state oper))
            (if (and (valid-operator state oper) (valid-state new-state))
               (setq childs (cons (list new-state oper) childs))))))

;-------------------------------------------------
; duplicated-state y dup-states-filter
;    duplicated-state - Revisa si el estado recibido se encuentra en la lista de
;    memoria de intentos previos.
;    dup-states-filter - Recibe una lista de estados, filtrando aquellos
;     repetidos
;-------------------------------------------------
(defun duplicated-state (state memory-lst)
  (cond
    ((null memory-lst) NIL)
    ((equal state (third (first memory-lst))) T)
    (T (duplicated-state state (rest memory-lst)))))

(defun dup-states-filter (states-lst)
  (cond
    ((null states-lst) NIL)
    ((duplicated-state (first (first states-lst)) *memory*) (dup-states-filter (rest states-lst)))
    (T (cons (first states-lst) (dup-states-filter (rest states-lst))))))

;------------------------------------------------------------------------------------------------
; extract-solution  (node)
;    node - Nodo que contiene el estado meta
;    Rastrea los nodos intermedios recorridos desde el estado inicial al estado meta, que forman
;    parte de la solución, agregando cada uno de ellos a la lista [*solution*].
;------------------------------------------------------------------------------------------------
(defun extract-solution  (node)
  (labels ((find-node (id lst)
            (cond
              ((or (null lst) (null id)) NIL)
              ((= id (first (first lst))) (first lst))
              (T (find-node id (rest lst))))))
          (let ((actual-node (find-node (first node) *memory*)))
            (loop until (null actual-node) do
              (push actual-node *solution*)
              (setq actual-node (find-node (second actual-node) *memory*))))))

;-----------------------------------------------
; display-solution
;    Muestra en pantalla la solución encontrada
;-----------------------------------------------
(defun display-solution ()
  (format t "Solucion en ~a pasos:~%" (- (length *solution*) 1))
  (let ((node NIL))
    (loop for i upto (1- (length *solution*)) do
      (setq node (nth i *solution*))
      (if (= i 0) (format t "Inicio en: ~a~%" (third node))
        (format t "~2a Aplicando ~9a se llega a ~a~%" i (first (fourth node)) (third node))))))

(defun reset-all ()
  (setq *search-space* NIL
        *memory* NIL
        *actual-parent* NIL
        *node-id* -1
        *solution* NIL
        *expanded-nodes* 0
        *max-length* 0
        *tries* 0))

;------------------------------------------------------------------------------------
; blind-search (initial goal method)
;    initial - Estado inicial del problema
;    goal - Estado meta del problema
;    method - Método con el que se realizará la búsqueda
;    Realiza la búsqueda ciega del rpoblema a resolver usando el método indicado por
;    method.
;------------------------------------------------------------------------------------
(defun blind-search (initial goal method)
  (reset-all)
  (insert-state initial NIL method)
  (let ((actual-node NIL)
        (state NIL)
        (operator NIL)
        (childs NIL)
        (goal-reach NIL))
    (loop until (or goal-reach (null *search-space*)) do
      (incf *tries*)
      (setq actual-node (get-next-state)
            state (third actual-node)
            operator (fourth actual-node))
      (push actual-node *memory*)
      (cond
        ((equal goal state) (extract-solution  actual-node)
                            (format t "Meta encontrada en ~a intentos~%" *tries*)
                            (display-solution)
                            (setq goal-reach T))
        (T (setq *actual-parent* (first actual-node))
           (setq childs (expand state))
           (setq childs (dup-states-filter childs))
           (loop for child in childs do
             (insert-state (first child) (second child) method))))
        (if (> (length *search-space*) *max-length*)
          (setq *max-length* (length *search-space*)))
        ))
  )

;;;=======================================================================================
;;; Stats
;;; Estadisticas de desempeño del algoritmo generalizado para depth and breath first search
;;;=======================================================================================
(defun stats ()
  (format t "~%Nodos creados: ~a~%" *node-id*)
  (format t "Nodos expandidos: ~a~%" *expanded-nodes*)
  (format t "Longitud maxima de la frontera de busqueda: ~a~%" *max-length*)
  (format t "Longitud de la solución: ~a operadores aplicados~%" (1- (length *solution*)))
  (format t "Tiempo total: ~a segundos~%" (float *running-time*))
  )

(defparameter time1 NIL)
(defparameter time2 NIL)

(format t "~%Busqueda a lo profundo DFS: ~%")
(setq time1 (get-internal-run-time))
(blind-search '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :depth-first)
(setq time2 (get-internal-run-time))
(setq *running-time* (/ (- time2 time1) internal-time-units-per-second))
(stats)

(format t "~%Busqueda a lo ancho: BFS: ~%")
(setq time1 (get-internal-run-time))
(blind-search '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :breadth-first)
(setq time2 (get-internal-run-time))
(setq *running-time* (/ (- time2 time1) internal-time-units-per-second))
(stats)


