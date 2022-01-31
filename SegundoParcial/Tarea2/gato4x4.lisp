;; =======================================Descripcion del programa====================
;;; Gato con cuadricula 4x4
;;; Este programa trata de dar una propuesta a un juego clasico
;;; haciendo uso de la teoria de juegos
;;; Se usaran los algoritmos de Alfa y Beta junto con el algoritmo
;;; de MinMax para lograr una respuesta de la computadora a una jugada en un tablero
;;; 
;;; Desarrollador: Estrada Bernal José Bryan
;;; Fecha: 06/05/2021
;;; Ubuntu/linux WSL 
;;; Lang: Commond Lisp
;;; Interprete and Compiler: SBLC
;;; =====================================================================================

;;; ===================================Definicion-de-los-parametros======================

(defparameter maximo most-positive-fixnum)
(defparameter minimo most-negative-fixnum)
(defparameter  *output* nil)

(defparameter operators '(
                          (1 0 0)
                          (2 0 1)
                          (3 0 2)
                          (4 0 3) 
                          (5 1 0)
                          (6 1 1)
                          (7 1 2)
                          (8 1 3)
                          (9 2 0)
                          (10 2 1)
                          (11 2 2)
                          (12 2 3)
                          (13 3 0)
                          (14 3 1)
                          (15 3 2)
                          (16 3 3)
                          ))
;;; ==================================Definición de Funciones============================

;;; ==================================endGame============================================
;;; Esta funcion califica si el juego ha terminado ya sea que se gano, se empato 
;;; o perdio
;;; =====================================================================================
(defparameter *evalState* 0)
(defun endGame (state ) 
  (let ((res nil))
 
    (setq res T)
    (loop for x in sta
    te do
      (loop for y in x do 
        (if (null y) 
          (setq res nil)
        )
     ))
  

  res)
  )


(defun evalState (state profundidad)
(let ((turno (evenp profundidad)) (signo1 nil ) (signo2 nil) (lineas1 0) (lineas2 0) (contar1 0) (contar2 0))
  (cond  
    (turno 
    (setq signo1 'O) 
    (setq signo2 'X)
    ) 
    ((not turno) 
    (setq signo1 'X)
    (setq signo2 'O)
    )
    )
  ;;; el que se busca es el signo1 el signo2 es que el interrumpe
  (loop for i from 0 to 3 do 
        (if (equal (nth i (nth i state)) signo1)
          (incf contar1)
        )
        (if (equal (nth i (nth i state)) signo2)
          (incf contar2)
        )
        )
  (cond ((= contar1 contar2 0) 
        (incf lineas1)
        (incf lineas2)
        )
        ((and (> contar1 0) (= contar2 0)) (incf lineas1))
        ((and (> contar2 0) (= contar1 0) )(incf lineas2))
        
        )

  (setq contar2 0)
  (setq contar1 0)
  (do ((i 0 (1+ i))
       (y 3 (1- y))
       )
      ((and (= i 4) (= y -1) ))
      
        (if (equal (nth y (nth i state)) signo1)
          (incf contar1)
        )
        (if (equal (nth y (nth i state)) signo2)
          (incf contar2)
        )
      
      )
    (cond ((= contar1 contar2 0) 
        (incf lineas1)
        (incf lineas2)
        )
        ((and (> contar1 0) (= contar2 0)) (incf lineas1))
        ((and (> contar2 0) (= contar1 0) )(incf lineas2))
        
        )
    ;;; horitzontal
    (loop for i in state 
       do 

    (setq contar2 0)
    (setq contar1 0)
     (loop for y in i do 
        (if (equal y signo1)
          (incf contar1)
        )
        (if (equal y signo2)
          (incf contar2)
        )
     )
      (cond ((= contar1 contar2 0) 
        (incf lineas1)
        (incf lineas2)
        )
        ((and (> contar1 0) (= contar2 0)) (incf lineas1))
        ((and (> contar2 0) (= contar1 0) )(incf lineas2))
        
        )

    )
    (loop for i from 0 to 3 do 
     (setq contar2 0)
     (setq contar1 0)
     (loop for fila in state do 
      (if (equal (nth i fila) signo1)
          (incf contar1)
        )
        (if (equal (nth i fila) signo2)
          (incf contar2)
        )
     )
      (cond ((= contar1 contar2 0) 
        (incf lineas1)
        (incf lineas2)
        )
        ((and (> contar1 0) (= contar2 0)) (incf lineas1))
        ((and (> contar2 0) (= contar1 0) )(incf lineas2))
        
        )

    )
    (- lineas1 lineas2)
)
)
;;; ===================================Aplicar Operador===================================
;;; se pone el simbolo en la posicion que se esta piediendo atraves del operador
;;; el signo que se acomoda en la posicion del operador esta en funcion de la
(defun aplicaOper (state op profundidad)
(let ((turno (evenp profundidad)) (signo nil) (fila (second op)) (columna (third op)) (res (copy-list state)))
(if turno (setq signo 'O) (setq signo 'X)) ;; Cuando es par significa que es el tiro de la pc
(setf (elt res fila) (copy-list (elt res fila)))
(setf (elt  (elt res fila) columna) signo)
;;(format t "~A......~A" state res)
res
)
)

;;; ==================================Nega-Max===========================================
;;; Es la función con la cual se pretende realizar la busqueda en profundidad para poder
;;; encontrar la mejor jugada que se puede hacer en ese momento del juego respecto del
;;; estado
;;; =====================================================================================
(defparameter out 0)
(defparameter maxProf 3)
(defun NegaMax (state  profundidad )
  (let ((MejorMov nil)(MejorValor minimo) (value 0) (newState nil))
  
  (cond 
    ((or (endGame state) (equal profundidad maxProf))
        (format t "~A=========~A" profundidad state) 
        (evalState state profundidad)
        
     )
    (T 
     (dolist (obj operators)
              ;; (format t "state ~A newState ~A profundidad: ~A operador: ~A ~% " state newState profundidad obj)
           ;;(format t "~A" obj) 
      
       (if (null (nth  (third obj) (nth (second obj) state)))
           (progn 
               (setq newState (aplicaOper state obj profundidad))
               (setq value (NegaMax newState (1+ profundidad)))
               (print value)
               (setq value (- value))
               (if (> value MejorValor)
                   (progn (setq MejorValor value) 
                          (setq MejorMov (first obj))
                          (setq out (first obj))
                          )
                   )
             )
           )
       
       )
    MejorValor )
    )
  
  

  ))
(defparameter num 0)
(defun tictactoe (tablero) 
  (NegaMax tablero 0)
  (setq *output* out)
  (print out)  
)


  (tictactoe '(
             (o   x nil   nil )
	     (nil x   o nil)
	     (nil nil  x   x)
	     (o nil  nil  nil)))


