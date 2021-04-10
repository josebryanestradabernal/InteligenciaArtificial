;;ejercicio 1
(defun ElemInPos(elem lista pos) 
  (cond
    ( ( > pos (length lista) ) nil )
    ( (equal (nth pos lista) elem )  T)
    ( T nil )
    )
  ) 

(print (ElemInPos 5 '(3 4 5) 2))
;; ejercicio 2

(defun Inicia-en(lista elem )
  (let ( (pos (position elem lista))  )
    
      (subseq lista pos)
    
    )
  )
(print (Inicia-en '(4 5 6 6 7 8 9) 6))
;;; ejercicio 3 
(defun Termina-en (lista elem)
  (reverse
    (Inicia-en (reverse lista) elem)
    )
  )


(print (Termina-en '(4 5 6 6 7 8 9) 6))

;;; ejercicio 4
(defun Primer-impar(lista)
  (let ( (resultado '() ) )
  (dolist (object lista resultado)
    (if (and (numberp object) (oddp object))
        (progn
          (setq resultado (list object (position object lista)))
          (return)
         )
        )
    )
  resultado
  )
  )
     
(print (primer-impar '(2 4 7)))
(print (primer-impar '(2 4 4)))

;;;ejercicio 5
(defun Ultimo-elemento(lst)
  (let ((size (length lst)) (valor nil) (concurrencia 0))
    ( do ((var size (1- var)) )
         ((= var -1) nil)
         (cond 
           ( (and (equal valor nil) (realp (nth var lst )) 
                (>= (nth var lst) 0) )
             (setq valor (nth var lst))
             (setq concurrencia 1)
            );condicion cuando no se ha encontrado el valor
           ((and (>= concurrencia 1) (equal valor (nth var lst)))
             (setq concurrencia (1+ concurrencia))
            )
           )
         )
    (list valor concurrencia)
  )
)

(print (Ultimo-elemento '( (5 6  7) ) ))
;; ejercicio 6
(defun Conteo (lst)
  (let ((conteoNums 0) (conteoSubList 0))
    (dolist (obj lst)
      (cond 
        ((numberp obj)
         (setq conteoNums (1+ conteoNums))
         )
        ((listp obj)
         (setq conteoSubList (1+ conteoSubList))
         )
        )
      )
    (cons conteoNums conteoSubList)
    )
  )
(print (conteo '( 2 3 (5 a b) (a b ) 6)))

;;; ejercicio 7 
(defun Aplana (lst ) 
  (let ( (resultado '() ) )
    (defun recursive (lst)
      (dolist (obj lst )
        (if (listp obj)
            (recursive obj )
            (setq resultado (append resultado (list obj)))
            )
        )
      )
    (recursive lst)
    resultado
    )
  
  )
(print (Aplana '(5 6 7 8 ( 6 (A C D) 7 8))))
;;;ejeicio 8
(defun Diagonal(matriz)
  (let ((resultado ' ()) (avance 0))
    (dolist (obj matriz resultado)
      (setq resultado (append resultado (list (nth avance obj) ) ))
      (setq avance (1+ avance))
      )
    resultado
    )
  
  )
(print (Diagonal '(
                   (2 3 7)
                   (5 H 8)
                   (9 5 4))
        ) )
(print (Diagonal '()))

;;; ejercicio 9
(defun Sustituir(lst)
  (let ((resultado '()) (elemento nil))
    (dolist (obj lst)
      (cond 
        ((equal nil obj) (setq elemento 'N))
        ((atom obj) (setq elemento 'A))
        ((listp obj) (setq elemento 'L))
        )
      (setq resultado (append resultado (list elemento)))
      )
    resultado
    )
  )
(defun SustiuirRec (lst)
  (let ((size (length lst))(obj nil))
    (defun recursive (i)
      (if (< i size)
        (progn  
        (setq obj (nth i lst))
        (cond  
          ((equal nil obj) (append (list 'N) (recursive (1+ i)) ))
          ((atom obj) (append (list 'A) (recursive (1+ i)) ))
          ((listp obj) (append (list 'L) (recursive (1+ i)) ))
        
          
          )  
        )
        )
      )
       (recursive 0)
    )
  )
(print (Sustituir '( 5 6 () ( 6  7))))
(print (SustiuirRec '( 5 6 () ( 6  7))))
(print (SustiuirRec'()))
;;;Ejercicio 10
(defun Suma-númerica(lst) 
  (let ((acumulador 0 ))
    (dolist (obj lst acumulador)
      (if (numberp obj)
          (setq acumulador (+ obj acumulador))
          )
      )
    
    )
  )
(print (Suma-númerica '(5 6 6.0 N (n n))))
;;; ejercicio 1i
(defun FiltraVocales(lst)
        (let ((obj nil)(size (length lst))(resultado '()))
  (setq lst (remove 'a lst ))
  (setq lst (remove 'e lst )) 
  (setq lst (remove 'i lst ))
  (setq lst (remove 'o lst))
  (setq lst (remove 'u lst))
  (setq size (length lst))
  (do 
      ((var 0 (1+ var)))
      ((= var size ) resultado)
      (setq obj (nth var lst))
    (if (listp obj)
        (setq resultado (append resultado (list  (FiltraVocales obj)  ) ))
        (setq resultado (append resultado (list obj)) )
         )
  
  )
  resultado
  )
        )
(print (FiltraVocales '(a b c d (a z x (a b v)i v o)) ))
;;; ejercicio 12 
( defun FiltraMultiplos (lst num)
        (let ((resultado '()))
          (dolist (obj lst resultado)
            (cond 
              ((and (numberp obj) (not (= 0 (mod obj num))))
                (setq resultado (append resultado (list obj) ))
               )
              ((listp obj) 
                (setq resultado (append resultado  ( list (FiltraMultiplos obj num))))
               )
                
                )
            )
            
          )
          
        )
        
(print (FiltraMultiplos '(1 2 3  4 55 (3 4 5 6) 2) 2))

;;; ejercicio 13
(defun Celdas (lst)
  (let ((contador 0)) 
    (defun recursive(lst1)
      (dolist (obj lst1) 
        (if (consp obj)
            (setq contador (1+ contador))
            )
        (if (listp obj)
            (recursive obj)
            )
        )
      )
    (recursive lst)
    contador
    )
  
  )
(print (Celdas '(1 2 3 (4  4) A (A B (5 7) C) 5)))
;;; ejercicio 14
(defun Implica (&rest argv)
  (let ((response (nth 0 argv))) 
    (dolist (obj argv response)
      (if (and response (not obj))
          (progn 
          (setq response nil)
          (return response)
          )
          )
      (setq response (or response obj))

      )
    )
  )
(print (Implica nil t t t t nil t ))
;;; ejercicio 15 multiplicacion de matriz
(defun Multiplicar(matriz1 matriz2)
  (if (= (length(first matriz1))  (length matriz2)) 
      (let ((matriz3 '())
            (aux '())
            (filas1 (length matriz1)) 
            (filas2 (length matriz2))
            (columnas1 (length (first matriz1)))
            (columnas2 (length (first mtriz2)))
            )
            
            (do ((i 0 (1+ i)))
                ((= i filas1))
                (setq aux '())
                (do ((j 0 (1+ j)))
                    ((= j columnas2))
                        (setq aux (append  (list 0) aux))
                    )
                (setq matriz3 (append  matriz3 (list aux) ))
                )
            ;;; apartir de aqui se encuentra el algoritmo de multiplicacion de matrices
            (do ((i 0 (1+ i)))
                ((= i filas1) matriz3)
                (do ((j 0 (1+ j)))
                    ((= j columnas2) matriz3)
                    (do ((k 0 (1+ k)))
                        ((= k filas2) matriz3)
                        (setf (elt  (elt  matriz3 i) j) (+ (elt  (elt  matriz3 i) j) (* (elt  (elt matriz1 i) k) (elt  (elt  matriz2 k)  j) )  ) )
                        
                        ) 
                        )          
                )
        matriz3
            )
      nil
      )
  )
(print (Multiplicar '((1 2) (4 3) ) '((4 5) (5 5)) ))
