;;ejercicio 1
(defun ElemInPos(elem lista pos) 
  (let ((posi 0)(resultado nil))
    (defun recursive (lst)
      (cond 
        ((and (/= posi pos)(< posi (length lst) ))
          (setq posi (1+ posi))
          (recursive lst)
          )
        ((and (= pos posi) (=(nth pos lst) elem)) 
         (setq resultado T)
         )
        (T nil)
      )
    )
    (recursive lista)
    resultado
  ) )

(print (ElemInPos 5 '(3 4 5) 2))
;; ejercicio 2

(defun Inicia-en(lista elem )
  (let ( (resul '()) (posi 0) (flag nil))
    (defun recu ()
      (cond 
        ((and (< posi (length lista)) (or (= elem (nth posi lista)) (and  T flag) )) ;;si la posicion es menor que la lista
         (setq flag T)
         (setq resul (append resul (list (nth posi lista))))
         (setq posi (1+ posi))
         (recu)
         )
        ((< posi (length lista))
         (setq posi (1+ posi)) 
         (recu)
         ) 
        (T nil)
        )
      )
      (recu)
      resul
    
    )
  )
(print (Inicia-en '(4 5 6 6 7 8 9) 5))
;;; ejercicio 3 recursivo usando el anterior para reusar codigo 
(defun Termina-en (lista elem)
  (reverse
    (Inicia-en (reverse lista) elem)
    )
  )


(print (Termina-en '(4 5 6 6 7 8 9) 6))

;;; ejercicio 4
(defun Primer-impar(lista)
  (let ( (resultado '() ) (object nil) (pos 0) )
  (defun recursiva ()
    (if (< pos (length lista))
    (progn     
        
    (setq object (nth pos lista))
    (setq pos (1+ pos ))
    (if (and (numberp object) (oddp object))
        (progn
          (setq resultado (list object (position object lista)))
         
         )
         (recursiva)     
        )
    ) )
    
    )
  (recursiva)
  resultado
  )
  )
     
(print (primer-impar '(2 4 7)))
(print (primer-impar '(2 4 4)))

;;;ejercicio 5
(defun Ultimo-elemento(lst)
  (let ((size (length lst)) (valor nil) (var (length lst)) (concurrencia 0))
    ( defun recu()
         (if (> var 0)
         (progn
         (setq var (1- var))
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
           (recu)
         )
         ))
         (recu)
    (list valor concurrencia)
  )
)

(print (Ultimo-elemento '( (5 6  7) 2 7 4 2) ))
;; ejercicio 6
(defun Conteo (lst)
  (let ((conteoNums 0) (conteoSubList 0)(obj nil) (size (length lst)) (conta 0))
    (defun recu ()
      (if (< conta size)
      (progn
        (setq obj (nth conta lst))
        (setq conta (1+ conta))
        (cond 
        ((numberp obj)
         (setq conteoNums (1+ conteoNums))
         )
        ((listp obj)
         (setq conteoSubList (1+ conteoSubList))
         )
        )
        (recu))
      )
      )(recu)
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
  (let ((resultado ' ()) (size (length matriz)) (avance 0) (obj nil))
    (defun recu()
      (if (< avance size)
          (progn
            (setq obj (nth avance matriz))
            (setq resultado (append resultado (list (nth avance obj) ) ))
            (setq avance (1+ avance))
            (recu)
        )
      )
      )
    (recu) 
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
(print (SustiuirRec '( 5 6 () ( 6  7))))
(print (SustiuirRec'()))
;;;Ejercicio 10
(defun Suma-númerica(lst) 
  (let ((acumulador 0 )(contador 0)(size (length lst))(obj nil))
    (defun recu ()
      (if (< contador size)
        (progn
          (setq obj (nth contador lst))
          (if (numberp obj)
          (setq acumulador (+ obj acumulador))
          )
          (setq contador (1+ contador))
          (recu)
        )
        )
      
      )
    (recu)
    acumulador
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
  (let ((response (nth 0 argv)) (size (length argv)) (obj nil) (contador 0)) 
    (defun recu ()
      (cond ( (< contador size)
          (setq obj (nth contador argv))
          (setq contador (1+ contador))
          (if (and response (not obj))
          
          (setq response nil)
          
          )
          (if (not (equal nil response))(
                 setq response (or response obj)
                 ))
          (recu)
      )
            )
      )
    (recu)
    response
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
            (columnas2 (length (first matriz2)))
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
;;Ejercicio 16
(defun cambiar (lst elm1 elm2)
  (defun recu (lista)
    (do 
        ((var 0 (1+ var)))
        ((= var (length lista)) lista)
      (if (equal elm1 (elt lista var)) 
          (setf  (elt lista var) elm2)
          (if (listp (elt lista var))
              (recu (elt lista var)) 
              )
          )
      )
    )
  (recu lst)
  lst 
  )

(print (cambiar '(2 3 ( 4 5 6) 4 5) '(4 5 6) 1))
;;ejercicio 17 Fibonacci 

(defun fib (n)
    "Naive recursive computation of the nth element of the Fibonacci sequence"
      (check-type n (integer 0 *))
        (if (< n 2) n
                  (+ (fib (1- n)) (fib (- n 2)))))
;;(print (time (fib 50 ))) ;;tarda mucho por eso lo comente 

(defun fib2 (n)
    "Tail-recursive computation of the nth element of the Fibonacci sequence"
      (check-type n (integer 0 *))
        (labels ((fib-aux (n f1 f2)
                                       (if (zerop n) f1
                                                                 (fib-aux (1- n) f2 (+ f1 f2)))))
                    (fib-aux n 0 1)))
(print (time (fib2 50)))


(defun fib3 (n)
    "loop-based iterative computation of the nth element of the Fibonacci sequence"
      (check-type n (integer 0 *))
        (loop for f1 = 0 then f2
                      and f2 = 1 then (+ f1 f2)
                              repeat n finally (return f1)))
(print (time (fib3 50)))

(defun fib4 (n)
    "do-based iterative computation of the nth element of the Fibonacci sequence"
      (check-type n (integer 0 *))
        (do ((i n (1- i))
                    (f1 0 f2)
                           (f2 1 (+ f1 f2)))
                  ((= i 0) f1)))
(print (time (fib4 50 )))

(defun fib5 (n)
    "CPS computation of the nth element of the Fibonacci sequence"
      (check-type n (integer 0 *))
        (labels ((fib-aux (n k)
                                       (if (zerop n)
                                                                   (funcall k 0 1)
                                                                                         (fib-aux (1- n) (lambda (x y)
                                                                                                                                                   (funcall k y (+ x y)))))))
                    (fib-aux n #'(lambda (a b) a))))

(print (time ( fib5 50)))

(defun fib6 (n)
     (labels ((fib2 (n)
                                 (cond ((= n 0)
                                                                (values 1 0))
                                                              (t
                                                                                       (multiple-value-bind (val prev-val)
                                                                                                                                      (fib2 (- n 1))
                                                                                                                                                                 (values (+ val prev-val)
                                                                                                                                                                                                            val))))))
             (nth-value 0 (fib2 n))))
(print (time (fib6 50)))

(defun fib7 (n)
    "Successive squaring method from SICP"
      (check-type n (integer 0 *))
        (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                            ((evenp count)
                            (fib-aux a
       b
                                                                                                                                                                                   (+ (* p p) (* q q))
                                                                                                                                                                                                                        (+ (* q q) (* 2 p q))
                                                                                                                                                                                                                                                            (/ count 2)))
                                                                                                 (t (fib-aux (+ (* b q) (* a q) (* a p))
                                                                                                                                                   (+ (* b p) (* a q))
                                                                                                                                                                                         p
                                                                                                                                                                                                                               q
                                                                                                                                                                                                                                                                     (- count 1))))))
                    (fib-aux 1 0 0 1 n)))
(print (time (fib7 50)))

(defun fib8 (n)
    (if (< n 2) n
            (if (oddp n) 
                      (let ((k (/ (1+ n) 2)))
                                (+ (expt (fib8 k) 2) (expt (fib8 (1- k)) 2)))
                            (let* ((k (/ n 2)) (fk (fib8 k)))
                                      (* (+ (* 2 (fib8 (1- k))) fk) fk)))))

(print (time (fib8 50 ))) 

(defun fib9 (n)
    (/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
                  (expt (/ (- 1 (sqrt 5)) 2) n))
            (sqrt 5)))
(print (time (fib9 50 )))
;;ejercicio 18
(defun mapear(fun &rest argv)
  (let ((result '())(aux t) (argc (length argv)) (lst '())(lista '()))
      (defun recu ()
        (setq lst '())
        
        (do ((cont 0(1+ cont)))
            ((= cont argc))
        
          (cond ((equal (elt argv cont) nil)
                 (setq aux nil)
                 ( return)
                 )
              )
          (setq lst (append lst (list (pop (elt argv cont))) ))
          )
        (cond (aux
                (setq result (append result(list (apply fun  lst))))
               (recu)
                ) 
                )
        )
      (recu)
      result
    )
  )
(print (mapear (lambda (x y) (+ x y)) '(1 2 3) '(3 2 1)))
;;ejercicio 19 igual al ejercicio 8 de la lista 2
;;ejercicio 20 
(defun Eliminar (lst num)
  (let ((res '())(cont 0)(size (length lst)))
    (defun recu ()
      (if (< cont size)
          (progn 
            (if (and (numberp (nth cont lst)) (> (nth cont lst ) num ))
              (setq res (append res (list(nth cont lst))))
              )
            (setq cont (1+ cont))
            (recu)
            )
          )
      ) (recu)
    res
   )
  
  )
(print (Eliminar '(1 2  3 89 34 (5 6 3)) 3))

;;ejercicio 21
(defun PegaYCambia(lst1 lst2 elm1 elm2)
  (let ((res1 '()) (res2 '()  )(cont1 0) (cont2 0) )
    (defun recu()
      (cond 
        ((and (< cont1 (length lst1)) (< cont2 (length lst2)))
         (if (and (< cont1 (length lst1)) (not (equal elm1 (nth cont1 lst1))) (not (equal elm2 (nth cont1 lst1))) )
             (setq res1 (append res1(list (nth cont1 lst1))))
             )
         
         (if (and (< cont2 (length lst2)) (not (equal elm1 (nth cont2 lst2))) (not (equal elm2 (nth cont2 lst2))) )
             (setq res2 (append res2 (list (nth cont2 lst2))))
             )
         (setq cont1 (1+ cont1))
         (setq cont2 (1+ cont2))
         (recu)
         )
        )
        )(recu)
    (append res1 res2)
  )
  )
(print (PegaYCambia '(1 2 3 4  5) '(2 4 5 98 ) 2 5))
;;ejercicio 22
(defun qsort (L)
    (cond
          ((null L) nil)
              (t
                     (append
                               (qsort (list< (car L) (cdr L)))
                                       (cons (car L) nil) 
                                               (qsort (list>= (car L) (cdr L)))))))

(defun list< (a b)
    (cond
          ((or (null a) (null b)) nil)
              ((< a (car b)) (list< a (cdr b)))
                  (t (cons (car b) (list< a (cdr b))))))

(defun list>= (a b)
    (cond
          ((or (null a) (null b)) nil)
              ((>= a (car b)) (list>= a (cdr b)))
                  (t (cons (car b) (list>= a (cdr b))))))

(print (qsort '(5 6 3423 5 5 6 3423 5 66)))

