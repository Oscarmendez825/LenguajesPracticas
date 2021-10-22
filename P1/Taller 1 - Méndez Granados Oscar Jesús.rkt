#lang racket

;Estudiante: Oscar Jesús Méndez Granados
;Carné:2019150432

;---------------------------------------FACTORIAL----------------------------------------

(define(factorial numero) 
  (cond((zero? numero) 1) ;condicion de parada 
        (else
         (* numero (factorial (- numero 1)))))) ;se multiplica el numero por el factorial del numero-1



;---------------------------------------FIBONACCI----------------------------------------

(define (fibonacci numero) 
  (cond ((equal? numero 0) 0);condicion de parada
         ((equal? numero 1) 1);condicion de parada
          (else
           (+ (fibonacci (- numero 1)) (fibonacci (- numero 2))))))  ; se hace la suma de Fib(n-1) + Fib(n-2)



;---------------------------------------MIEMBRO------------------------------------------

(define (miembro? symb lista)
  (cond ((null? lista) #f) ;condicion de parada
        ((equal? symb (car lista)) #t) ; verifica si el simbolo esta en la primera posicion de la lista y retorna true
        (else
         (miembro? symb (cdr lista))))) ; elimina el primer elemento de la lista y vuelve a llamar a la funcion recursivamente



;---------------------------------------ELIMINAR-----------------------------------------


(define (eliminar symb lista) 
  (cond ((null? lista) '( )) ;condicion de parada
        ((equal? symb (car lista)) (eliminar symb (cdr lista))) ;si es igual a el esperado se elimina
        (else
         (cons (car lista) (eliminar symb (cdr lista))))));va recorriendo la lista

;---------------------------------------QUICKSORT----------------------------------------


(define (pivote lista) ;PIVOTE
  (cond ((null? lista) #f) 
        (else
         (pivoteAux (car lista) (cdr lista) '( ) '( ))); primer elemento, resto de la lista, listas para mayores y menores
  )
)

(define (pivoteAux pivot lista menores mayores)
  (cond ((null? lista) (list menores mayores)); si lista es vacia, se retorna una lista con los mayores y otra con los menores
        
  ((<= (car lista) pivot) ; si el elemento en la primer posicion de la lista es menor o igual al pivote se agrega a lista de menores y se elimina de la lista general
   (pivoteAux pivot (cdr lista) (cons (car lista) menores) mayores))
  
  (else
   (pivoteAux pivot (cdr lista) menores (cons (car lista) mayores))))) ;se agrega a la lista de mayores

(define (quicksort lista)
  (cond ((null? lista) '( )) 
        (else
         (unir (unir (quicksort (menores lista)) (list (pivot lista)))
                 (quicksort (mayores lista)))))) ; se unen las listas de menores, el pivote y mayores en una sola lista

(define (pivot lista)
  (car lista)) ; retorna el primer elemento de la lista

(define (mayores-menores lista)
  (pivote lista)) ; retorna la lista con las listas de mayores y menores

(define (mayores lista)
  (cadr (mayores-menores lista))) ; retorna la lista de mayores

(define (menores lista)
  (car (mayores-menores lista))) ; retorna la lista con los menores

;-------------------------------FUNCION ATRIBUTOS AUTOMOVIIL-------------------------------------------
(define (automovil lista1 lista2)
  (cond ((and (empty? lista1) (empty? lista2)) '()); condicion de parada
        (else (cons (list (car lista2) (car lista1)) (automovil (cdr lista1) (cdr lista2)))))) ; recursivamente se asocian los elementos y se hace la llamada recursiva

;---------------------------------ELIMINAR MIEMBRO DE UN ARBOL------------------------------------------

(define (tree node leftNode rightNode) ;definicion del arbol
  (cond( (and (null? leftNode)
              (null? rightNode))
         node)
       (else
        (list node leftNode rightNode))))

(define (root tr) ;Raiz 
  (cond ((not(list? tr))
         tr)
        (else
         (car tr))))


(define (leftNode tr) ;Nodo izquierdo
  (cond ((not(list? tr))
         '( ))
        (else
         (cadr tr))))

(define (rightNode tr) ;Nodo derecho
  (cond ((not(list? tr))
         '( ))
        (else
         (caddr tr))))

(define (mayor tr) ;devuelve el nodo mayor de un arbol (siempre es el ultimo a la derecha)
  (cond ((null? tr) #f)
        ((null? (rightNode tr)) (root tr))
        (else (mayor (rightNode tr)))))

(define (eliminarNodo ele tr)
  (cond ((null? tr) '( ))
        
        ((< ele (root tr)) ;se busca el nodo a eliminar
         (tree (root tr)
               (eliminarNodo ele (leftNode tr))
               (rightNode tr)))
        ((> ele (root tr))
         (tree (root tr)
               (leftNode tr)
               (eliminarNodo ele (rightNode tr))))
        
        ((and(null? (leftNode tr)) ;no tiene hijos
             (null? (rightNode tr))) '( ))
        
        ((null? (leftNode tr)) (rightNode tr)) ;no existe el hijo izquierdo
        
        ((null? (rightNode tr)) (leftNode tr)) ;no existe el hijo derecho
        
        (else ;tiene dos hijos
          (tree (mayor (leftNode tr))
                 (eliminarNodo (mayor (leftNode tr)) (leftNode tr))
                 (rightNode tr)))))


; -----------------------------------GRAFO ANCHURA PRIMERO----------------------------------------------

;grafo que el profe puso en la presentacion
(define grafoEx '((i (a b))
                  (a (i c d))
                  (b (i c d))
                  (c (a b x))
                  (d (a b f))
                  (x (c))
                  (f (d))))

;devuelve una solucion cuando llega al nodo destino
(define (sol? final camino)
  (equal? final (car camino)))

;retorna los vecinos de un nodo
(define (vecinos ele grafo)
   (cond ((equal? (assoc ele grafo) #f) #f)
          (else (cadr (assoc ele grafo)))))

;se crean nuevos caminos, de no ser posible no lo hace
(define (extender camino grafo)
  (apply append
         (myMap (lambda(x)
                (cond ((miembro? x camino) '( ))
                      (else (list (cons x camino)))))
              (vecinos (car camino) grafo))))

;algoritmo de anchura primero
(define (porAnchura inicio final grafo)
  (porAnchuraAux (list (list inicio)) final grafo '()))
(define (porAnchuraAux caminos final grafo result)
  (cond ((null? caminos)
         (myMap myReverse result))
        ((sol? final (car caminos))
         (porAnchuraAux (cdr caminos) final grafo (cons (car caminos) result)))
        (else
         (porAnchuraAux (append (cdr caminos)
                                (extender (car caminos) grafo))
                        final
                        grafo
                        result))))

;-------------------------------FUNCIONES EXTRAS NECESARIAS---------------------------------------------

;unir = append

(define (unir lista1 lista2)
  (cond ((null? lista1) lista2) 
        ((null? lista2) lista1) 
        (else
         (cons (car lista1)
               (unir (cdr lista1) lista2))))) 
;myMap = map

(define (myMap funcion lista)
  (cond ((empty? lista)  '())
        (else (cons (funcion (car lista)) (myMap funcion (cdr lista))))))

;myReverse = reverse

(define (myReverse lista)
  (cond ((null? lista) '())
        (else
         (append (reverse (cdr lista)) (list (car lista))))))

;----------------------------------LLAMADAS DE PRUEBA-----------------------------------------------------
(factorial 9)
(fibonacci 4)
(miembro? 'a '(a b c))
(eliminar 'a '(a b c a))
(quicksort '(2 3 4 1 1 2))
(automovil '(Hatchback Suzuki Forza1 Rojo si Manual)
           '(Tipo Marca Modelo Color AC Transmision))
(eliminarNodo 10 '(10 (5 3 8) 18))
(porAnchura 'i 'f grafoEx)
 
