#lang racket

;FACTORIAL
(define(factorial numero) ;se define la funcion factorial
  (cond((zero? numero) 1) ;condicion de parada si numero es igual a 0 devuelve 1
        (else
         (* numero (factorial (- numero 1)))))) ;se multiplica el numero por el factorial del numero-1

;FIBONACCI
(define (fibonacci numero) ;se define la funcion fibonacci
  (cond ((equal? numero 0) 1);condicion de parada si numero es igual a 0 devuelve 1
         ((equal? numero 1) 1);condicion de parada si numero es igual a 0 devuelve 1
          (else
           (+ (fibonacci (- numero 1)) (fibonacci (- numero 2))))))  ; se hace la suma de Fib(n-1) + Fib(n-2)

;MIEMBRO
(define (miembro? symb lista) ;se define la funcion miembro?
  (cond ((null? lista) #f) ;verifica si la lista es vacia y si lo es retorna false
        ((equal? symb (car lista)) #t) ; verifica si el simbolo esperado esta en la primera posicion de la lista y si esta retorna true
        (else
         (miembro? symb (cdr lista))))) ; elimina el primer elemento de la lista y vuelve a llamar a la funcion recursivamente

;ELIMINAR
(define (eliminar symb lista) ;se crea la funcion eliminar
  (cond ((null? lista) '( )) ;si la lista es vacia retorna vacio
        ((equal? symb (car lista)) (eliminar symb (cdr lista))) ;si el simbolo es igual al primer elemento lo elimina con el cdr y llama a la funcion de forma recursiva
        (else
         (cons (car lista) (eliminar symb (cdr lista)))) ;agrega el primer elemento de la lista que no calza en otra y continua iterando con los elementos restantes
    )
)
;QUICKSORT
(define (pivote lista)
  (cond ((null? lista) #f) 
        (else
         (pivoteAux (car lista) (cdr lista) '( ) '( ))) ; se pasa como parametros el primer elemento de la lista, la lista sin ese elemento y dos listas vacias, una para mayores y otra para menores
  )
)
(define (pivoteAux pivot lista menores mayores)
  (cond ((null? lista) (list menores mayores)) ;condicion de parada, si lista es vacia, se retorna una lista con los mayores y otra con los menores ambos en una misma lista
  ((<= (car lista) pivot) ; si el elemento en la primer posicion de la lista es menor o igual al pivote se agrega a lista de menores y se elimina de la lista general
   (pivoteAux pivot (cdr lista) (cons (car lista) menores) mayores))
  (else
   (pivoteAux pivot (cdr lista) menores (cons (car lista) mayores))))) ; si no se cumple la condicion anterior entonces se agrega a la lista de mayores

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

(define (unir lista1 lista2)
  (cond ((null? lista1) lista2) ; si lista1 es nula se retorna lista2
        ((null? lista2) lista1) ; si lista2 es nula se retorna lista1
        (else
         (cons (car lista1)
               (unir (cdr lista1) lista2))))) ; se saca y luego se elimina el primer elemento de la lista y se va uniendo recursivamente