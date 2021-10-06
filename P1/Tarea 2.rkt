#lang racket
;;ELIMINAR

(define (tree node leftNode rightNode)
  (cond( (and (null? leftNode)
              (null? rightNode))
         node)
       (else
        (list node leftNode rightNode))))

(define (root tr)
  (cond ((not(list? tr))
         tr)
        (else
         (car tr))))


(define (leftNode tr)
  (cond ((not(list? tr))
         '( ))
        (else
         (cadr tr))))

(define (rightNode tr)
  (cond ((not(list? tr))
         '( ))
        (else
         (caddr tr))))

(define (mayor tr) ;;devuelve el nodo mayor de un arbol (siempre es el ultimo a la derecha)
  (cond ((null? tr) #f)
        ((null? (rightNode tr)) (root tr))
        (else (mayor (rightNode tr)))))

(define (eliminar ele tr)
  (cond ((null? tr) '( ))
        ((< ele (root tr)) ;;se busca el nodo a eliminar
         (tree (root tr)
               (eliminar ele (leftNode tr))
               (rightNode tr)))
        ((> ele (root tr))
         (tree (root tr)
               (leftNode tr)
               (eliminar ele (rightNode tr))))
        ((and(null? (leftNode tr)) ;;no tiene hijos
             (null? (rightNode tr))) '( ))
        ((null? (leftNode tr)) (rightNode tr)) ;; no existe el hijo derecho
        ((null? (rightNode tr)) (leftNode tr)) ;;no existe el hijo derecho
        (else ;;tiene dos hijos
          (tree (mayor (leftNode tr))
                 (eliminar (mayor (leftNode tr)) (leftNode tr))
                 (rightNode tr)))))
(eliminar 10 '(10 (5 3 8) 18))


;; GRAFO ANCHURA PRIMERO
(define (miembro? symb lista) ;se define la funcion miembro?
  (cond ((null? lista) #f) ;verifica si la lista es vacia y si lo es retorna false
        ((equal? symb (car lista)) #t) ; verifica si el simbolo esperado esta en la primera posicion de la lista y si esta retorna true
        (else
         (miembro? symb (cdr lista))))) ; elimina el primer elemento de la lista y vuelve a llamar a la funcion recursivamente

;;grafo que el profe puso en la presentacion
(define grafoEx '((i (a b))
                  (a (i c d))
                  (b (i c d))
                  (c (a b x))
                  (d (a b f))
                  (x (c))
                  (f (d))))

;; devuelve una solucion cuando llega al nodo destino
(define (sol? final camino)
  (equal? final (car camino)))

;; retorna los vecinos de un nodo
(define (vecinos ele grafo)
   (cond ((equal? (assoc ele grafo) #f) #f)
          (else (cadr (assoc ele grafo)))))
;; se crean nuevos caminos, de no ser posible no lo hace
(define (extender camino grafo)
  (apply append
         (map (lambda(x)
                (cond ((miembro? x camino) '( ))
                      (else (list (cons x camino)))))
              (vecinos (car camino) grafo))))

;; algoritmo de profundidad primero
(define (porAnchura inicio final grafo)
  (porAnchuraAux (list (list inicio)) final grafo '()))
(define (porAnchuraAux caminos final grafo result)
  (cond ((null? caminos)
         (map reverse result))
        ((sol? final (car caminos))
         (porAnchuraAux (cdr caminos) final grafo (cons (car caminos) result)))
        (else
         (porAnchuraAux (append (cdr caminos)
                                (extender (car caminos) grafo))
                        final
                        grafo
                        result))))

(porAnchura 'i 'f grafoEx)