;; Implementação dos algoritmos de procura.
;; Autores: Nuno Martinho & João Coelho.




;; <no>::= (<tabuleiro> <pai> <caixas-objetivo> <g> <h>)
;; definir a estrutura da solucao
;; <solucao>::= (<camiho-solucao> <abertos> <fechados>)

(defun no-teste () 
    '(
        (
		    ((0)(0))  
		    ((0)(1))    
	    )
        nil 1 0 0
     )

)

(defun no-teste-2 () 
    '(
(((0 0 0) (0 0 1) (0 1 1) (0 0 1))
((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
        nil 3 0 0
     )

)

(defun no-teste-3 () 
    '(
        (
		    ((1)(1))  
		    ((1)(1))    
	    )
        nil 1 0 0
     )

)
;; ============ SEARCH ALGORITHMS ============
;; o no inicial vai na lista de abertos
;; (bfs 'expandir-no (list (no-teste)))
(defun bfs (fnExpandir abertos &optional (fechados '()))
    "Algoritmo de proucra em largura primeiro: Breadth-First-Search."
    (cond 
        ( (= (length abertos) 0) NIL)
        (T
            (let*
                (
                    (no-atual (car abertos))
                    (sucessores (funcall fnExpandir no-atual))
                )
                ;;verificar se ha solucao
                (cond
                    (
                        (or
                            (= (- (get-no-objetivo no-atual) (calcular-caixas-fechadas (get-no-estado no-atual))) 0)
                            (= (length sucessores) 0)
                        )

                        (list (get-caminho-solucao no-atual) (length abertos) (length fechados))

                    )
                    (T
                        (bfs 
                            fnExpandir 
                            (append (cdr abertos) (remover-nil (remover-duplicados (remover-duplicados sucessores abertos) fechados))  ) 
                            (append fechados (list no-atual))
                        )
                    )
                )
            )
        )
    )
)

;; (dfs 'expandir-no 3 (list (no-teste)))
(defun dfs (fnExpandir maxProfundidade abertos &optional (fechados '()))   
   "Algoritmo de proucra em profundidade primeiro: Depth-First-Search."
    (cond 
        ( (= (length abertos) 0) NIL)
        ( (> (get-no-g (car abertos) ) maxProfundidade) (dfs fnExpandir maxProfundidade (cdr abertos) (append fechados (list (car abertos)))))
        (T
            (let* 
                (
                    (no-atual (car abertos))
                    (sucessores (funcall fnExpandir no-atual))
                )
                (cond
                    (
                        (or
                            (= (- (get-no-objetivo no-atual) (calcular-caixas-fechadas (get-no-estado no-atual))) 0)
                            (= (length sucessores) 0)
                        )                      
                        (list (get-caminho-solucao no-atual) (length abertos) (length fechados))

                    )
                    (T
                        (dfs fnExpandir maxProfundidade (append sucessores (cdr abertos)) (append fechados (list no-atual)))
                    )
                )
            )
        )
    )
)

(defun a* (fnExpandir fnHeuristica abertos &optional (fechados '()) (numeroExpandidos 0))
    "Algoritmo A*"
    (cond 
        ((= (length abertos) 0) NIL)
        (T
            (let*
                (
                    (no-atual (get-f-mais-baixo abertos))
                    (sucessores (funcall fnExpandir no-atual fnHeuristica))
                    (novos-fechados (trocar fechados) )
                    (abertos-recalculados) ??????
                    (novos-abertos (trocar fechados) ) ??????
                )
                (if (or
                        (= (- (get-no-objetivo no-atual) (calcular-caixas-fechadas (get-no-estado no-atual))) 0) ;;ou ves que a heuristica é 0
                        (= (length sucessores) 0)
                    )
                    (list (get-caminho-solucao no-atual) (length abertos) (length fechados) numeroExpandidos)
                    (a* fnExpandir fnHeuristica abertos fechados (1+ numeroExpandidos) )
                )

            )
        )
    )

)
;; ============ NOS ============

;; (criar-no (test-board) nil 1)
;;( ( ((0)(0))  ((0)(1)) ) nil 1 0 0 )


(defun criar-no (tabuleiro pai caixas-objetivo &optional (g 0) (h 0))
"Constroi a estrutura do no."
  (list tabuleiro pai caixas-objetivo g h)
)

;; ============ SELETORES NOS ============

(defun get-no-estado (no) 
 "Devolve o estado (tabuleiro) de um no."
    (car no)
)

(defun get-no-pai (no)
 "Devolve o no pai deste no."   
    (cadr no)
)

(defun get-no-objetivo (no)
    "Devolve o numero de caixas fechadas deste estado."
    (nth 2 no)
)

(defun get-no-g (no)
    "Devolve o g (profundidade) de um no."
    (nth 3 no)
)

(defun get-no-h (no)
    "Devolve a heuristica de um no."
    (nth 4 no)
)

(defun calcular-no-f (no)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-no-g no) (get-no-h no))
)

(defun get-f-mais-baixo (lista)
    "Devolve o no com o f mais baixo de uma lista."
    (cond
        ((= (length lista) 1) (car lista))
        (T
            (let 
            (
                (outro-no ((get-f-mais-baixo (cdr lista))))
            )
            
                (if (< (calcular-no-f (car lista)) (calcular-no-f outro-no))
                    (car lista)
                    outro-no
                )
            )
        )
    )
)

;; ============ GERAR NOS ============
;; (gerar-nos-horizontal (no-teste))
#|
(
  (
    (((1) (0)) ((0) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
  (
    (((0) (1)) ((0) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
)
|#

(defun gerar-nos-horizontal (no &optional (linha 1) (col 1))
    "Devolve os sucessores de um no, da parte horizontal do tabuleiro. (Começa no index 1)"
    (cond 
        ( (> col (count-colunas (get-no-estado no))) (gerar-nos-horizontal no (1+ linha)))
		( (> linha (count-linhas (get-no-estado no))) '())
        ( (= (get-arco-na-posicao (1- linha) (1- col) (get-arcos-horizontais (get-no-estado no) )) 1)  (gerar-nos-horizontal no linha (1+ col)))

        (T
            (cons 
                (criar-no (arco-horizontal linha col (get-no-estado no)) no (get-no-objetivo no) (1+ (get-no-g no))) 
                (gerar-nos-horizontal no linha (1+ col))
            )    
        )
    )
)
;; (gerar-nos-vertical (no-teste))
#|
(
  (
    (((0) (0)) ((1) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
)
 |#
(defun gerar-nos-vertical (no &optional (linha 1) (col 1))
    "Devolve os sucessores de um no, da parte vertical do tabuleiro. (Começa no index 1)"
    (cond 
        ( (> col (count-colunas (get-no-estado no))) (gerar-nos-vertical no (1+ linha)))
		( (> linha (count-linhas (get-no-estado no))) '())
        ( (= (get-arco-na-posicao (1- linha) (1- col) (get-arcos-verticais (get-no-estado no) )) 1)  (gerar-nos-vertical no linha (1+ col)))

        (T
            (cons 
                (criar-no (arco-vertical col linha (get-no-estado no)) no (get-no-objetivo no) (1+ (get-no-g no))) 
                (gerar-nos-vertical no linha (1+ col))
            )    
        )
    )
)

;; (expandir-no (no-teste))
#|
 (
    ((((1) (0)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
    ((((0) (1)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
    ((((0) (0)) ((1) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
 )
 |#
(defun expandir-no (no)
    "Expande um no e devolve os seus sucessores."
    (append (gerar-nos-horizontal no) (gerar-nos-vertical no))
)

;; gerar children com heuristica

;; ============ SOLUCAO ============
(defun get-caminho-solucao (no)
    "Devolve uma lista de estados do no inicial ate ao no da solucao."
    (cond
        ( (null (get-no-pai no)) (list (get-no-estado no)))
        (T
           (append (get-caminho-solucao (get-no-pai no)) (list (get-no-estado no)) )
        )
    )
)

;; ============ Funcoes auxiliares para procura ============
(defun remover-duplicados (lista1 lista2)
"Remove da lista1 os valores ja existentes na lista2"
  (if (or (null lista1) (null lista2))
      lista1
      (mapcar #'(lambda(elm2) (if (existe-valor elm2 lista2) NIL elm2)) lista1)

      
  )
)

(defun existe-valor (valor lista)
 "Devolve T ou NIL se o valor existe ou nao dentro da lista."
  (eval (cons 'or (mapcar #'(lambda(elemento) (equal (get-no-estado valor) (get-no-estado elemento))) lista)))
)

(defun remover-nil (lista)
    (apply #'append (mapcar #'(lambda(x) (if (null x) NIL (list x))) lista))
)

;; remover nos repetidos de abertos e fechados
;; remover nil ????
;;

(defun heuristica-base (no)
 "Heuristica dada no enunciado: h(x) = o(x) _ c(x) : o(x): objetivo de caixas do tabuleiro, c(x): numero de caixas fechadas"
  (- (get-no-objetivo no) (calcular-caixas-fechadas (get-no-estado no)) )  

)

;; ============ MEDIDAS DE DESEMPENHO ============

;; Estrutura de dados a ser utilizada: (<caminho-solucao> <n-abertos> <n-fechados>)

;; fator de ramificação média
(defun fator-ramificacao-media (lista &optional (L (num-nos-expandidos lista)) (valor-T (num-nos-gerados lista)) (margem-erro 0.1) (b-min 1) (b-max 10e11))
"Retorna o fator de ramificacao media (c/ bisseccao)"
    (let ((b-avg (/ (+ b-min b-max) 2)))
        (cond ((< (- b-max b-min) margem-erro) (/ (+ b-max b-min) 2))
              ((< (aux-ramificacao b-avg L valor-T) 0) (fator-ramificacao-media lista L valor-T margem-erro b-avg b-max))
              (T (fator-ramificacao-media lista L valor-T margem-erro b-min b-avg))      
        )
    )
)

;; B + B^2 + ... + B^L = T
(defun aux-ramificacao (B L valor-T)
 "B + B^2 + ... + B^L = T"
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (aux-ramificacao B (- L 1) valor-T)))
  )
)

(defun tamanho-solucao (lista)
"Retorna o tamanho da solucao"
    (length (car lista))
)

(defun num-nos-gerados (lista)
"Retorna o numero de nos gerados"
    (+ (second lista) (third lista))
)

(defun num-nos-expandidos (lista)
"Retorna o numero de nos expandidos"
    (third lista)
)

(defun num-nos-expandidos-a* (lista)
"Retorna o numero de nos expandidos (a*)"
    (fourth lista)
)

(defun penetrancia (lista)
"Calcula a penetrancia"
    (/ (length (car lista)) (num-nos-gerados lista))
)

(defun no-solucao (lista)
"Retorna o no solucao"
    (nth (1- (length (car lista))) (car lista))
)
