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
(((0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 1 0 0 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0))
((0 0 0 0 0 0 0) (0 0 0 0 1 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0)))
        nil 5 0 0
     )

)
;; ============ SEARCH ALGORITHMS ============

;; ============ NOS ============

;; (criar-no (test-board) nil 1)
;;( ( ((0)(0))  ((0)(1)) ) nil 1 0 0 )


(defun criar-no (tabuleiro pai caixas-objetivo &optional (g 0) (h 0))
"Constroi a estrutura do no."
  (list tabuleiro pai caixas-objetivo g h)
)

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

(defun get-no-f (no)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-no-g no) (get-no-h no))
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
            (cons (criar-no (arco-horizontal linha col (get-no-estado no)) no (get-no-objetivo no) (1+ (get-no-g no))) (gerar-nos-horizontal no linha (1+ col)))    
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

;; ============ Funcoes auxiliares para procura ============
;; remover nos repetidos de abertos e fechados
;; remover nil ????
;;


;; ============ PERFORMANCE MEASURES ============
