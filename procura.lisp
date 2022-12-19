;; Implementação dos algoritmos de procura.
;; Autores: Nuno Martinho & João Coelho.

;; ============ SEARCH ALGORITHMS ============



;; ============ FUNCÕES AUXILIARES NO ============
;;<no>::= (<tabuleiro> <pai> <caixas-objetivo> <g> <h>)
;; definir a estrutura da solucao
;; <solucao>::= (<camiho-solucao> <abertos> <fechados>)

;; (criar-no (test-board) nil 1)
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

(defun get-no-profundidade (no)
    "Devolve o g (profundidade) de um no."
    (nth 3 no)
)

(defun get-no-h (no)
    "Devolve a heuristica de um no."
    (nth 4 no)
)

(defun get-no-f (no)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-no-profundidade no) (get-no-h no))
)


;; ============ GERAR NOS ============

;; gerar um nó sucessor
;; (gerar-no-sucessor (tabuleiro-teste) 1 1 'arco-horizontal)
;; (gerar-no-sucessor (tabuleiro-teste) 1 1 'arco-vertical)
;;                  FALTA DAR FIX
(defun gerar-no-sucessor (no listPos arcPos funcao)
    "Gerar um nó sucessor de um nó pai"
    (let ((novoTabuleiro (get-no-estado no)))
        (criar-no
            (funcall funcao listPos arcPos novoTabuleiro)
            no (get-no-objetivo no) (1+ (get-no-profundidade no)) (get-no-h no)
        )
    )
)

;; gerar sucessores
;; (expandir um no) gerar sucessores (expandir no) -> lista de sucessores de um no.
;;                  NÃO TERMINADO
(defun gerar-no-sucessores (no)
    "Gerar nós sucessores de um nó"
    (mapcar #'(lambda (tabuleiro)
        (criar-no tabuleiro no (get-no-objetivo no) (1+ (get-no-profundidade no))))
        (mapcar #'(lambda (posicao)
            (arco-na-posicao (car posicao) (cadr posicao) (get-no-estado no)))
            
        )
    )
)

;; ============ Funcoes auxiliares para procura ============
;; remover nos repetidos de abertos e fechados
;; remover nil ????
;;


;; ============ PERFORMANCE MEASURES ============