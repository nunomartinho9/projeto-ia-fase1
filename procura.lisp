;; Implementação dos algoritmos de procura.
;; Autores: Nuno Martinho & João Coelho.

;; ============ SEARCH ALGORITHMS ============



;; ============ FUNCÕES AUXILIARES NO ============
;;<no>::= (<tabuleiro> <pai> <caixas-objetivo> <g> <h>)
;; definir a estrutura da solucao
;; <solucao>::= (<camiho-solucao> <abertos> <fechados>)

;; (construct-node (test-board) nil 1)
(defun criar-no (board parent boxesRemaining &optional (g 0) (h 0))
"Constroi a estrutura do no."
  (list board parent boxesRemaining g h)
)

(defun get-no-estado (node) 
 "Devolve o estado (tabuleiro) de um no."
    (car node)
)

(defun get-no-pai (node)
 "Devolve o no pai deste no."   
    (cadr node)
)

(defun get-no-objetivo (node)
    "Devolve o numero de caixas fechadas deste estado."
    (nth 2 node)
)

(defun get-no-profundidade (node)
    "Devolve o g de um no."
    (nth 3 node)
)

(defun get-no-h (node)
    "Devolve a heuristica de um no."
    (nth 4 node)
)

(defun get-no-f (node)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-node-g node) (get-node-h node))
)


;; ============ GERAR NOS ============

;; gerar sucessor
;; (expandir um no) gerar sucessores (expandir no) -> lista de sucessores de um no.
;; 


;; ============ Funcoes auxiliares para procura ============
;; remover nos repetidos de abertos e fechados
;; remover nil ????
;;


;; ============ PERFORMANCE MEASURES ============