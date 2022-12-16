;; Deve conter a implementação de:
;; 1. Algoritmo de Procura de Largura Primeiro (BFS)
;; 2. Algoritmo de Procura do Profundidade Primeiro (DFS)
;; 3. Algoritmo de Procura do Melhor Primeiro (A*)
;; 4. Os algoritmos SMA*, IDA* e/ou RBFS (caso optem por implementar o bónus)
;; Implementação dos algoritmos de procura.
;; Autores: Nuno Martinho & João Coelho.

;; ============ SEARCH ALGORITHMS ============



;; ============ NODE ============
;;<node>::= (<state> <parent> <closed-boxes> <g> <h>)

(defun construct-node (board parent closedBoxes &optional (g 0) (h 0))
"Constroi a estrutura do no."
  (list board parent closedBoxes g h)
)

(defun get-node-state (node) 
 "Devolve o estado (tabuleiro) de um no."
    (car node)
)

(defun get-node-parent (node)
 "Devolve o no pai deste no."   
    (cadr node)
)

(defun get-node-closed-boxes (node)
    "Devolve o numero de caixas fechadas deste estado."
    (nth 2 node)
)

(defun get-node-depth (node)
    "Devolve o g de um no."
    (nth 3 node)
)

(defun get-node-h (node)
    "Devolve a heuristica de um no."
    (nth 4 node)
)

(defun get-node-f (node)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-node-g node) (get-node-h node))
)


;; ============ BOXES & ARCS ============

;; ============ PERFORMANCE MEASURES ============