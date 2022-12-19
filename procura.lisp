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

;; Estrutura de dados a ser utilizada: (<solucao> <num-abertos> <num-fechados>)

;; fator de ramificação média
;; "funcao": num-nos-expandidos ou num-nos-expandidos-a*
(defun fator-ramificacao-media (lista &optional (nos-expandidos (num-nos-expandidos lista)) (nos-gerados (num-nos-gerados lista)))
"Retorna o fator de ramificação média"
    
)

(defun tamanho-solucao (lista)
"Retorna o tamanho da solução"
    (length (car lista))
)

(defun num-nos-gerados (lista)
"Retorna o número de nós gerados"
    (+ (second lista) (third lista))
)

(defun num-nos-expandidos (lista &optional algoritmo)
"Retorna o número de nós expandidos"
    (cond ((or (eql algoritmo "bfs") (eql algoritmo "dfs")) (third lista))
          ((eql algoritmo "a*") (fourth lista)) 
            (T (third lista))
    )
)

(defun penetrancia (lista)
"Calcula a penetrância"
    (/ (length (car lista)) (num-nos-gerados lista))
)

(defun no-solucao (lista)
"Retorna o nó solução"
    (nth (1- (length (car lista))) (car lista))
)

(defun hora-atual ()
"Retorna a hora atual (hh mm ss)"
    (multiple-value-bind (s m h)
            (get-decoded-time)
        (list h m s))
)