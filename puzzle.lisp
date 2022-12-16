;; Código relacionado com o problema.
;; Autores: Nuno Martinho & João Coelho.



;;; Tabuleiro
;; '( ((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)) )
(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

;; ============ SELETORES ============

;; (get-arcos-horizontais (tabuleiro-teste))
;; ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
(defun get-arcos-horizontais (tabuleiro)
	"Retorna a lista dos arcos horizontais de um tabuleiro."
	(car tabuleiro)
)

;; (get-arcos-verticais (tabuleiro-teste))
;; ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun get-arcos-verticais (tabuleiro)
	"Retorna a lista dos arcos verticiais de um tabuleiro."
	(car(cdr tabuleiro))
)

;; (get-arco-na-posicao 2 3 (get-arcos-horizontais (tabuleiro-teste)))
;; 1
(defun get-arco-na-posicao (nLista pos listaArcos)
	"Função que retorna o arco que se encontra numa posicao da lista de arcos horizontais ou verticais."
	(if (or (= nLista 0) (= pos 0)) 
		 NIL
		(nth (- pos 1) (nth (- nLista 1) listaArcos))
	)
)

;; ============ AUXILIARES ============

;; (substituir 1 (car (get-arcos-horizontais (tabuleiro-teste)))) -> (0 0 0)
;; (1 0 0)
;; (substituir 2 (car (get-arcos-verticais (tabuleiro-teste))) 2) -> (0 1 0)
;; (0 2 0)

(defun substituir (index arcsList &optional (x 1))
	"Função que recebe um índice, uma lista e valor x e deverá substituir o elemento nessa posição pelo valor x"
	(cond 
		((= (- index 1) 0) (cons x (cdr arcsList)))
		
		(T (cons (car arcsList) (substituir (- index 1) (cdr arcsList) x)))
	)
)

;; (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste)))
;; ((0 0 0) (0 1 1) (0 1 1) (0 0 1))

;; (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste)))
;; ((0 0 0) (0 1 1) (1 0 1) (1 1 1))

(defun arco-na-posicao (listPos arcPos arcsList &optional (x 1))
	"Insere um arco numa lista que representa o conjunto de arcos horizontais ou verticais de um tabuleiro."	
	(cond 
		( (= listPos 1) (cons (substituir arcPos (nth (- listPos 1) arcsList) x) (cdr arcsList)))

		(T (cons (car arcsList)  (arco-na-posicao (- listPos 1) arcPos (cdr arcsList) x)) )

	)
	
)

;; ============ OPERADORES ============

;; arco-horizontal
;; (arco-horizontal 3 1 (tabuleiro-teste))
;; (arco-horizontal 3 2 (tabuleiro-teste))
;; (arco-horizontal 7 2 (tabuleiro-teste))
;; (((0 0 0) (0 0 1) (1 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
(defun arco-horizontal (listPos arcPos tabuleiro &optional (x 1))
	"Função que recebe dois índices e o tabuleiro e coloca um arco horizontal nessa posição."
	(cond
		( (> listPos (length (get-arcos-horizontais tabuleiro)) ) NIL)
		( (> arcPos (length (car (get-arcos-horizontais tabuleiro))) ) NIL)	
		( (= (get-arco-na-posicao listPos arcPos (get-arcos-horizontais tabuleiro)) 1) NIL)
		(T 
			(list (arco-na-posicao listPos arcPos (get-arcos-horizontais tabuleiro) x) 
				  (get-arcos-verticais tabuleiro)) 
		)
	)

)



;; arco-vertical
;; (arco-vertical 1 2 (tabuleiro-teste))
;; ((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (1 1 1) (1 0 1) (0 1 1))
;; (arco-vertical 2 2 (tabuleiro-teste))
;; (arco-vertical 5 5 (tabuleiro-teste))
(defun arco-vertical (arcPos listPos tabuleiro &optional (x 1))
	"Função que recebe dois índices e o tabuleiro e coloca um arco vertical nessa posição."
	(cond
		( (> listPos (length (get-arcos-verticais tabuleiro)) ) NIL)
		( (> arcPos (length (car (get-arcos-verticais tabuleiro))) ) NIL)	
		( (= (get-arco-na-posicao listPos arcPos (get-arcos-verticais tabuleiro)) 1) NIL)
		(T 
			(list (get-arcos-horizontais tabuleiro) 
				  (arco-na-posicao listPos arcPos (get-arcos-verticais tabuleiro) x)) 
		)
	)

)


;; ============ NODE EXPANSION ============