;; Código relacionado com o problema.
;; Autores: Nuno Martinho & João Coelho.



;; ============ TABULEIROS PARA TESTE ============
;; '( ((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)) )
(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

(defun tabuleiro-teste-simples ()
  "Retorna um tabuleiro 2x2 (2 arcos na vertical por 2 arcos na horizontal)"
	'(
		((0)(0))
		((0)(1))
	)
)

(defun caixa-fechada ()
  "Retorna um tabuleiro 2x2 (2 arcos na vertical por 2 arcos na horizontal)"
	'(
		((1)(1))
		((1)(1))
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
	"Função que retorna o arco que se encontra numa posicao da lista de arcos horizontais ou verticais. (começa no 0 o index)"
	(if (or (< nLista 0) (< pos 0)) 
		 NIL
		(nth pos (nth nLista listaArcos))
	)
)

;; ============ AUXILIARES ============

;; (substituir 1 (car (get-arcos-horizontais (tabuleiro-teste)))) -> (0 0 0)
;; (1 0 0)
;; (substituir 2 (car (get-arcos-verticais (tabuleiro-teste))) 2) -> (0 1 0)
;; (0 2 0)

;;; TESTAR POIS OUVE ALTERAÇÕES \/ -> STATUS: OK

(defun substituir (index arcsList &optional (x 1))
	"Função que recebe um índice (começa no 1), uma lista e valor x e deverá substituir o elemento nessa posição pelo valor x"
	(cond 
		((= (- index 1) 0) (cons x (cdr arcsList)))
		
		(T (cons (car arcsList) (substituir (- index 1) (cdr arcsList) x)))
	)
)

;; (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste)))
;; ((0 0 0) (0 1 1) (0 1 1) (0 0 1))
;; (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste)))
;; ((0 0 0) (0 1 1) (1 0 1) (1 1 1))

;;; TESTAR POIS OUVE ALTERAÇÕES \/ -> STATUS: OK

(defun arco-na-posicao (listPos arcPos arcsList &optional (x 1))
	"Insere um arco numa lista que representa o conjunto de arcos horizontais ou verticais de um tabuleiro."	
	(cond 
		( (= listPos 1) (cons (substituir arcPos (nth (- listPos 1) arcsList) x) (cdr arcsList)))

		(T (cons (car arcsList)  (arco-na-posicao (- listPos 1) arcPos (cdr arcsList) x)) )

	)
	
)

;;(count-colunas (tabuleiro-teste))
;;3
(defun count-colunas (tabuleiro)
	"Contagem de colunas do tabuleiro"
	(length (car (get-arcos-horizontais tabuleiro)))
)

;;(count-linhas (tabuleiro-teste))
;;4
(defun count-linhas (tabuleiro)
	"Contagem de linhas do tabuleiro"
	(length (get-arcos-horizontais tabuleiro))
)

;; ============ OPERADORES ============

;; arco-horizontal
;; (arco-horizontal 3 1 (tabuleiro-teste))
;; (arco-horizontal 3 2 (tabuleiro-teste))
;; (arco-horizontal 7 2 (tabuleiro-teste))
;; (((0 0 0) (0 0 1) (1 1 1) (0 0 1)) ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))

;;; TESTAR POIS OUVE ALTERAÇÕES \/ -> STATUS: OK
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


;; (existe-caixa-fechada 0 0 (caixa-fechada))
;; T
;; (existe-caixa-fechada 0 0 (tabuleiro-teste-simples))
;; NIL
;;(existe-caixa-fechada 1 0 (tabuleiro-teste-simples))
;; NIL
(defun existe-caixa-fechada (linha coluna tabuleiro) 
	"Verifica num determinado arco com as suas coordenadas, se existe uma caixa fechada num tabuleiro"
	
	(and 
		(=
			(if (not (get-arco-na-posicao linha coluna (get-arcos-horizontais tabuleiro)))
			  0 (get-arco-na-posicao linha coluna (get-arcos-horizontais tabuleiro))		
			) 1
		)
		(=
			(if (not (get-arco-na-posicao (1+ linha) coluna (get-arcos-horizontais tabuleiro)))
			  0 (get-arco-na-posicao (1+ linha) coluna (get-arcos-horizontais tabuleiro))	
			) 1
		)
		(=
			(if (not (get-arco-na-posicao coluna linha (get-arcos-verticais tabuleiro))) 
				0 (get-arco-na-posicao coluna linha (get-arcos-verticais tabuleiro))		
			) 1
		)
		(=
			(if (not (get-arco-na-posicao (1+ coluna) linha (get-arcos-horizontais tabuleiro))) 
			0 (get-arco-na-posicao (1+ coluna) linha (get-arcos-horizontais tabuleiro))		
			) 1
		)
	)
)

;; verificar no tabuleiro quantas caixas fechadas
(defun contar-caixas-fechadas (tabuleiro &optional (linha 0) (col 0))
	"Devolve o numero de caixas fechadas num tabuleiro. (começa no index 0)"
	(cond
		( (>= col (count-colunas tabuleiro)) (contar-caixas-fechadas tabuleiro (1+ linha)))
		( (>= linha (count-linhas tabuleiro)) 0)
		(T
			(+ 
				(if (existe-caixa-fechada linha col tabuleiro) 1 0)

				(contar-caixas-fechadas tabuleiro linha (1+ col))
			)
		)
	)


)

;; calcular quantas caixas ainda faltam fechar (obj - closed) nao fazer por enquanto

;;verificar se pode meter o arco na horizontal num tabuleiro (ou seja eu quero devolver uma posicao em que se possa jogar, procurar sequencial)
;; (onde-jogar-horizontal (tabuleiro-teste))
;; NOT DONE - TA MALI
(defun onde-jogar-horizontal (tabuleiro &optional (linha 1) (arcPos 1))
	"Devolve uma posição onde pode ser feita uma jogada na horizontal (procura sequencial)"
	(cond
		(T 
			(if (not (arco-horizontal linha arcPos tabuleiro))
				(if (> arcPos 3)
					(onde-jogar-horizontal tabuleiro (1+ linha))
					(if (> linha (count-linhas tabuleiro))
						nil
						(onde-jogar-horizontal tabuleiro (1+ arcPos))
					)
				)
				(append (list linha) (list arcPos))
			)
		)
	)
)
;;verificar se pode meter o arco na vertical num tabuleiro
;; saber o numero das possibilidaes pode ser bom :)
;; fazer a jogada