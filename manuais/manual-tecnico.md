# **Projeto N.º 1** - *Dots and Boxes*

Inteligência Artificial - Escola Superior de Tecnologia de Setúbal
2022/2023   

## ***Manual Técnico***
___
Nuno Martinho, n.º 201901769  
João Coelho, n.º 201902001

### **Introdução**  
___
Neste manual técnico é abordada a implementação de um programa em *LISP* que tem como objetivo resolver tabuleiros do jogo *Dots and Boxes*.  
O objetivo deste é permitir que o utilizador possa receber uma solução possível do número de jogadas num tabuleiro de modo a completar o objetivo de caixas fechadas.  

### **Organização do projeto**  
___
O projeto encontra-se organizado em 3 ficheiros de código:  

- **projeto.lisp** - carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.  
- **puzzle.lisp** - implementação da resolução do problema.  
- **procura.lisp** - implementação dos algoritmos de procura.  

### **projeto.lisp**
___
Neste ficheiro encontram-se funções relativas ao carregamento, leitura e escrita de ficheiros, bem como à interação com o utilizador.  
O programa é iniciado ao executar a função ***iniciar*** que apresenta um menu principal com 3 opções:  

1. mostar um tabuleiro entre todos os disponíveis no ficheiro ***problemas.dat***
2. resolver um tabuleiro
3. sair do programa.

```lisp
;; Função iniciar
(defun iniciar ()
"Inicializa o programa"
    (menu)
    (let ((opcao (read)))
        (if
            (or (not (numberp opcao)) (< opcao 1) (> opcao 3))
                (progn (format t "Escolha uma opção válida!") (iniciar))
                (ecase opcao
                    ('1 (progn
                            (let ((tabuleiro (opcao-tabuleiro 'iniciar)))
                                (if (listp tabuleiro) (print-tabuleiro (second tabuleiro)))
                            )
                            (iniciar)
                        )
                    )
                    ('2 (progn
                            (let ((solucao (opcao-algoritmo)))
                                (progn
                                    (format t "~%Tabuleiro ~a" (first solucao))
                                    (format t "~%  - Algoritmo ~a" (second solucao))
                                    (format t "~%  - Objetivo: ~a" (third solucao))
                                    (format t "~%  - Solucao:")
                                    (print-tabuleiro (car (fifth solucao)))
                                )
                            )
                            (iniciar)
                        )
                    )
                    ('3 (format t "Obrigado por jogar!"))
                )
        )
    )
)
```

Menu principal  
![menu iniciar](./images/main_menu.png)

Para que seja possível apresentar os tabuleiros contidos no ficheiro ***problemas.dat*** é usada uma função ***ler-tabuleiros*** para ler esse mesmo ficheiro.

```lisp
;; Função ler-tabuleiros
(defun ler-tabuleiros ()
"Le os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)
```

Ao selecionar a primeira opção *Visualizar problemas* o utilizador verá então no ecrã uma lista com todos os tabuleiros lidos a partir da função anterior. Poderá depois selecionar um dos tabuleiros para poder ver o mesmo impresso no ecrã.  

```lisp
;; Função tabuleiros-menu: apresenta os tabuleiros ao utilizador
(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%|                                |")
                (format t "~%|        0 - Voltar atras        |") 
                (format t "~%o                                o")
                (format t "~%~%>> ")
            )
        )
        (T (progn
                (if (= i 1) 
                    (progn 
                        (format t "~%o                                o")
                        (format t "~%|    - Escolha o tabuleiro: -    |")
                        (format t "~%|                                |")
                    )
                )
                (format t "~%|        ~a - Tabuleiro ~a         |" i (code-char (+ i 64)))
                (tabuleiros-menu (+ i 1) (cdr problemas))
            )
        )
    )
)
```

```lisp
;; Função opcao-tabuleiro: permite ao utilizador escolher um tabuleiro
(defun opcao-tabuleiro (voltar)
"Recebe um tabuleiro do menu"
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (funcall voltar))
                  ((not (numberp opcao)) (progn (format t "Escolha uma opção válida~%")))
                  (T
                    (let ((lista (ler-tabuleiros)))
                        (if (or (< opcao 0) (> opcao (length lista)))
                            (progn 
                                (format t "Escolha uma opcao valida!") (opcao-tabuleiro 'tabuleiros-menu)
                            )
                            (list opcao (nth (1- opcao) lista))
                        )
                    )
                  )
            )
        )
    )
)
```

Menu tabuleiros  
![menu tabuleiros](./images/menu_tabuleiros.png)

Voltando ao menu principal, temos a segunda opção do menu ***Resolver um problema*** que permite ao utilizador escolher um algortimo, um tabuleiro, o objetivo de caixas fechadas e, se aplicável, profundidade máxima. No fim, é calculada a solução e apresentada no ecrã do utilizador, voltando de seguida ao menu inicial.

Também é criado ou ficheiro ***resultados.dat*** com resultados mais detalhados de todas as execuções realizadas.

Menu algoritmos  
![menu algoritmos](./images/menu_algoritmos.png)

Menu Objetivo Caixas Fechadas  
![menu_objetivo](./images/caixas_fechadas.png)

Menu Profundidade  
![menu_profundidade](./images/menu_profundidade.png)

```lisp
;; Função opcao-algoritmo: trata a opção de algoritmo do utilizador
(defun opcao-algoritmo ()
"Recebe a opção de algoritmo do utilizador e executa-o"
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opção válida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
                    (T (let* (
                                (no-tabuleiro (opcao-tabuleiro 'opcao-algoritmo))
                                (objetivo (opcao-objetivo))
                                (id-tabuleiro (code-char (+ (first no-tabuleiro) 64)))
                                (tabuleiro (second no-tabuleiro))
                                (no (list (criar-no tabuleiro nil objetivo))) 
                            )
                        (ecase opcao
                            (1
                                (let ((solucao (list id-tabuleiro 'BFS objetivo (hora-atual) (bfs 'expandir-no no) (hora-atual))))
                                    (progn 
                                        (ficheiro-estatisticas solucao) 
                                        solucao
                                    )
                                )
                            )
                            (2
                                (let* (
                                        (profundidade (opcao-profundidade))
                                        (solucao (list id-tabuleiro 'DFS objetivo (hora-atual) (dfs 'expandir-no profundidade no) (hora-atual) profundidade))
                                    )
                                    (progn
                                        (ficheiro-estatisticas solucao)
                                        solucao
                                    )
                                )
                            )
                            (3)
                        )
                    ))
            )
        )
    )
)
```

```lisp
;;; Funções auxiliares para criação do ficheiro output resultados.dat

(defun ficheiro-estatisticas (solucao)
"Ficheiro de resultados estatisticos (solucao + dados estatisticos sobre a eficiencia)"
    (let* (
            (id-tabuleiro (first solucao))
            (algoritmo (second solucao))
            (objetivo (third solucao))
            (hora-inicio (fourth solucao))
            (caminho-solucao (fifth solucao))
            (hora-fim (sixth solucao))
            (profundidade (seventh solucao))
           )

        (with-open-file (file "resultados.dat" :direction :output :if-does-not-exist :create :if-exists :append)
            (ecase algoritmo
                ('bfs (estatisticas file id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim))
                ('dfs (estatisticas file id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim profundidade))
                ('a* ())
                ('ida* ())
            )
        )
    )
)

(defun hora-atual ()
"Retorna a hora atual (hh mm ss)"
    (multiple-value-bind (s m h)
            (get-decoded-time)
        (format nil "~a:~a:~a" h m s))
)

(defun estatisticas (stream id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim &optional profundidade)
"Solução e dados de eficiência para os algoritmos"
    (progn
        (format stream "~%Tabuleiro ~a" id-tabuleiro)
        (format stream "~% - Algoritmo: ~a" algoritmo)
        (format stream "~% - Objetivo: ~a caixas" objetivo)
        (format stream "~% - Solução encontrada")
        (print-tabuleiro (no-solucao caminho-solucao) stream)
        (format stream "~% - Fator de ramificação média: ~f" (fator-ramificacao-media caminho-solucao))
        (if (eql algoritmo 'DFS)
            (format stream "~% - Profundidade máxima: ~a" profundidade)
        )
        (format stream "~% - Nº nós gerados: ~a" (num-nos-gerados caminho-solucao))
        (format stream "~% - Nº nós expandidos: ~a" (num-nos-expandidos caminho-solucao))
        (format stream "~% - Penetrância: ~f" (penetrancia caminho-solucao))
        (format stream "~% - Início: ~a" hora-inicio)
        (format stream "~% - Fim: ~a~%~%" hora-fim)
    )
)
```

### **puzzle.lisp**
___
Aqui estão as funções relativas à resolução problema em si

Foram implementadas algumas funções de tabuleiros de teste mais básicos para ir retificando a implementação das funções.

![tabuleiros_teste](./images/tabuleiros-teste.png)

### Funções seletoras e auxiliares
Estão também implementadas algumas funções seletoras e auxiliares, tais como:

```lisp
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
(defun arco-na-posicao (listPos arcPos arcsList &optional (x 1))
 "Insere um arco numa lista que representa o conjunto de arcos horizontais ou verticais de um tabuleiro. (Começa no indice 1)" 
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
```

### Funções operadores

Estão também implementadas funções que permitem inserir peças no tabuleiro, verificar se existem caixas fechadas