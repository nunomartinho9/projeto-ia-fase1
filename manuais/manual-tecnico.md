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
        ;;(format stream "~% - Fator de ramificação média: ~f" (fator-ramificacao-media caminho-solucao))
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

