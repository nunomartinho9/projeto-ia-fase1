;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;; Autores: Nuno Martinho & João Coelho.

;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")

;; ============= START =============

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
                                (print-tabuleiros (caadr solucao)))
                            (iniciar)  
                        )
                    )
                    ('3 (format t "Obrigado por jogar!"))
                )
        )
    )
)

;; ============= MENUS =============

(defun menu ()
"Mostra o menu inicial"
    (progn
        (format t "~%A carregar jogo...~%")
        (sleep 1)
        (format t "~%o                              o")
        (format t "~%|      - Dots and Boxes -      |")
        (format t "~%|                              |")
        (format t "~%|   1 - Visualizar problemas   |")
        (format t "~%|   2 - Escolher um problema   |")
        (format t "~%|   3 - Sair                   |")
        (format t "~%o                              o")
        (format t "~%~%>> ")
    )
)


(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%o                                o")
                (format t "~%~% 0 - Voltar atras") (format t "~%~%>> ")
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


(defun algoritmos-menu ()
"Mostra os algoritmos disponiveis no menu"
    (progn
        (format t "~%o                                   o")
        (format t "~%|      - Escolha o algoritmo -      |")
        (format t "~%|                                   |")
        (format t "~%|         1 - Breadth-First         |")
        (format t "~%|          2 - Depth-First          |")
        (format t "~%|              3 - A*               |")
        (format t "~%|             4 - IDA*              |")
        (format t "~%|                                   |")
        (format t "~%|            0 - Voltar             |")
        (format t "~%o                                   o")
        (format t "~%~%>> ")
    )
)

(defun profundidade-menu ()
"Mostra uma mensagem para escolher a profundidade"
    (progn
        (format t "~%o                                                o")
        (format t "~%|      - Defina a profundidade a utilizar -      |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)

;; ============= TABULEIROS =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Le os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

(defun opcao-tabuleiro (menuVoltar)
"Recebe um tabuleiro do menu"
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (funcall menuVoltar))
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

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (tabuleiro &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (l)
        (format stream "~%~t~t ~a" l)) tabuleiro))
    )
    (format t "~%")
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (tabuleiros &optional (stream t))
"Imprime os tabuleiros do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (tabuleiro)
        (format stream "~%~t~t ~a" (print-tabuleiro tabuleiro))) tabuleiros))
    )
    (format t "~%")
)

;; ============= ALGORITMOS =============

;; FUNCAO INACABADA - FALTAM METER AS FUNCOES DOS ALGORITMOS
;; <solucao>::= (<id-tabuleiro> <algoritmo> <caminho-solucao> <profundidade> <hora-inicio> <hora-fim>)
(defun opcao-algoritmo ()
"Recebe a opção de algoritmo do utilizador e executa-o"
    (progn 
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (funcall menuVoltar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opção válida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
                    (T (let* ((no-tabuleiro (opcao-tabuleiro 'opcao-algoritmo))
                                (id-tabuleiro (first no-tabuleiro))
                                (tabuleiro (second no-tabuleiro))
                                (no (list (criar-no tabuleiro nil (!!!!!!!!!!!)))) 
                            )
                    (ecase opcao
                        (1
                            (let ((solucao (list id-tabuleiro (hora-atual))))
                                form*
                            )
                        )
                        (2)
                        (3)
                        (4)
                    )
                  )
                )
            )
        )
    )
)

(defun opcao-profundidade ()
"Recebe um valor de profundidade do utilizador"
    (if (not 'profundidade-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-algoritmo))
                  ((or (not (numberp opcao) (< opcao 0)))
                    (progn
                        (format t "Escolha uma opção válida!~%")
                        (opcao-profundidade)
                    )
                  )
                  (T opcao)
            )
        )
    )
)


;; ============= ESTATISTICAS =============

;; <solucao>::= (<id-tabuleiro> <algoritmo> <caminho-solucao> <profundidade> <hora-inicio> <hora-fim>)

;;(ficheiro-estatisticas '("solucao" "A" "BFS" (hora-atual) (hora-atual)))
(defun ficheiro-estatisticas (solucao)
"Ficheiro de resultados estatisticos (solucao + dados estatisticos sobre a eficiencia)"
    (let* ((
            (id-tabuleiro (first solucao))
            (algoritmo (second solucao))
            (caminho-solucao (third solucao))
            (hora-inicio (fourth solucao))
            (hora-fim (fifth solucao))
           )
        )
        (with-open-file (file "resultados.dat" :direction :output :if-exists :append)
            (ecase algoritmo
                ('bfs ())
                ('dfs ())
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

(defun estatisticas-bfs (stream id-tabuleiro algoritmo caminho-solucao hora-inicio hora-fim &optional profundidade)
"Solução e dados de eficiência para o algoritmo BFS"
    (progn
        (format stream "~% Tabuleiro ~a" id-tabuleiro)
        (format stream "~%  - Algoritmo: ~a" algoritmo)
        (format stream "~%  - Solução encontrada")
        (print-tabuleiro (no-solucao caminho-solucao) stream)
        (format stream "~%  - Fator de ramificação média: ~f" (fator-ramificacao-media caminho-solucao))
        (format stream "~%  - Nº nós gerados: ~a" (num-nos-gerados caminho-solucao))
        (format stream "~%  - Nº nós expandidos: ~a" (num-nos-expandidos caminho-solucao))
        (format stream "~%  - Penetrância: ~f" (penetrancia caminho-solucao))
        (format stream "~%  - Início: ~a Fim: ~a" hora-inicio hora-fim)
    )
)