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
                (progn (format t "Escolha uma opcao valida!") (iniciar))
                (ecase opcao
                    ('1 (progn 
                            (let ((tabuleiro (opcao-tabuleiro 'iniciar))) 
                                (if (listp tabuleiro) (print-tabuleiro (second tabuleiro)))
                            )
                            (iniciar)
                        )
                    )
                    ('2 ()
                    )
                    ('3 (format t "Obrigado por jogar!"))
                )
        )
    )
)

;; ============= MENU =============

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

;; ============= TABULEIROS =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Lê os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%o                            o")
                (format t "~%~% 0 - Voltar atras") (format t "~%~%>> ")
            )
        )
        (T (progn
                (if (= i 1) 
                    (progn 
                        (format t "~%o                            o")
                        (format t "~%|    Escolha o tabuleiro:    |")
                    )
                )
                (format t "~%|    ~a - Tabuleiro ~a         |" i (code-char (+ i 64)))
                (tabuleiros-menu (+ i 1) (cdr problemas))
            )
        )
    )
)

(defun opcao-tabuleiro (menuVoltar)
"Escolhe um tabuleiro do menu"
    (progn (tabuleiros-menu)
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
