;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;; Autores: Nuno Martinho & João Coelho.

;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")

;; ============= START =============

(defun start ()
"Inicializa o programa"
    (menu)
    (let ((option (read)))
        (if 
            (or (not (numberp option)) (< option 1) (> option 2))
                '((format t "Insira uma opção válida!") (start))
            (ecase option
                ('1 ())
                ('2 (format t "Adeus!"))
            )
        )
    )
)

;; ============= MENU =============

(defun menu ()
"Mostra o menu inicial"
    (progn
        (format t "Loading game...~%")
        (sleep 1)
        (format t "~%o                           o")
        (format t "~%      - Dots and Boxes -     ")
        (format t "~%                             ")
        (format t "~%   1 - Visualizar problemas  ")
        (format t "~%   2 - Escolher um problema  ")
        (format t "~%   3 - Sair                  ")
        (format t "~%o                           o")
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

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (tabuleiro &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (l)
        (format stream "~%~t~t ~a" l)) tabuleiro))
    )
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (tabuleiros &optional (stream t))
"Imprime os tabuleiros do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (tabuleiro)
        (format stream "~%~t~t ~a" (print-tabuleiro tabuleiro))) tabuleiros))
    )
)
