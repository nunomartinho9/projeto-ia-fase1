;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;; Autores: Nuno Martinho & João Coelho.

;; ============ CARREGAR FICHEIROS ============
(load "procura.lisp")
(load "puzzle.lisp")

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

(defun menu ()
    "Mostra o menu inicial"
    (format t "Loading game...")
    (sleep 2)
    (format t "~%o                           o")
    (format t "~%      - Dots and Boxes -     ")
    (format t "~%                             ")
    (format t "~%   1 - Escolher um problema  ")
    (format t "~%   2 - Sair                  ")
    (format t "~%o                           o")
    (format t "~%~%>> ")
)