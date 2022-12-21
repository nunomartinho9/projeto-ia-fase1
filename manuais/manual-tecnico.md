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

Função ***iniciar***

```lisp
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

