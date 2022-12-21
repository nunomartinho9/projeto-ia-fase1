# **Projeto N.º 1** - *Dots and Boxes*

Inteligência Artificial - Escola Superior de Tecnologia de Setúbal  
2022/2023

## ***Manual de Utilizador***

___
Nuno Martinho, n.º 201901769  
João Coelho, n.º 201902001

## Índice

___

- [**Acrónimos e Siglas**](#acrónimos-e-siglas)
- [**Introdução**](#introdução)
- [**Instalação e Utilização**](#instalação-e-utilização)

### **Acrónimos e Siglas**

___

> IDE - Integrated Development Environment (Ambiente de Desenvolvimento Integrado)

### **Introdução**

___
Este manual tem como objetivo ser um guia para a correta utilização do programa ***Dots and Boxes***, uma versão simplificada do jogo original criado por Édoudard Lucas em 1889, desenvolvido em linguagem de programação *LISP*.

Ao contrário da versão original que permite a 2 jogadores jogarem, esta versão apenas permite que o programa analise, através das escolhas do utilizador, um tabuleiro de tamanho variável (linhas * colunas) e que calcule, através do algoritmo escolhido uma solução para esse tabuleiro.

### **Instalação e Utilização**

___

>### Instalação

Para que o programa possa funcionar é necessário possuir um IDE ou um interpretador que compile a linguagem *Common LISP*.

>### Utilização

Para iniciar o programa é necessário executar a função ***(iniciar)***.

![menu iniciar](./images/main_menu.png)

Para continuar a navegar pelos menus basta apenas premir um dos algarismos presentes no ecrã e clicar na tecla ***Enter***.

Poderá utilizar a opção ***1 - Visualizar problemas*** para visualizar todos os tabuleiros/problemas disponíveis.

>### Resolução de um problema

Se selecionou a opção ***2 - Resolver um problema*** irá encontrar o seguinte ecrã:

![menu algoritmos](./images/menu_algoritmos.png)

Escolha um algoritmo de procura dentro dos apresentados introduzindo o número coorespondente.

Depois ser-lhe-á pedido o número objetivo de caixas fechadas que deseja que o programa alcance na solução.

![menu objetivo](./images/caixas_fechadas.png)


    Atenção: em alguns algoritmos poderá ser-lhe pedido para introduzir um valor de *profundidade máxima* ou para selecionar uma heurística. Nesse caso, basta que interaja como nos menus anteriores e siga as instruções. 

Após a escolha do algoritmo poderá selecionar um dos tabuleiros presentes no ficheiro *problemas.dat*.

![menu tabuleiros](./images/menu_tabuleiros.png)

Ao selecionar um dos tabuleiros será calculada e apresentada a solução respetiva tanto no ecrã como num ficheiro externo ***resultados.dat*** criado automaticamente pelo programa.

![solucao](./images/solucao.png)

![resultados](./images/resultados.png)
