# Sudoku

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo (de acordo com a Planilha de Divisão dos Grupos)**: 01<br>
**Paradigma**: Lógico<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 19/0023376 |   Abraão Alves Ribeiro |
| 19/0094257 |   Paulo Henrique Rezende |
| 19/0047968 |   Paulo Vitor Silva Abi| Acl 
| 23/2022952 |   Leonardo de Souza Takehana| 
| 19/0026600 |   Davi Marinho da Silva Campos | 
| 19/0105267 |   Diógenes Dantas Lélis Júnior| 
| 19/0103302 |   Bernardo Chaves Pissutti |
| 17/0161871 |   Denniel William Roriz Lima| 
| 18/0113321 |   Francisco Mizael Santos da Silva | 
| 19/0093196 |   Mateus Caltabiano Neves Frauzino| 
| 19/0091720 |   Lucas Macedo Barboza |

## Sobre 
O projeto proposto tem como objetivo implementar um jogo de Sudoku, que gera um tabuleiro aleatório para o jogador resolver. O jogo inclui um sistema de vidas, onde os jogadores perdem uma vida ao fazer uma jogada incorreta. Quando todas as vidas se esgotam, o jogador deve recomeçar o jogo do zero.

A lógica do Sudoku foi desenvolvida em Prolog e se comunica com a interface, construída utilizando HTML, CSS e JavaScript, através de uma API também escrita em Prolog. A interface pode ser acessa através do ```localhost:80``` . Todos esses serviços são orquestrados pelo Docker Compose, que facilita o processo de teste e execução do ambiente.

## Screenshots
<!-- Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento. -->
#### A seguir temnos a interface de programação (API) do jogo:

1. Requisição de início de jogo

![image](https://github.com/user-attachments/assets/89961492-039d-41e9-9acd-27caf6234dba)

2. Requisição de alteração do valor de célula

![image](https://github.com/user-attachments/assets/1427ce73-3e71-4fbf-a7e4-6658c86815a7)

#### Abaixo está a interface gráfica desenvolvida em HTML, CSS, e JS:

![image](https://github.com/user-attachments/assets/68686725-72d0-4561-9b14-7f026dc18443)

#### A seguir um gif mostrando o jogo em execução:

![ezgif-1-9acede5c00](https://github.com/user-attachments/assets/a53ae80b-64f6-4407-8482-8890321d736d)

## Instalação 
**Linguagens**: Prolog, HTML, CSS<br>
**Tecnologias**: Docker, swipl<br>
<!-- Descreva os pré-requisitos para rodar o seu projeto e os comandos necessários.
Insira um manual ou um script para auxiliar ainda mais.
Gifs animados e outras ilustrações são bem-vindos! -->
1. Instalar o Docker em https://docs.docker.com/get-docker/
2. Instalar o Docker compose em seu ambiente 
###  Linux
Para instalação no Linux siga as instruções da página https://docs.docker.com/desktop/install/linux-install/
### MacOS
Para instalação no MacOS siga as instruções da página https://docs.docker.com/desktop/install/mac-install/
### Windows
Para instalação no Windows siga as instruções da página https://docs.docker.com/desktop/install/windows-install/

3. Dê clone do projeto
```
git clone https://github.com/UnBParadigmas2024-1/2024.1_G1_Funcional_Filtro_Kalman.git
```
4. Entre na pasta
```
cd 2024.1_G1_Logico_Sudoku
```
5. Inicie o projeto
```
docker-compose up --build
```
 Ou
```
docker compose up --build
```
6. Acessar Interface

Abra o navegador e acesse o `http://localhost:80`

Obs: Para linux é necessário utilizar o comendo sudo antes

## Uso 
1. Após iniciar o projeto, clique no botão `Começar` e as células do tabuleiro serão preenchidas.
![image](https://github.com/user-attachments/assets/68686725-72d0-4561-9b14-7f026dc18443)
2. Para adicionar números às celulas, basta clicar na célula desejada e digitar um número. Caso não conheça, as regras básicas do sudoku podem ser encontradas aqui: [Regras básicas do Sudoku](https://sudoku.com/pt/regras-do-sudoku/)
3. Assim que você adicionar o número em uma celula o jogo irá verificar se a resposata está correta, se tiver, a sua contagem de vida continuará a mesma, se o seu palpite estiver errado a contagem de vida irá diminuir.
4. Por fim, para reiniciar, basta apertar em `Começar` novamente.

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) |
| -- | -- | -- |
| Paulo Henrique  | Trabalhei no subgrupo "Resolver tabuleiro e exibir dicas", desta forma criei as funções responsáveis por fazer validação dos números que não podem se repertir para a coluna, linha e quadrante. Como o prolog faz a varredura de forma horizontal no vetor da matriz, tive que criar a função de transposição da matriz, para que programa conseguisse ler os elementos da coluna. | Excelente |
| Paulo Vitor Silva Abi Acl  | No geral atuei no subgrupo "Selecionar dificuldade e gerar tabuleiro", mais especificamente na parte de criar o tabuleiro pronto para jogo e também na parte da seleção de dificuldade | Boa |
| Leonardo de Souza Takehana  | Atuei um pouco em todas as areas do projeto, com foco na parte lógica e organizei os grupos de trabalho	| Excelente |
| Davi Marinho da Silva Campos  | Atuei na implementação do frontend e na integração e criação da API, ajudei na lógica do programa e participei das reuniões com os outros integrantes para a implementação do projeto. | Boa  |
| Diógenes Dantas Lélis Júnior  |  |  |
| Bernardo Chaves Pissutti  | Ajudei a integrar o front com a lógica do projeto, para isso tive que criar as rotas para receber as requisições e executar as funções em ordem correta, usando a linguagem prolog. Pude realizar esse trabalho com o auxilio da biblioteca http que abstrai o tratamento de requisições http em prolog e facilita bastante o trabalho. | Boa |
| Abraão Alves  | Participação do grupo 2 (resolver tabuleiro) | Regular |
| Denniel William Roriz Lima  | Função de comparação, alguns tratamentos no para validação quando for um resultado verdadeiro ou falso, ajuste de bugs, documentação e outras tarefas dentro da subdivisão "Selecionar dificuldade e gerar tabuleiro" | Boa |
| Francisco Mizael Santos da Silva  |  |  |
| Mateus Caltabiano Neves Frauzino  | Axuxiliei o grupo da API a desenvolver os endpoints de /sudoku/start/, /sudoku/get-lives/, /sudoku/get-help/, alguns deles não puderam ser utilizados nesse momento, mas serão úteis nas possíveis melhorias do projeto. Também auxiliei na solução de um bug crítico que estáva deixando mais de um número igual no mesmo quadrante | Boa |
| Lucas Macedo Barboza  | Frontend da aplicação, ajuda nas endpointes da api, ajustes de bugs e integração | Boa |

## Outros 

### Organização de tarefas 
O grupo foi separado em quatro grupos menores para as atividades ficarem melhor organizadas. A divisão de tarefas entre cada subgrupo foi a seguinte:
* **Front end**: Criar o layout do projeto e consumir os endpoints para funcionamento feito pelo grupo 3;
* **Grupo 1**: Selecionar dificuldade e gerar tabuleiro;
* **Grupo 2** : Resolver tabuleiro e exibir dicas;
* **Grupo 3**: Api para integrar com o front;

### Lições aprendidas
   * Com esse trabalho foi possível aprender a criar aplicações conhecidas em uma linguagem nova, permitindo focar exclusivamente na aprendizagem do prolog e do paradigma lógico.
   * Aprendemos que para percorrer as colunas de uma matriz, em Prolog, é preciso fazer uma transposição na matriz, pois ele não consegue iterar de forma natural pelas colunas como faz pelas linhas. A iteração natural de Prolog é sobre listas de listas, onde cada lista representa uma linha da matriz. Para acessar as colunas, a matriz precisa ser transposta, o que transforma as colunas em linhas, permitindo a iteração natural de Prolog sobre essas listas.
   * Prolog não foi ideal para o tipo de aplicação que tivemos que desenvolver, apesar de ter alguams ferramentas úteis, a comunidade e os recursos disponíveis se provaram mais limitados em comparação com linguagens mais populares para desenvolvimento web.


### Percepções 
   * Dificuldade em se adaptar a linguagem e ao paradigma logico
   * A divisão em grupos muito dependentes uns dos outros dificultou na organização geral da equipe.

### Contribuições e fragilidades
   * O período de greve certamente prejudicou o ritmo e a organização pessoal de todos, se provando uma dificuldade extra para o trabalho.
   * A arquitetura do projeto foi desenvolvida com um front-end em HTML e uma API em Prolog. Usamos nossa API, que contém a lógica de negócios, para manipular as solicitações do front-end. No entanto, encontramos um problema com o front-end devido à utilização do CORS (Cross-Origin Resource Sharing). O CORS é um mecanismo utilizado pelos navegadores para compartilhar recursos entre diferentes origens, conforme especificado pelo W3C. Ele faz uso de cabeçalhos HTTP para informar aos navegadores se um determinado recurso pode ou não ser acessado. Devido a isso, a interface não consegue se comunicar com a API. Para focarmos nosso tempo em tarefas mais importantes, utilizamos o Postman para garantir que a API do sistema esteja funcional. 

### Trabalhos futuros
   * Implementar mecanicas de dicas e anotações
   * Implementar resolução automatica

## Fontes
<!-- Referencie, adequadamente, as referências utilizadas.
Indique ainda sobre fontes de leitura complementares. -->
1. EASYBRAIN. Regras do sudoku. 2024. Disponível em: https://sudoku.com/pt/regras-do-sudoku/. Acesso em: 17 jul. 2024.
2. SWI-PROLOG. SWI-Prolog documentation. Disponível em: https://www.swi-prolog.org/pldoc/index.html. Acesso em: 17 jul. 2024.
3. SWI-PROLOG. The HTTP server libraries. Disponível em: https://www.swi-prolog.org/pldoc/man?section=httpserver. Acesso em: 17 jul. 2024.
