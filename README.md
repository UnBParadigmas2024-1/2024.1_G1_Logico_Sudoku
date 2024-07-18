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
O projeto proposto visa implementar um jogo de sudoku que gera um tabuleiro aleatoriamente com a quantidade de células preenchidas de acordo com a dificuldade selecionada. A ideia inicial era de apresentamos um sistema de vidas e a possibilidade de selecionar entre três diferentes dificuldades, que influenciam na quantidade de células preenchidas ao inicializar o jogo.

## Screenshots
<!-- Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento. -->
1. Requisição de início de jogo

![image](https://github.com/user-attachments/assets/89961492-039d-41e9-9acd-27caf6234dba)


2. Requisição de alteração do valor de célula

![image](https://github.com/user-attachments/assets/1427ce73-3e71-4fbf-a7e4-6658c86815a7)


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
docker-compose up
```

Obs: Para linux é necessário utilizar o comendo sudo antes

## Uso 
1. Após iniciar o projeto, clique no botão `Começar` e as células do tabuleiro serão preenchidas.
   ![image](https://github.com/user-attachments/assets/11f41590-9bc4-4c7d-833b-fbb2a067007e)

2. Para adicionar números às celulas, basta clicar na célula desejada e digitar um número. Caso não conheça, as regras básicas do sudoku podem ser encontradas aqui: [Regras básicas do Sudoku](https://sudoku.com/pt/regras-do-sudoku/)
3. O botão `Resolver passo` adiciona um número correto no tabuleiro.
4. O botão `Resolver tudo` finaliza o jogo e verifica se sua resposta para o tabuleiro gerado está correta.
5. Por fim, para reiniciar, basta apertar em `Limpar tabuleiro` e `Começar` novamente.

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) |
| -- | -- | -- |
| Paulo Henrique  | Trabalhei no subgrupo "Resolver tabuleiro e exibir dicas", desta forma criei as funções responsáveis por fazer validação dos números que não podem se repertir para a coluna, linha e quadrante. Como o prolog faz a varredura de forma horizontal no vetor da matriz, tive que criar a função de transposição da matriz, para que programa conseguisse ler os elementos da coluna. | Boa |
| Paulo Vitor Silva Abi Acl  | No geral atuei no subgrupo "Selecionar dificuldade e gerar tabuleiro", mais especificamente na parte de criar o tabuleiro pronto para jogo e também na parte da seleção de dificuldade | Boa |
| Leonardo de Souza Takehana  |  |  |
| Davi Marinho da Silva Campos  |  |  |
| Diógenes Dantas Lélis Júnior  |  |  |
| Bernardo Chaves Pissutti  |  |  |
| Abraão Alves  |  |  |
| Denniel William Roriz Lima  | Função de comparação, alguns tratamentos no para validação quando for um resultado verdadeiro ou falso, ajuste de bugs, documentação e outras tarefas dentro da subdivisão "Selecionar dificuldade e gerar tabuleiro" | Boa |
| Francisco Mizael Santos da Silva  |  |  |
| Mateus Caltabiano Neves Frauzino  | Participação no grupo 3 (API) e documentação | Regular |
| Lucas Macedo Barboza  | Frontend da aplicação, ajuda nas endpointes da api, ajustes de bugs e integração | Boa |

## Outros 

### Organização de tarefas 
O grupo foi separado em quatro grupos menores para as atividades ficarem melhor organizadas. A divisão de tarefas entre cada subgrupo foi a seguinte:
* **Front end**: Criar o layout do projeto e consumir os endpoints para funcionamento feito pelo grupo 3;
* **Grupo 1**: Selecionar dificuldade e gerar tabuleiro;
* **Grupo 2** : Resolver tabuleiro e exibir dicas;
* **Grupo 3**: Api para integrar com o front;

### Lições aprendidas
   * 

### Percepções 
   * Dificuldade em se adaptar a linguagem e ao paradigma logico
   * A divisão em grupos muito dependentes uns dos outros dificultou na organização geral da equipe.

### Contribuições e fragilidades
   * O período de greve certamente prejudicou o ritmo e a organização pessoal de todos, se provando uma dificuldade extra para o trabalho.
   * A arquitetura do projeto foi desenvolvida com um front-end em HTML e uma API em Prolog. Usamos nossa API, que contém a lógica de negócios, para manipular as solicitações do front-end. No entanto, encontramos um problema com o front-end devido à utilização do CORS (Cross-Origin Resource Sharing). O CORS é um mecanismo utilizado pelos navegadores para compartilhar recursos entre diferentes origens, conforme especificado pelo W3C. Ele faz uso de cabeçalhos HTTP para informar aos navegadores se um determinado recurso pode ou não ser acessado. Devido a isso, a interface não consegue se comunicar com a API. Para focarmos nosso tempo em tarefas mais importantes, utilizamos o Postman para garantir que a API do sistema esteja funcional. 

### Trabalhos futuros
   * Possibilitar o início de novo jogo sem necessidade de rebuildar a aplicação.
   * Resolver os problemas do CORS.
   * 


## Fontes
<!-- Referencie, adequadamente, as referências utilizadas.
Indique ainda sobre fontes de leitura complementares. -->
1. EASYBRAIN. Regras do sudoku. 2024. Disponível em: https://sudoku.com/pt/regras-do-sudoku/. Acesso em: 17 jul. 2024.
