% LEMBRAR DE TIRAR DEPOIS
:- module(main, [solve_sudoku/2]).

% Função de teste - LEMBRAR DE TIRAR DEPOIS
solve_sudoku(Board, Solution) :-
    write('PASSEI AQUI'), nl,
    Solution = Board.

create_row(N, Elem, Row) :-
    length(Row, N),
    maplist(=(Elem), Row).

create_matrix(N, Elem, Matrix) :-
    length(Matrix, N),
    maplist(create_row(N, Elem), Matrix).

init_matrix(Matrix) :-
    create_matrix(9, 0, Matrix).

element_at(Matrix, Row, Col, Element) :-
    nth1(Row, Matrix, MatrixRow),
    nth1(Col, MatrixRow, Element).

# :- initialization(main).

# main :-
#     % Função de teste - LEMBRAR DE TIRAR DEPOIS
#     solve_sudoku(Board, Solution) :-
#         Solution = Board.

#     % Inicializa a matriz 9x9
#     init_matrix(Matrix),
#     format('Matriz 9x9 vazia:~n~w~n', [Matrix]),

#     % Acessa o elemento na posição (2, 3)
#     element_at(Matrix, 2, 3, Element),
#     format('Elemento na posição (2, 3): ~w~n', [Element]),

#     halt.
