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

compare_rows([], []).
compare_rows([H1|T1], [H2|T2]) :-
    H1 =:= H2,
    compare_rows(T1, T2).

compare_matrices([], []).
compare_matrices([Row1|Matrix1], [Row2|Matrix2]) :-
    compare_rows(Row1, Row2),
    compare_matrices(Matrix1, Matrix2).

compare_9x9_matrices(Matrix1, Matrix2) :- 
    length(Matrix1, 9),
    length(Matrix2, 9),
    maplist(length_(9), Matrix1),
    maplist(length_(9), Matrix2),
    compare_matrices(Matrix1, Matrix2).

length_(L, List) :- length(List, L).

:- initialization(main).

main :-
    % Inicializa duas matrizes 9x9
    init_matrix(Matrix1),
    init_matrix(Matrix2),

    % Inicializa a matriz 9x9
    init_matrix(Matrix),
    format('Matriz 9x9 vazia:~n~w~n', [Matrix]),

    % Acessa o elemento na posição (2, 3)
    element_at(Matrix, 2, 3, Element),
    format('Elemento na posição (2, 3): ~w~n', [Element]),

    % Compara duas matrizes
    (   compare_9x9_matrices(Matrix1, Matrix2)
    ->  format('As matrizes são iguais.~n')
    ;   format('As matrizes são diferentes.~n')),

    halt.
