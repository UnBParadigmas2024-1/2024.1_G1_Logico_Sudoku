create_row(N, Elem, Row) :-
    length(Row, N),
    maplist(=(Elem), Row).

create_matrix(N, Elem, Matrix) :-
    length(Matrix, N),
    maplist(create_row(N, Elem), Matrix).

init_matrix(Matrix) :-
    create_matrix(9, 2, Matrix).

element_at(Matrix, Row, Col, Element) :-
    nth1(Row, Matrix, MatrixRow),
    nth1(Col, MatrixRow, Element).

element_exists(Element, List) :-
    member(Element, List).

verify_in_row(Element, Matrix, Row) :-
    nth1(Row, Matrix, MatrixRow),
    element_exists(Element, MatrixRow).

verify_in_column(Element, Matrix, Col) :-
    transpose(Matrix, TransposedMatrix),
    nth1(Col, TransposedMatrix, Column),
    element_exists(Element, Column).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

verify_in_quadrant(Matrix, X, Y, Element) :-
    QX is ((X - 1) // 3) * 3 + 1,
    QY is ((Y - 1) // 3) * 3 + 1,
    extract_quadrant(Matrix, QX, QY, Quadrant),
    element_exists(Element, Quadrant).

extract_quadrant(Matrix, QX, QY, Quadrant) :-
    QX2 is QX + 2,
    QY2 is QY + 2,
    findall(Element, (
        between(QX, QX2, Row),
        between(QY, QY2, Col),
        element_at(Matrix, Row, Col, Element)
    ), Quadrant).
    
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

    (verify_in_row(0, Matrix, 2) -> format('Elemento 0 encontrado na linha 2~n') ; format('Elemento 0 não encontrado na linha 2~n')),

    (verify_in_column(0, Matrix, 3) -> format('Elemento 0 encontrado na coluna 3~n') ; format('Elemento 0 não encontrado na coluna 3~n')),

    (verify_in_quadrant(Matrix, 2, 3, 0) -> format('Elemento 0 encontrado no quadrante 3x3 contendo (2, 3)~n') ; format('Elemento 0 não encontrado no quadrante 3x3 contendo (2, 3)~n')),
    
    (compare_9x9_matrices(Matrix1, Matrix2) ->  format('As matrizes são iguais.~n') ; format('As matrizes são diferentes.~n')),

    halt.
