
% LEMBRAR DE TIRAR DEPOIS
:- module(main, [solve_sudoku/2]).


create_row(N, Elem, Row) :-
    length(Row, N),
    maplist(=(Elem), Row).

create_matrix(N, Elem, Matrix) :-
    length(Matrix, N),
    maplist(create_row(N, Elem), Matrix).

element_at(Matrix, Row, Col, Element) :-
    nth1(Row, Matrix, MatrixRow),
    nth1(Col, MatrixRow, Element).

insert_element(Matrix, Row, Col, Element, NewMatrix) :-
    replace(Matrix, Row, Col, Element, NewMatrix).


replace([H|T], 1, Col, NewElem, [R|T]) :-
    replace_row(H, Col, NewElem, R).
replace([H|T], Row, Col, NewElem, [H|R]) :-
    Row > 1,
    Row1 is Row - 1,
    replace(T, Row1, Col, NewElem, R).


replace_row([_|T], 1, X, [X|T]).
replace_row([H|T], Col, X, [H|R]) :-
    Col > 1,
    Col1 is Col - 1,
    replace_row(T, Col1, X, R).

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

element_not_found(Element, Matrix, Row, Col) :-
    \+ (verify_in_row(Element, Matrix, Row)),
    \+ (verify_in_column(Element, Matrix, Col)),
    \+ (verify_in_quadrant(Matrix, Row, Col, Element)).

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

random_number(RandomNumber) :-
    random_between(1, 9, RandomNumber).

shuffle_list(List) :-
    findall(X, between(1, 9, X), OriginalList),
    random_permutation(OriginalList, List).


fill_matrix(Matrix, NewMatrix) :-
    fill_matrix_row(Matrix, 1, NewMatrix).

fill_matrix_row(Matrix, 10, NewMatrix) :-
    fill_matrix_column(Matrix, Row, 1, NewMatrix).
fill_matrix_row(Matrix, Row, NewMatrix) :-
    fill_matrix_column(Matrix, Row, 1, NewMatrix1),
    NextRow is Row + 1,
    fill_matrix_row(NewMatrix1, NextRow, NewMatrix).

fill_matrix_column(Matrix, Row, 10, NewMatrix) :-
    random_number(RandomNumber),
    insert_element(Matrix, Row, Col, RandomNumber, NewMatrix). 

fill_matrix_column(Matrix, Row, Col, NewMatrix) :-
    shuffle_list(ShuffledList),
    check_vector_elements(ShuffledList, Matrix, Row, Col, Resultado),
    random_number(RandomNumber),
    insert_element(Matrix, Row, Col, RandomNumber, NewMatrix1),
    NextCol is Col + 1,
    fill_matrix_column(NewMatrix1, Row, NextCol, NewMatrix).

check_vector_elements([], _, _, _, _).
check_vector_elements([Element|Rest], Matrix, Row, Col, Result) :-
    ( element_not_found(Element, Matrix, Row, Col) ->
        Result = Element  % Retorna o próprio elemento se não encontrado
    ;   check_vector_elements(Rest, Matrix, Row, Col, Result)
    ).

init_matrix(Matrix) :-
    create_matrix(9, 0, Matrix).

:- initialization(main).

main :-

    % Inicializa a matriz 9x9
    init_matrix(Matrix),
    format('Matriz 9x9 vazia:~n~w~n', [Matrix]),
    % Inicializa duas matrizes 9x9
    init_matrix(Matrix1),
    fill_matrix(Matrix1,Matrix2),

    format('Matriz 9x9 vazia:~n~w~n', [Matrix1]),
    format('Matriz 9x9 Preenchida:~n~w~n', [Matrix2]),

    % Acessa o elemento na posição (2, 3)
    element_at(Matrix2, 2, 3, Element),
    format('Elemento na posição (2, 3): ~w~n', [Element]),

    (verify_in_row(0, Matrix2, 2) -> format('Elemento 0 encontrado na linha 2~n') ; format('Elemento 0 não encontrado na linha 2~n')),

    (verify_in_column(0, Matrix2, 3) -> format('Elemento 0 encontrado na coluna 3~n') ; format('Elemento 0 não encontrado na coluna 3~n')),

    (verify_in_quadrant(Matrix2, 2, 3, 0) -> format('Elemento 0 encontrado no quadrante 3x3 contendo (2, 3)~n') ; format('Elemento 0 não encontrado no quadrante 3x3 contendo (2, 3)~n')),
    
    (compare_9x9_matrices(Matrix1, Matrix2) ->  format('As matrizes são iguais.~n') ; format('As matrizes são diferentes.~n')),

    random_number(RandomNumber),
    write('Número aleatório entre 1 e 9: '), write(RandomNumber), nl,

    halt.
