:- dynamic element/4.


create_row(_, 0, _, _) :- !.
create_row(MatrixId, Col, Elem, Row) :-
    asserta(element(MatrixId, Row, Col, Elem)),
    Col1 is Col - 1,
    create_row(MatrixId, Col1, Elem, Row).


create_matrix(_, 0, _, _) :- !.
create_matrix(MatrixId, Rows, Elem, TotalCols) :-
    create_row(MatrixId, TotalCols, Elem, Rows),
    Rows1 is Rows - 1,
    create_matrix(MatrixId, Rows1, Elem, TotalCols).

element_at(MatrixId, Row, Col, Value) :-
    element(MatrixId, Row, Col, Value).

insert_element(MatrixId, Row, Col, Value) :-
    (   retract(element(MatrixId, Row, Col, _))
    ->  true
    ;   true
    ),
    asserta(element(MatrixId, Row, Col, Value)).

print_matrix(MatrixId, Rows, Cols) :-
    print_rows(MatrixId, 1, Rows, Cols).

print_rows(_, Row, Rows, _) :- Row > Rows, !.
print_rows(MatrixId, Row, Rows, Cols) :-
    print_cols(MatrixId, Row, 1, Cols),
    nl,
    Row1 is Row + 1,
    print_rows(MatrixId, Row1, Rows, Cols).

print_cols(_, _, Col, Cols) :- Col > Cols, !.
print_cols(MatrixId, Row, Col, Cols) :-
    ( element(MatrixId, Row, Col, Value) -> write(Value) ; write(0) ),  % Assume 0 se o elemento não estiver definido
    write(' '),
    Col1 is Col + 1,
    print_cols(MatrixId, Row, Col1, Cols).

matrix_to_list(MatrixId, Rows, Cols, MatrixList) :-
    matrix_to_list(MatrixId, Rows, Cols, 1, MatrixList).

matrix_to_list(_, Rows, _, Row, []) :- Row > Rows, !.
matrix_to_list(MatrixId, Rows, Cols, Row, [RowList|Rest]) :-
    row_to_list(MatrixId, Row, Cols, 1, RowList),
    Row1 is Row + 1,
    matrix_to_list(MatrixId, Rows, Cols, Row1, Rest).

row_to_list(_, _, Col, Col, []) :- !.
row_to_list(MatrixId, Row, Cols, Col, [Value|Rest]) :-
    ( element(MatrixId, Row, Col, Value) -> true ; Value = 0 ),  % Assume 0 se o elemento não estiver definido
    Col1 is Col + 1,
    row_to_list(MatrixId, Row, Cols, Col1, Rest).

element_exists(Element, List) :-
    member(Element, List).

verify_in_row(Element, MatrixId, Row) :-
    matrix_to_list(MatrixId,9,9,Matrix),
    nth1(Row, Matrix, MatrixRow),
    element_exists(Element, MatrixRow).

verify_in_column(Element, MatrixId, Col) :-
    matrix_to_list(MatrixId,9,9,Matrix),
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

verify_in_quadrant(MatrixId, X, Y, Element) :-
    matrix_to_list(MatrixId,9,9,Matrix),
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

element_found(Element, MatrixId, Row, Col) :-
(   verify_in_row(Element, MatrixId, Row)
;   verify_in_column(Element, MatrixId, Col)
;   verify_in_quadrant(MatrixId, Row, Col, Element)
).

compare_rows([], []).
compare_rows([H1|T1], [H2|T2]) :-
    H1 =:= H2,
    compare_rows(T1, T2).

compare_matrices([], []).
compare_matrices([Row1|Matrix1], [Row2|Matrix2]) :-
    compare_rows(Row1, Row2),
    compare_matrices(Matrix1, Matrix2).

compare_9x9_matrices(Matrix1Id, Matrix2Id) :-
    matrix_to_list(Matrix1Id,9,9,Matrix1),
    matrix_to_list(Matrix2Id,9,9,Matrix2), 
    length(Matrix1, 9),
    length(Matrix2, 9),
    maplist(length_(9), Matrix1),
    maplist(length_(9), Matrix2),
    compare_matrices(Matrix1, Matrix2).

length_(L, List) :- length(List, L).

random_number(RandomNumber) :-
    random_between(1, 8, RandomNumber).

shuffle_list(List) :-
    findall(X, between(1, 9, X), OriginalList),
    random_permutation(OriginalList, List).

has_negative_one(MatrixId, Rows, Cols, Found) :-
    (   between(1, Rows, Row),
        between(1, Cols, Col),
        element(MatrixId, Row, Col, Element),
        Element =:= -1
    ->
        Found = true
    ;
        Found = false
    ).

fill_matrix(MatrixId):-
    fill_matrix_row(MatrixId,1),
    has_negative_one(MatrixId, 9, 9,Found),  
    ( Found 
        ->
            fill_matrix(MatrixId)
    ).


fill_matrix_row(MatrixId,10) :-
    fill_matrix_column(MatrixId, 10, 1).
fill_matrix_row(MatrixId, Row) :-
    fill_matrix_column(MatrixId, Row, 1),
    NextRow is Row + 1,
    fill_matrix_row(MatrixId, NextRow).

fill_matrix_column(MatrixId, Row, 10) :-
    random_new_number(MatrixId,Row, 10, RandomNumber),
    insert_element(MatrixId, Row, 10, RandomNumber). 

fill_matrix_column(MatrixId,Row, Col) :-
    random_new_number(MatrixId,Row, Col, RandomNumber),
    insert_element(MatrixId, Row, Col, RandomNumber),
    NextCol is Col + 1,
    fill_matrix_column(MatrixId, Row, NextCol).

copy_value(Var1, Var2) :-
    Var2 = Var1.

increment(X, Y) :-
    Y is X + 1.

random_new_number(MatrixId,Row, Col, Times, 9, RandomNumber) :-
    copy_value(1,Number),
    increment(Times,NewTimes),
    (   element_found(Number, MatrixId, Row, Col),
        NewTimes =< 10
        ->
            random_new_number(MatrixId, Row, Col, NewTimes, Number, RandomNumber)
        ;
            NewTimes > 10
            ->
                RandomNumber = -1
            ;
                copy_value(Number, RandomNumber)
    ).

random_new_number(MatrixId,Row, Col, Times, PreviousNumber, RandomNumber) :-
    increment(PreviousNumber,Number),
    increment(Times,NewTimes),
    (   element_found(Number, MatrixId, Row, Col),
        NewTimes =< 10
        ->
            random_new_number(MatrixId, Row, Col, NewTimes, Number, RandomNumber)
        ;
            NewTimes > 10
            ->
                RandomNumber = -1
            ;
                copy_value(Number, RandomNumber)
    ).

random_new_number(MatrixId,Row, Col, RandomNumber) :-
    random_number(Number),
    ( element_found(Number, MatrixId, Row, Col)
    -> random_new_number(MatrixId, Row, Col, 1, Number, RandomNumber)
    ;  copy_value(Number,RandomNumber)
    ).

:- initialization(main).

main :-
    create_matrix(1, 9, 0, 9),
    create_matrix(2, 9, 0, 9),

    fill_matrix(1),

    write("Matriz 1: "),nl,print_matrix(1, 9, 9),
    write("Matriz 2: "),nl,print_matrix(2, 9, 9),

    (verify_in_row(0, 1, 2) -> format('Elemento 0 encontrado na linha 2~n') ; format('Elemento 0 não encontrado na linha 2~n')),

    (verify_in_column(0, 1, 3) -> format('Elemento 0 encontrado na coluna 3~n') ; format('Elemento 0 não encontrado na coluna 3~n')),

    (verify_in_quadrant(1, 2, 3, 0) -> format('Elemento 0 encontrado no quadrante 3x3 contendo (2, 3)~n') ; format('Elemento 0 não encontrado no quadrante 3x3 contendo (2, 3)~n')),
    
    ( element_found(0, 1, 2, 3)
    -> format('Elemento 0 encontrado na linha 2, coluna 3 ou no quadrante 3x3 contendo (2, 3)~n')
    ;  format('Elemento 0 não encontrado na linha 2, coluna 3 ou no quadrante 3x3 contendo (2, 3)~n')
    ),

    (compare_9x9_matrices(1, 2) ->  format('As matrizes são iguais.~n') ; format('As matrizes são diferentes.~n')),

    % random_number(RandomNumber),
    % write('Número aleatório entre 1 e 9: '), write(RandomNumber), nl,

    halt.
