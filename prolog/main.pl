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


:- initialization(main).

main :-
    % Inicializa a matriz 9x9
    init_matrix(Matrix),
    format('Matriz 9x9 vazia:~n~w~n', [Matrix]),

    % Acessa o elemento na posição (2, 3)
    element_at(Matrix, 2, 3, Element),
    format('Elemento na posição (2, 3): ~w~n', [Element]),

    (verify_in_row(0, Matrix, 2) -> format('Elemento 0 encontrado na linha 2~n') ; format('Elemento 0 não encontrado na linha 2~n')),

   

    halt.
