% LEMBRAR DE TIRAR DEPOIS
:- module(main, [solve_sudoku/2]).

% Função de teste - LEMBRAR DE TIRAR DEPOIS
solve_sudoku(Board, Solution) :-
    write('PASSEI AQUI'), nl,
    Solution = Board.
