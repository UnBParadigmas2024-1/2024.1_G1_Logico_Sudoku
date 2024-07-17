:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).

:- use_module(main).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

:- http_handler(root('sudoku/start'), start_sudoku_handler, [method(get)]).
:- http_handler(root('sudoku/solve'), solve_sudoku_handler, [method(post)]).
:- http_handler(root('sudoku/get-lives'), get_lives_handler, [method(get)]).
:- http_handler(root('sudoku/get-help'), get_help_handler, [method(get)]).


start_sudoku_handler(_Request) :-
    main:init_matrix(Matrix1),
    main:fill_matrix(Matrix1, Matrix2),
    reply_json_dict(_{puzzle: Matrix2}).

solve_sudoku_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    ( main:compare_9x9_matrices(DictIn.puzzle, Solution) -> 
        Result = true
    ;
        Result = false
    ),
    reply_json_dict(_{solution: Result}).

% get_lives_handler(_Request) :-
    % main:get_lives(Lives),
    % reply_json_dict(_{vidas: Lives}).

% get_help_handler(_Request) :- 
%     main:get_help(Help),
%     reply_json_dict(_{mensagem: Help})

:- initialization(server(8080)).