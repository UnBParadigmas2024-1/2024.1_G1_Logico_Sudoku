:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_cors)).
:- use_module(library(nb_set)).

:- use_module(main).

:- set_setting_default(http:cors, [*]).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root('sudoku/start'), start_sudoku_handler, [method(get)]).
:- http_handler(root('sudoku/solve'), solve_sudoku_handler, [method(post)]).
:- http_handler(root('sudoku/get-lives'), get_lives_handler, [method(get)]).
:- http_handler(root('sudoku/get-help'), get_help_handler, [method(get)]).

start_sudoku_handler(_Request) :-
    cors_enable,
    main:init_matrix(Matrix1),
    main:fill_matrix(Matrix1, Matrix2),
    nb_setval(sudoku_matrix, Matrix2),
    nb_setval(lives, 3),
    reply_json_dict(_{puzzle: Matrix2}).

solve_sudoku_handler(Request) :-
    % option(method(post), Request), !,
    cors_enable(Request,
                [ methods([get,post,delete])
                ]),
    http_read_json_dict(Request, DictIn),
    nb_getval(sudoku_matrix, StoredMatrix),
    ( main:compare_9x9_matrices(DictIn.puzzle, StoredMatrix)
    ->  Result = true
    ;   nb_getval(lives, Lives),
        NewLives is Lives - 1,
        nb_setval(lives, NewLives),
        Result = false
    ),
    reply_json_dict(_{solution: Result, vidas: NewLives}).

get_lives_handler(_Request) :-
    nb_getval(lives, Lives),
    reply_json_dict(_{vidas: Lives}).

% get_help_handler(_Request) :-
%     % Substitua pela sua lógica de ajuda
%     reply_json_dict(_{mensagem: "Aqui está a sua ajuda!"}).

:- initialization(server(8080)).
