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

:- http_handler(root('sudoku/start'), start_sudoku_handler, [method(post)]).
:- http_handler(root('sudoku/move'), move_sudoku_handler, [method(post)]).
:- http_handler(root('sudoku/get-lives'), get_lives_handler, [method(get)]).
:- http_handler(root('sudoku/status'), get_status_handler, [method(get)]).
% :- http_handler(root('sudoku/solve'), solve_sudoku_handler, [method(post)]).
% :- http_handler(root('sudoku/get-help'), get_help_handler, [method(get)]).

start_sudoku_handler(Request) :-
    cors_enable,
    http_read_json_dict(Request, DictIn),
    nb_setval(level, easy),
    nb_getval(level, Level),
    main:init_matrix(1, Level),
    main:matrix_to_list(1, 9, 9, MatrixList),

    reply_json_dict(_{puzzle: MatrixList}).

move_sudoku_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    ( main:play_game(1,DictIn.row,DictIn.col,DictIn.number) 
    -> reply_json_dict(_{status: 'success'})
    ; reply_json_dict(_{status: 'failed'})
    ).


get_lives_handler(_Request) :-
     main:get_life(Life),
     reply_json_dict(_{vidas: Life}).


get_status_handler(_Request) :-
     main:game_status(Status),
     reply_json_dict(_{status: Status}).

% get_help_handler(_Request) :-
%     % Substitua pela sua lógica de ajuda
%     reply_json_dict(_{mensagem: "Aqui está a sua ajuda!"}).

% solve_sudoku_handler(Request) :-
%     % option(method(post), Request), !,
%     cors_enable(Request,
%                 [ methods([get,post,delete])
%                 ]),
%     http_read_json_dict(Request, DictIn),
%     nb_getval(sudoku_matrix, StoredMatrix),
%     ( main:compare_9x9_matrices(DictIn.puzzle, StoredMatrix)
%     ->  Result = true
%     ;   nb_getval(lives, Lives),
%         NewLives is Lives - 1,
%         nb_setval(lives, NewLives),
%         Result = false
%     ),
%     reply_json_dict(_{solution: Result, vidas: NewLives}).


:- initialization(server(8080)).
