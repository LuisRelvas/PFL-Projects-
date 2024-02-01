:- consult(utils).
:- consult(input).

% flugelrad/0
% Game header
flugelrad :-
    write('=====================\n'),
    write('Welcome to Flugelrad!\n'),
    write('=====================\n').

 
% menu/0
% Main menu
menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n').


% Initialize GameState with Board, ChangeBoard and Player who will start the game
% configuration(-GameState)
configurations([Board,ChangeBoard,Player]):-
    flugelrad,
    set_mode,
    choose_player(Player),
    initial_random_state,
    initial_state(Board,ChangeBoard).

% Initialize Board and ChangeBoard
% initial_state(-Board,-ChangeBoard)
initial_state(Board,ChangeBoard):-
    board(I,Board),
    check(U,ChangeBoard).

% Initialize a random state
initial_random_state :-
    now(X),
    setrand(X).

% change the player who is playing
% change_player(+Player1,-Player2)
change_player(player1, player2).
change_player(player2, player1).

% Reset the game
% Attributing the initial state to the board and changeboard
% reset_game/0
reset_game :-
retractall(board(_,_)),
        assertz(board(I,[
            [1,2,3,4,5,6],  % A 
            [7,8,9,10,11,3], % B 
            [12,5,13,14,15,16], % C
            [4,11,17,18,19,13], % D
            [10,20,21,22,23,17], % E 
            [14,19,24,25,26,27], % F
            [18,23,28,29,30,24] % G 
        ])),
    retractall(check(_,_)),
    retractall(set_list(_,_)),
    assertz(check(U,[
    [0,0,0,1,-,2,0,7,-,8,0,0,0],
    [0,0,6,0,0,0,3,0,0,0,9,0,0],
    [0,12,-,5,-,4,-,11,-,10,-,20,0],
    [16,0,0,0,13,0,0,0,17,0,0,0,21],
    [0,15,-,14,-,19,-,18,-,23,-,22,0],
    [0,0,27,0,0,0,24,0,0,0,28,0,0],
    [0,0,0,26,-,25,0,30,-,29,0,0,0]
])),clear_data,clear_console,
    play.


