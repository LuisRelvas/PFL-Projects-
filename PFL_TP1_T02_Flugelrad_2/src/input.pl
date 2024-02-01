:- dynamic set_list/2.

% Choose Bot difficulty
% It can be 1 or 2
% choose_difficulty(+Bot)
choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Greedy\n'),
    get_input(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).

% Main menu options.
% Each option calls a different predicate
% option(+N)
option(1):-
    write('Human vs. Human\n'),
    asserta((difficulty(player1, 3))),
    asserta((difficulty(player2, 3))),
    set_name(player1), set_name(player2).
option(2):-
    write('Human vs. Bot\n'),
    set_name(player1),
    asserta((name_of(player2, 'bot'))), !, 
    choose_difficulty(player2).
option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% Unifies player with the player who will start the game
% choose_player(-Player)
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a \n2 - ~a \n', [Name1, Name2]),
    get_input(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player),
    assert(set_list(Index,Player)).

% Reads an input often a number between Min and Max
% get_input(+Min,+Max,+Context,-Value)
get_input(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_input(Value),
    between(Min, Max, Value), !.

% Unifies Number with input number from console
% read_input(-Number)
read_input(X):-
    read_input_aux(X,0).
read_input_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_input_aux(X,Acc1).
read_input_aux(X,X). 

% Choose the Game Mode
% 1 - Human vs. Human
% 2 - Human vs. Bot
% 3 - Bot vs. Bot
% set_mode/0
set_mode :-
    menu,
    get_input(1, 3, 'Mode', Option), !,
    option(Option).

% Concatenates the Result with an input line up to endline '\n'
% read_string(-Result,+Acc)
read_string(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    read_string(Result, Acc1).
read_string(Result, Acc):-
    atom_chars(Result, Acc).


% Asks for the players name and saves it
% set_name(+Player)
set_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    read_string(Name, []),
    asserta(name_of(Player, Name)).

% predicade that receives a GameState and waits for the Input of the user to choose the number of the hexagon
% get_hexagon(+GameState,-Number,+ListOfMoves)
get_hexagon(GameState, Number,ListOfMoves) :-
    repeat,
    get_input(0,6,'Please choose the Number of the Hexagon you want to rotate',Number),
    \+ member(Number,ListOfMoves),
    repeat.
    
    
% predicade that decides what to do when the Game is over
% get_final(-Choose)
get_final(Choose) :- 
    write('Please select one of the following actions:\n'),
    write('0 - Exit Game\n'),
    write('1 - Play Again\n'),
    get_input(0,1,'Select',Choose), !.


% predicade that receives a GameState and waits for the Input of the user to choose the number of Times the hexagon will rotate
% get_number(+GameState,-Times)
get_number(GameState, Times) :-
    repeat,
    get_input(1,5,'Please choose how many times you want to rotate the Hexagon',Times).
