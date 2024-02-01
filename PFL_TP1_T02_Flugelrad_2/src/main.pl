:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).
:- consult(input).
:- consult(configurations).
:- consult(utils).
:- dynamic final_pos/1.
:- dynamic final_times/1.
:- dynamic hexagon/2.
:- dynamic board/2.
:- dynamic check/2.
:- dynamic connected/2.
:- dynamic set_list/2.


% define balls positions initial

pos(V,[[1,a],[2,v],[3,e],[4,e],[5,e],[6,v],[7,a],[8,v],[9,a],[10,e],[11,e],[12,a],[13,e],[14,e],[15,a],[16,v],[17,e],[18,e],[19,e],[20,v],[21,a],[22,v],[23,e],[24,e],[25,v],[26,a],[27,v],[28,a],[29,v],[30,a]]).


% Your original matrix.
board(I,[
    [1,2,3,4,5,6],  % A 
    [7,8,9,10,11,3], % B 
    [12,5,13,14,15,16], % C
    [4,11,17,18,19,13], % D
    [10,20,21,22,23,17], % E 
    [14,19,24,25,26,27], % F
    [18,23,28,29,30,24] % G 
]).


% My original matrix but in the Users Perspective
check(U,[
    [0,0,0,1,-,2,0,7,-,8,0,0,0],
    [0,0,6,0,0,0,3,0,0,0,9,0,0],
    [0,12,-,5,-,4,-,11,-,10,-,20,0],
    [16,0,0,0,13,0,0,0,17,0,0,0,21],
    [0,15,-,14,-,19,-,18,-,23,-,22,0],
    [0,0,27,0,0,0,24,0,0,0,28,0,0],
    [0,0,0,26,-,25,0,30,-,29,0,0,0]
]).


% Game Loop when a user wins the game
% game_cycle(+GameState)
game_cycle(GameState):-
    game_over(GameState,Winner),
    get_final(Choose),
    ((Choose == 0 -> 
        clear_data,
        halt
    );
    reset_game).

% Game Loop when the game is not over
% game_cycle(+GameState)
game_cycle(GameState):-
    pos(V,K),
    set_list(Index,First),
    display_Player(GameState,Player),nl,
    retractall(connected(_,_)),
    valid_moves(GameState,ListOfMoves),
    display_Not_Allowed(GameState,ListOfMoves),
    choose_move(GameState,Number,Times,ListOfMoves),
    nth0(0,GameState,Board1),
    nth0(Number,Board1,T),
    common_elements(GameState, T, CommonPairs),
    hexagon_update(GameState),
    rotate_hexagon(Number,Times,RotatedList,GameState),
    move(T,GameState,CommonPairs,RotatedList,UpdatedBoard),
    NewGameState = [UpdatedBoard,ChangeBoard,Player],
    display_Board(NewGameState,NewBoard,0),
    NewGameState1 = [UpdatedBoard,NewBoard,Player],
    separate_lists_by_value(K,Alists,Vlists,_),
    separate_lists_by_value(K,Alists1,Vlists1,_),
    display_Balls(NewGameState1,Alists,Vlists),
    (((Player == First) ->
    (iterate_common(NewGameState1,Alists,Alists1,0)));
    iterate_common(NewGameState1,Vlists,Vlists1,0)),
    iterate_connected(NewGameState1),
    change_player(Player,NextPlayer),
    NewGameState2 = [UpdatedBoard,NewBoard,NextPlayer],
    game_cycle(NewGameState2).

% predicade to display the Balls attributed to each player
% display_Balls(+GameState,+Alists,+Vlists)
display_Balls([_,_,Player],Alists,Vlists) :-
    set_list(Index,First),
    change_player(First,Other),
    name_of(Other,Name1),
    name_of(First, Name),
    format('~a\'s balls: ~w', [Name,Alists]),nl,
    format('~a\'s balls: ~w', [Name1,Vlists]),nl.


% predicade to display the winner of the game
% To check If we have a winner we check the predicade winner/2
% winner/2 is a dynamic predicade that will be asserted when a player wins the game
% game_over(+GameState,-Winner)
game_over([_,_,Other],Winner) :-
    change_player(Other,Winner),
    ((winner(Winner,1) ; winner(Winner,1)) -> 
        name_of(Winner, Name),
        write(' _________________________________'),nl,
        write('                                   '),nl,
        write('             YOU WON               '),nl,
        write('                                   '),nl,
        write('                                   '),nl,
        write('                                   '),nl,
        format('      ~a won the game!         \n ', [Name]),
        write('                                   '),nl,
        write('                                   '),nl,
        write('                                   '),nl,
        write('                                   '),nl,
        write(' _________________________________'),nl
        ). 



% predicade that ,accordingly to the type of player, chooses the number of the hexagon and the number of Times it will rotate :
% If the type of player = Human then it will ask the user to choose the number of the hexagon and the number of Times it will rotate;
% If the type of player = Bot Random then it will choose a random number between 1 and 6 For the hexagon 
% and a random number between 1 and 6 for the number of Times it will rotate ;
% If the type of player = Bot Greedy then it will choose the best move possible for the bot to win the game
% using the value predicade.
% choose_move(+GameState,-Number,-Times,+ListOfMoves)
choose_move(GameState,Number,Times,ListOfMoves) :-
    nth0(2,GameState,Player),
    ((difficulty(Player,1) -> (random_betweenN(1,6,ListOfMoves,Number),random_betweenT(1,6,Times));
    (difficulty(Player,2) -> (value(GameState),final_pos(Number),final_times(Times),retractall(connected(_,_))))));
    get_hexagon(GameState,Number,ListOfMoves), get_number(GameState,Times).


% value(+GameState)
value(GameState) :-
    value(GameState, 0, 1,0,PosFinal,TimesFinal).

% predicade For the Bot Greedy to choose the best possible move in order to Connect the most consecutive balls possible
% the procedure is the same as gamecycle but note that in some functions he doesnt complete all the steps For example,
% in the predicade display_Board he doesnt Do the final step that is Display the Board because it would be a waste of Time and Resources
% So he only does the steps that are necessary For him to choose the best move possible
% value(+GameState,+Pos,+Times,+Size,+PosFinal,+TimesFinal)
value(_, 7, _,_,_,_) :- !. % Stop when Pos reaches 7.
value(GameState, Pos, Times,Size,PosFinal,TimesFinal) :-
    pos(V,K),
    set_list(Index,First),
    retractall(connected(_,_)),
    nth0(0,GameState,Board1),
    nth0(Pos,Board1,Tb),
    common_elements(GameState, Tb, CommonPairsB),
    hexagon_update(GameState),
    rotate_hexagon(Pos, Times, RotatedListB, GameState),
    move(T,GameState,CommonPairsB,RotatedListB,UpdatedBoard),
    BotState = [UpdatedBoard,ChangeBoard,_],
    display_Board(BotState,NewBoard,1),
    BotState2 = [UpdatedBoard,NewBoard,_],
    separate_lists_by_value(K,Alists,Vlists,_),
    separate_lists_by_value(K,Alists1,Vlists1,_),
    nth0(2,GameState,Player),
    ((First == Player ->(
    iterate_common(BotState2,Alists,Alists1,1)));
    (iterate_common(BotState2,Vlists,Vlists1,1))),
    get_all_nodes(L),
    length(L,N),
    ((N > Size -> 
    retractall(final_pos(_)),
    retractall(final_times(_)),
    Size1 is N, PosFinal1 is Pos, TimesFinal1 is Times,
    assert(final_pos(Pos)),
    assert(final_times(Times))
    );(Size1 is Size)),
    NextPos is Pos,
    NextTimes is Times + 1,
    (NextTimes > 5 -> (NextPos1 is Pos + 1, NextTimes1 is 1) ; (NextPos1 is Pos, NextTimes1 is NextTimes)),
    value(GameState, NextPos1, NextTimes1,Size1,PosFinal1,TimesFinal1).

% Base Case For the value predicade, it must stop When the Pos is equal to 7.
% Base case when Pos reaches 7.
value(_, 7, _,_,_,_).

% Clear the information so that we can start or restart the game
clear_data :-
    retractall(connected(_,_)),
    retractall(difficulty(_,_)),
    retractall(name_of(_,_)),
    retractall(winner(_,_)).

% Predicade that loads the configurations of the game
% After he loads the configurations he will show the Board and the Balls of each player before the game starts
% After that he will start the game cycle
% play/0
play :-
    pos(V,O),
    configurations(GameState),
    clean_winner,
    separate_lists_by_value(O,Alists,Vlists,_),
    display_Board(GameState,NewBoard,0),
    display_Balls(GameState,Alists,Vlists),
    game_cycle(GameState),
    clear_data.

% Auxiliary predicade to help check If there are any hexagons with all the elements empty
% check_empty(+Y,+Board,+Elists)
check_empty(Y, Board, Elists) :-
    nth0(Y, Board, Row),
    between(0, 5, X),
    nth0(X, Row, Value),
    \+ member(Value, Elists).


% predicade that receives a GameState and a List of Hexagons that are not allowed to move because they have all the elements empty
% and returns in ListOfMoves the Hexagons that are allowed to move
% valid_moves(+GameState,-ListOfMoves)
valid_moves([Board, _, Player], ListOfMoves) :-
    pos(V, K),
    separate_lists_by_value(K, Alists, Vlists, Elists),
    % Initialize an empty list to accumulate values of Y
    findall(Y, (
        between(0, 6, Y),
        \+ check_empty(Y, Board, Elists)
    ), ListOfMoves).


% 1st arg: list of node ids of the path from the initial node (Ni) to the current node (Na).
% The head of the list contains Na, so the list is in reverse order.
% 2nd arg: id of the destination node.
% 3rd arg: output argument with the ordered list of edge ids to go from Ni to Nf.
% dfs(+Path, +Nf, -Fs)
dfs([Nf|_], Nf, []).
dfs([Na|T], Nf, [F|Fs]) :-
    (connected(Na, Nb)),
     \+member(Nb, [Na|T]), % Used to avoid cycles (reaching a visited node)
    dfs([Nb, Na|T], Nf, Fs).


% find_connection(?Origin, ?Destination, -Path)
% Find a path between Origin and Destination nodes.
find_connection(Ni, Nf,Path) :-
    dfs([Ni], Nf, Fs),
    length(Fs, V),
    (V >= 5 ->
        display('Game Over'), nl,
        retractall(winner(_,_)),
        asserta(winner(Player,1))
    ).


% Used to get all the connected values and to start the iteration from a member of C
% iterate_connected(+GameState)
iterate_connected(Path) :-
    get_all_nodes(C),
    (length(C, N), N < 6 -> 
        display('Nothing to do'),nl
    );
    member(Ni,L),
    (find_connection(Ni, M,Path)->true;true).

% Given a Valor and a Fixo it will check depending on the Position in the NewBoard if the Fixo is connected to the Valor
% If they are connected we will assert to the predicade connected the connection between the two
% Also If we are outside the value predicade then we will display the connection
% check_iterate(+GameState,+Valor,+Fixo)
check_iterate([_,NewBoard,_], Valor, Fixo,Loop) :-
    nth0(Initial, NewBoard, Row),
    nth0(Pos, Row, Fixo), % Find the position of our Head in the List
    (
        (member(Valor, Row) ->
            Pos1 is Pos + 1,
            nth0(Pos1, Row, V1),
            ((V1 == '-' -> 
                Pos2 is Pos1 + 1, 
                (nth0(Pos2, Row, V2), V2 == Valor -> 
                    assertz(connected(Fixo, Valor)),
                    assertz(connected(Valor,Fixo)),
                    (((Loop == 0)->
                    display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                )
            )
            ),
            true
        );
        (Initial = 0 -> 
    Initial1 is Initial + 1,
    nth0(Initial1,NewBoard,Row1),
    (member(Valor,Row1)->
        ((
            (Pos = 3 ; Pos = 7) ->
                Pos2 is Pos - 1,
                nth0(Pos2,Row1,Valor1),
                (Valor1 = Valor -> 
                    assertz(connected(Fixo, Valor)),
                    assertz(connected(Valor,Fixo)),
                    (((Loop == 0)->
                    display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                )
        );
        (
            (Pos = 5 ; Pos = 9)-> 
                Pos2 is Pos + 1,
                nth0(Pos2,Row1,Valor1),
                (Valor1 = Valor -> 
                    assertz(connected(Fixo, Valor)),
                    assertz(connected(Valor,Fixo)),
                    (((Loop == 0)->
                    display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                )
        ))
    )
);
        (Initial = 1 -> 
            Initial1 is Initial + 1,
            nth0(Initial1,NewBoard,Row1),
            (member(Valor,Row1)->
                (((Pos = 2) ->
                    Pos2 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
                );
                (
                ((Pos = 6) ->
                    Pos2 is Pos - 1,
                    Pos3 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    nth0(Pos3,Row1,Valor2),
                    ((Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    );
                    (Valor2 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    ))
                );
                ((Pos = 10) ->
                    Pos2 is Pos -1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    ))
                )));
        (Initial = 2 ->
                Initial1 is Initial + 1,
                nth0(Initial1,NewBoard,Row1),
                (member(Valor,Row1)->
                    (((Pos = 1 ; Pos = 5; Pos = 9)->
                        Pos2 is Pos - 1,
                        nth0(Pos2,Row1,Valor1),
                        (Valor1 = Valor -> 
                            assertz(connected(Fixo, Valor)),
                            assertz(connected(Valor,Fixo)),
                            (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                        )
                    );
                    ((Pos = 3 ; Pos = 7; Pos = 11)->
                        Pos2 is Pos + 1,
                        nth0(Pos2,Row1,Valor1),
                        (Valor1 = Valor -> 
                            assertz(connected(Fixo, Valor)),
                            assertz(connected(Valor,Fixo)),
                            (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                        )
                    ))
                ),
                true
        );
        (Initial = 3 ->
            Initial1 is Initial + 1,
            nth0(Initial1,NewBoard,Row1),
            (member(Valor,Row1)->
                (((Pos = 0) ->
                    Pos2 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
                );
                ((Pos = 4; Pos = 8 )->
                    Pos2 is Pos - 1,
                    Pos3 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    nth0(Pos3,Row1,Valor2),
                    ((Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    );
                    (Valor2 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    ))
                )))
            );
        (Initial = 4 -> 
            Initial1 is Initial + 1,
            nth0(Initial1,NewBoard,Row1),
            (member(Valor,Row1)->
                (((Pos = 3; Pos = 7)->
                    Pos2 is Pos - 1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
                );
                ((Pos = 5; Pos = 9)->
                    Pos2 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
                ))
            ),
            true
        );
        (Initial = 5 ->
            Initial1 is Initial + 1,
            nth0(Initial1,NewBoard,Row1),
            (member(Valor,Row1)->
                (((Pos = 2) ->
                    Pos2 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
                );
                ((Pos = 6) ->
                    Pos2 is Pos - 1,
                    Pos3 is Pos + 1,
                    nth0(Pos2,Row1,Valor1),
                    nth0(Pos3,Row1,Valor2),
                    ((Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    );
                    (Valor2 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    ))
                );
                (
                (Pos = 10) ->
                    Pos2 is Pos -1,
                    nth0(Pos2,Row1,Valor1),
                    (Valor1 = Valor -> 
                        assertz(connected(Fixo, Valor)),
                        assertz(connected(Valor,Fixo)),
                        (((Loop == 0)->
                        display('Connected'),display(Fixo),display('-'),display(Valor),nl);true)
                    )
            )))))),
            true.

% predicade that receives a GameState(UpdatedBoard,NewBoard,Player), and two lists List and List1
% List1 is used to pin the Header of the Lists,
% List is used to compare the pinned Value with the rest of the list
% In Each iteration of (Pin,Val) we will check in the NewBoard if the Pin is connected to the Val
% using the auxiliary predicade check_iterate
% In our game List and List1 can be this pairs (Alists,Alists1) or (Vlists,Vlists1).
% iterate_common(+GameState,+List,+List1)
iterate_common(_,_,[],_).
iterate_common([_,NewBoard,_], List, [H | T],Loop) :-  
        nth0(Initial, List, Val),
        (check_iterate([_,NewBoard,_], Val, H,Loop)->true;true), % 1 valor
	    Initial1 is Initial + 1,
        nth0(Initial1, List , Val1),
        (check_iterate([_,NewBoard,_], Val1,H,Loop)->true;true), % 2 Valor
        Initial2 is Initial1 + 1,
        nth0(Initial2, List, Val2),
        (check_iterate([_,NewBoard,_], Val2,H,Loop)->true;true), % 3 Valor
        Initial3 is Initial2 + 1,
        nth0(Initial3, List, Val3),
        (check_iterate([_,NewBoard,_], Val3,H,Loop)->true;true), % 4 Valor
        Initial4 is Initial3 + 1,
        nth0(Initial4, List, Val4),
        (check_iterate([_,NewBoard,_], Val4,H,Loop)->true;true), % 5 Valor
        Initial5 is Initial4 + 1,
        nth0(Initial5, List, Val5),
        (check_iterate([_,NewBoard,_], Val5,H,Loop)->true;true), % 6 Valor
        Initial6 is Initial5 + 1,
        nth0(Initial6, List, Val6),
        (check_iterate([_,NewBoard,_], Val6,H,Loop)->true;true), % 7 Valor
        Initial7 is Initial6 + 1,
        nth0(Initial7, List, Val7),
        (check_iterate([_,NewBoard,_], Val7,H,Loop)->true;true), % 8 Valor
        Initial8 is Initial7 + 1,
        nth0(Initial8, List, Val8),
        (check_iterate([_,NewBoard,_], Val8,H,Loop)->true;true), % 9 Valor
        iterate_common([_,NewBoard,_], List,T,Loop).
        


% predicade to change the Value of ChangeBoard to the NewBoard
% It also displays the current state of the Game
% display_Board(+GameState,+NewBoard,+Loop)
display_Board([UpdatedBoard,ChangeBoard,_],NewBoard,Loop) :-

% Construction Hexagon0
nth0(0, UpdatedBoard, RowA1),
nth0(0, RowA1, ValueA1),
nth0(1, RowA1, ValueA2),
nth0(1,UpdatedBoard, RowA2),
nth0(0, RowA2, ValueA3),
nth0(1,RowA2,ValueA4),


% Construction Hexagon1
nth0(5,RowA1,ValueB1),
nth0(2,RowA1,ValueB2),
nth0(2,RowA2,ValueB3),


% Construction Hexagon2
nth0(2,UpdatedBoard,RowC1),
nth0(0,RowC1,ValueC1),
nth0(1,RowC1,ValueC2),
nth0(3,RowA1,ValueC3),
nth0(4,RowA2,ValueC4),
nth0(3,RowA2,ValueC5),
nth0(4,UpdatedBoard,RowC2),
nth0(1,RowC2,ValueC6),

% Construction Hexagon3
nth0(2, UpdatedBoard, Row),
nth0(5, Row, ValueD1),
nth0(2, Row, ValueD2),
nth0(3, UpdatedBoard, Row1),
nth0(2, Row1, ValueD3),
nth0(4,UpdatedBoard,Row2),
nth0(2,Row2,ValueD4),

% Construction Hexagon4
nth0(2,UpdatedBoard,RowE1),
nth0(4,RowE1,ValueE1),
nth0(3,RowE1,ValueE2),
nth0(3,UpdatedBoard,RowE2),
nth0(4,RowE2,ValueE3),
nth0(3,RowE2,ValueE4),
nth0(4,UpdatedBoard,RowE3),
nth0(4,RowE3,ValueE5),
nth0(3,RowE3,ValueE6),

% Construction Hexagon5
nth0(5,UpdatedBoard,RowF1),
nth0(5,RowF1,ValueF1),
nth0(2,RowF1,ValueF2),
nth0(6,UpdatedBoard,RowF2),
nth0(2,RowF2,ValueF3),

% Construction Hexagon6
nth0(4,RowF1,ValueG1),
nth0(3,RowF1,ValueG2),
nth0(4,RowF2,ValueG3),
nth0(3,RowF2,ValueG4),

% Construction of the NewBoard
NewBoard = [
        [0,0,0,ValueA1,-,ValueA2,0,ValueA3,-,ValueA4,0,0,0],
        [0,0,ValueB1,0,0,0,ValueB2,0,0,0,ValueB3,0,0],
        [0,ValueC1,-,ValueC2,-,ValueC3,-,ValueC4,-,ValueC5,-,ValueC6,0],
        [ValueD1,0,0,0,ValueD2,0,0,0,ValueD3,0,0,0,ValueD4],
        [0,ValueE1,-,ValueE2,-,ValueE3,-,ValueE4,-,ValueE5,-,ValueE6,0],
        [0,0,ValueF1,0,0,0,ValueF2,0,0,0,ValueF3,0,0],
        [0,0,0,ValueG1,-,ValueG2,0,ValueG3,-,ValueG4,0,0,0]
    ],

    % Replace ChangeBoard with NewBoard
    retractall(check(U,_)),  % Remove any existing ChangeBoard facts
    asserta(check(U,NewBoard)),  % Add NewBoard as a fact

% If we are in the value predicade where Loop == 1 then we dont want to display the board
% If we are in the game predicade where Loop == 0 then we want to display the board 
% Loop is used to define when we are inside or out of the value predicade.

(((Loop == 0)->

write(' ____________________________________________________________'), nl,
write('                                                              '), nl,
format('        ~w ----  ~w   ~w ---- ~w                              ~n', [ValueA1, ValueA2, ValueA3, ValueA4]),
write('        /          \\ /          \\                           '), nl,
format('      ~w           ~w            ~w                          ~n', [ValueB1, ValueB2, ValueB3]),
write('     /   \\        / \\          /  \\                        '), nl,
format('   ~w --- ~w --- ~w - ~w ---  ~w --- ~w                       ~n', [ValueC1, ValueC2, ValueC3, ValueC4, ValueC5, ValueC6]),
write('   /       \\    /       \\    /        \\                    '), nl,
format(' ~w          ~w            ~w           ~w                  ~n', [ValueD1, ValueD2, ValueD3, ValueD4]),
write('   \\       /     \\       /   \\        /                    '), nl,
format('   ~w --- ~w --- ~w --- ~w --- ~w --- ~w                    ~n', [ValueE1, ValueE2, ValueE3, ValueE4, ValueE5, ValueE6]),
write('          /         \\  /        \\                         '), nl,
format('        ~w           ~w           ~w                         ~n', [ValueF1, ValueF2, ValueF3]),
write('        \\          /  \\         /                           '), nl,
format('         ~w --- ~w     ~w --- ~w                             ~n', [ValueG1, ValueG2, ValueG3, ValueG4]),
write(' ______________________________________________________________'), nl

);true).



% Check on the others sublists the common elements with the first sublist and return the list of common elements
% common_elements(+Board,-Common,-CommonPairs)
common_elements([Board,_,_], Common, CommonPairs) :-
    % Loop over each sublist in the board matrix
    % I want to save the row in the CommonPairs if it has common elements with the Common list
    findall(Index-Sublist, (
        nth0(Index, Board, Row),
        nth0(_, Row, Sublist),
        % check if the row is equal to the Common list if so then skip 
        \+ Row = Common, 
        % Check if the sublist is in the common matrix and not equal to any sublist in the Common matrix
        member(Sublist, Common, _)
    ), CommonPairs).


% iterate(+GameState,+List,+RotatedList,-UpdatedBoard)
iterate([], _, [Board,_], Board,RotatedList,UpdatedBoard).
iterate([Key-Value | Rest], [Board,_,_], List, V,RotatedList,UpdatedBoard) :-
    % Find the row in the Board matrix where the Row list is equal to the Key list
    nth0(Key, Board, Row),
    % Find the index of the Value in the Row list
    nth0(Index, Row, Value),
    % Replace the element in Row at the specified Index with V
    replace_at(Index, Row, V, Row1),
    replace_at(Key, Board, Row1, Board1),
     (  Rest \= [] ->
        move(List, [Board1,_,_], Rest,RotatedList,UpdatedBoard)
    ; true
    ),
    ( Rest = [] ->  
    nth0(Pos_list,Board,List),
    replace_at(Pos_list,Board1,RotatedList,Board2),
    retractall(board(I,_)),
    assertz(board(I,Board2)),
    UpdatedBoard = Board2
    ; true).
   

% predicade to determine the initial position of a Value before the Rotation and to calculate which value is in that Position after the Rotation
% It also updates the Board with the new Value -> UpdatedBoard
% This predicade is used to make as the name says the move of the Hexagon
% move(+List,+GameState,+Rest,+RotatedList,-UpdatedBoard)
move(List, [Board,_,_], [Key-Value | Rest],RotatedList,UpdatedBoard) :-
    nth0(Index, Board, List),
    nth0(Index,Board,Row),
    (  Row = List ->
        nth0(X, List, Value),
        nth0(Pos, List, Value),
        nth0(Pos, RotatedList, V),
        iterate([Key-Value|Rest], [Board,_,_], List, V,RotatedList,UpdatedBoard)
    ; true
    ).

% Define a predicate to rotate a hexagon list by a given number of positions and update it
% Update the Values of the Hexagons accordingly to the Board
% hexagon_update(+GameState)
hexagon_update([Board,_,_]) :- 
    nth0(0,Board,Row0),
    assertz(hexagon(0, Row0)),
    nth0(1,Board,Row1),
    assertz(hexagon(1, Row1)),
    nth0(2,Board,Row2),
    assertz(hexagon(2, Row2)),
    nth0(3,Board,Row3),
    assertz(hexagon(3, Row3)),
    nth0(4,Board,Row4),
    assertz(hexagon(4, Row4)),
    nth0(5,Board,Row5),
    assertz(hexagon(5, Row5)),
    nth0(6,Board,Row6),
    assertz(hexagon(6, Row6)).

% predicade To rotate the Hexagons 
% rotate_hexagon(+Number,+N,-RotatedList,+GameState)
rotate_hexagon(Number, N, RotatedList,[Board,_,_]) :-
    retractall(hexagon(Number, _)), % Remove all clauses of hexagon(Number, _)
    hexagon_update([Board,_,_]),
    hexagon(Number, HexagonList),
    rotate_list(HexagonList, N, RotatedList),
    retractall(hexagon(Number, _)), % Remove all clauses of hexagon(Number, _)
    assertz(hexagon(Number, RotatedList)). % Assert the rotated hexagon clause






