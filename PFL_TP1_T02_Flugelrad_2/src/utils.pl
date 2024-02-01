% Clears console
% clear_console/0
clear_console:- 
    write('\33\[2J').

% Show the name of the Player who is playing
% display_Player(+GameState,+Player)
display_Player([_,_,Player],Player) :-
    name_of(Player, Name),
write('--------------------------\n'),
format('       ~a\'s turn\n', [Name]),
write('--------------------------\n').

% Show the Hexagons you cant move because all the positions are empty
% display_Not_Allowed(+GameState,+ListOfMoves)
display_Not_Allowed(GameState,ListOfMoves) :- 
    write('--------------------------\n'),
    write(' Not Allowed Hexagon Move   \n'),
    format('         ~w\n', [ListOfMoves]),
    write('--------------------------\n').

% predicade that Given a Low and a High it will generate a Random value between this two values
% random_betweenT(+Low,+High,-Random)
random_betweenT(Low, High, Random) :-
    Range is High - Low + 1,
    random(R),
    Random is Low + floor(R * Range).

% predicade that Given a Low and a High it will generate a Random value between this two values
% Taking into account a list of Hexagons that are not allowed to move because they have all the elements empty
% random_betweenN(+Low,+High,+ListOfMoves,-Random)
random_betweenN(Low, High,ListOfMoves, Random) :-
    Range is High - Low + 1,
    random(R),
    Random is Low + floor(R * Range),
    ((member(Random,ListOfMoves) -> repeat);true).

% Separate lists based on 'e', 'a', and 'v' values form the PosLists.
% separate_lists(+PosLists, -ALists, -VLists, -ELists)
separate_lists([], [], [], []).
separate_lists([[Index, Value] | Rest], ALists, VLists, ELists) :-
    (Value = a -> ALists = [Index | ALists1], VLists = VLists1, ELists = ELists1;
     Value = v -> VLists = [Index | VLists1], ALists = ALists1, ELists = ELists1;
     Value = e -> ELists = [Index | ELists1], ALists = ALists1, VLists = VLists1;
     ALists = ALists1, VLists = VLists1, ELists = ELists1),
    separate_lists(Rest, ALists1, VLists1, ELists1).

% Separate lists by 'a', 'v', and 'e' values from the PosLists.
% separate_lists_by_value(+PosLists, -ALists, -VLists, -ELists)
separate_lists_by_value(PosLists, ALists, VLists, ELists) :-
    separate_lists(PosLists, ALists, VLists, ELists).


% Auxiliary predicade to get all nodes
% Checks if there are any connections between nodes 
% source_or_dest(+N)
source_or_dest(N) :- connected(N,_).
source_or_dest(N) :- connected(_,N).

% predicade that using the source_or_dest(N) predicade will return in List L all the nodes that have connection
% get_all_nodes(-L)
get_all_nodes(L) :-
    findall(N, source_or_dest(N), Nodes),
    sort(Nodes, L).

% Check if an element is a member of a list and return the remaining elements
% member(+Element,+List,-Rest)
member(Element, [Element|Rest], Rest).
member(Element, [Head|Rest], [Head|Rest1]) :-
    member(Element, Rest, Rest1).

% Predicade that given an Index, a List and a NewElement will return a NewList with the NewElement in the Index position
% replace_at(+Index,+List,+NewElement,-NewList)
replace_at(Index, List, NewElement, NewList) :-
    nth0(Index, List, _, TempList),
    nth0(Index, NewList, NewElement, TempList).

% Define a predicate to rotate a list
% rotate_list(+List,+N,-RotatedList)
rotate_list(List, 0, List).
rotate_list([H|T], N, RotatedList) :-
    N > 0,
    N1 is N - 1,
    append(T, [H], RotatedList1),
    rotate_list(RotatedList1, N1, RotatedList).

% Predicade to attribute to the players the number 0
% Note that when the number1 is attributed to a player using the winner predicade it means that he won the game
clean_winner :-
    asserta(winner(player1,0)),
    asserta(winner(player2,0)),
    asserta(winner(bot,0)),
    asserta(winner(bot1,0)),
    asserta(winner(bot2,0)).