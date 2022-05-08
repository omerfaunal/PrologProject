% omer faruk unal
% 2019400048
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].
:- init_from_map.

% 10 points
manhattan_distance([X1, Y1], [X2, Y2], Distance) :- 
    Distance is abs(X1-X2) + abs(Y1-Y2).

% 10 points
minimum_of_list([Minimum], Minimum) :- !.

minimum_of_list([H,K|T], Minimum) :- 
    H >= K, minimum_of_list([K|T], Minimum), !. 

minimum_of_list([H,K|T], Minimum) :- 
    H < K, minimum_of_list([H|T], Minimum), !.

% 10 points
object_position(ObjectDict, Object_X, Object_Y) :- 
    get_dict(x, ObjectDict, Object_X), get_dict(y, ObjectDict, Object_Y).

player_position(AgentDict, Agent_X, Agent_Y) :- 
    get_dict(x, AgentDict, Agent_X), get_dict(y, AgentDict, Agent_Y).

calculate_distance(AgentDict, ObjectDict , CurrDistance) :- 
    player_position(AgentDict, Agent_X, Agent_Y), 
    object_position(ObjectDict, Object_X, Object_Y), 
    manhattan_distance([Agent_X, Agent_Y], [Object_X, Object_Y], CurrDistance).

search_type_and_calculate_distance(AgentDict, CurrObject, CurrObjectType, CurrObjectType, Distance) :-
    calculate_distance(AgentDict, CurrObject, Distance).

search_type_and_calculate_distance(_AgentDict, _CurrObject, _ObjectType, _CurrObjectType, Distance) :-
    Distance is 9999999.

which(X, _Y, X).
which(_X, Y, Y).

calculate_all_distances(State, ObjectType, [H|T], D1, Index1) :-
    %Take ObjectDict and AgentDict from state
    nth0(0, State, AgentDict), nth0(1, State, ObjectDict),
    %Call same function until all keys are iterated ie 0 to last item.
    calculate_all_distances(State, ObjectType, T, D3, Index2),
    %Get Current object and check if the type matches with requested one
    get_dict(H, ObjectDict, CurrObject), get_dict(type, CurrObject, CurrObjectType),
    %Calculate its manhattan distance from player and set distance to D2
    search_type_and_calculate_distance(AgentDict, CurrObject, ObjectType, CurrObjectType, D2),
    %Find which one is smaller, current distance (D2) or previous distance (D3)
    D1 is min(D2, D3),
    %Find which object has distance D1
    which([D2, H], [D3, Index2], [D1, Index1]).
    
calculate_all_distances(_State, _ObjectType, [], Distance, _Index) :-
    %Set current distance as 9999999
    Distance is 9999999. 

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    nth0(1, State, ObjectDict), dict_keys(ObjectDict, Keys),
    %Take all the keys from ObjectDict ie. 0,1,2,3... to be able to iterate
    calculate_all_distances(State, ObjectType, Keys, Distance, ObjKey), Distance < 9999999,
    get_dict(ObjKey, ObjectDict, Object), !.

% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
gen_list(_,0,ListIn, ListIn) :-!.
gen_list(I,N,ListIn,[I|Is]) :- 
    %Add I to ListIn N times.
    %example: gen_list(one,3,[],Out) -> Out = [one, one, one]
    N1 is N-1, gen_list(I,N1,ListIn,Is).

%If N > 0, add go_up N times else add go_down N times
append_y(N, ListIn, ListOut) :-
    N >= 0, gen_list(go_up, N, ListIn, ListOut).
append_y(N, ListIn, ListOut) :-
    N < 0, N2 is abs(N), gen_list(go_down, N2, ListIn, ListOut).
append_y(N, ListIn, ListIn) :-
    N is 0.
%If N > 0, add go_right N times else add go_left N times
append_x(N, ListIn, ListOut) :-
    N >= 0, gen_list(go_right, N, ListIn, ListOut).
append_x(N, ListIn, ListOut) :-
    N < 0, N2 is abs(N), gen_list(go_left, N2, ListIn, ListOut).
append_x(N, ListIn, ListIn) :-
    N is 0.

navigate_to(State, X, Y, ActionList, DepthLimit) :-
    %Find map width and height. Check if X, Y is inside the map
    width(W), W2 is W -2, X =< W2, height(H), H2 is H - 2, Y =< H2,
    %Find player's position
    nth0(0, State, AgentDict), player_position(AgentDict, Agent_X, Agent_Y),
    %Find the moves and check if it is smaller than depth limit
    XOut is X - Agent_X, YOut is Agent_Y - Y, DepthLimit >= abs(XOut) + abs(YOut),
    append_y(YOut, [], LO), append_x(XOut, LO, ActionList), !.
% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
go_nearest_location_and_collect(State, ObjectType, ActionList) :-
    %Find nearest object with given type
    find_nearest_type(State, ObjectType, _Key, Object, _Distance),
    %Find object's HP
    get_dict(x, Object, X), get_dict(y, Object, Y), get_dict(hp, Object, HP), HP2 is HP + 1,
    %Navigate to object and find necessary actions.
    navigate_to(State, X, Y, ActionListOlder, 999999),
    %Add HP + 1 times left_click_c to ActionList
    gen_list(left_click_c, HP2, ActionListOlder, ActionListOld), reverse(ActionListOld, ActionList).

chop_nearest_tree(State, ActionList) :-
    go_nearest_location_and_collect(State, tree, ActionList).

% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
mine_nearest_stone(State, ActionList) :-
    go_nearest_location_and_collect(State, stone, ActionList).
% 10 points
% gather_nearest_food(+State, -ActionList) :- .
gather_nearest_food(State, ActionList) :-
    go_nearest_location_and_collect(State, food, ActionList).
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .

get_actions_for_item(_State, [], []).

% get_actions_for_item(State, RequestedItems, ActionList)
get_actions_for_item(State, [H|T], ActionListOut) :-
    %Collect H and get ActionLost
    go_nearest_location_and_collect(State, H, ActionList),
    %Execute actions and get NewState
    execute_actions(State, ActionList, NewState),
    %Append all Actions to ActionList
    append(ActionList, ActionListOut2, ActionListOut),
    %Get actions for the next item
    get_actions_for_item(NewState, T, ActionListOut2).

need(stone, cobblestone, 3).
need(cobblestone, cobblestone, 1).
need(food, fruits, 2).
need(tree, log, 3).

%Find what and how many to collect from given item
what_to_collect(_, Value, Out, Out) :-
    Value =< 0.
what_to_collect(ItemType, Value, In, Out) :-
    need(Collect, ItemType, HowMany),
    CollectCount is Value - HowMany,
    append([Collect], In, Out2),
    Value > 0,
    what_to_collect(ItemType, CollectCount, Out2, Out).

%find_requirements(ItemType (stone_axe, stone_pickaxe), Requirements)
find_requirements(ItemType, Req) :-
    %Find what we need to find given item.
    (
        (ItemType == castle) -> 
            append([cobblestone], [], Req)
        ;
            item_info(ItemType, Req2, _), dict_keys(Req2, Req)
    ).

calculate_requirements(_, _, [], Out, Out) :- !.
calculate_requirements(State, ItemType, [H|T], In, Out) :-
    (
        (ItemType == castle) -> 
            %If the item is castle then we need to find 9 cobblestone
            Value is 9
        ; %else
            %Figure out how many item we need
            item_info(ItemType, Req2, _), get_dict(H, Req2, Value)
    ),
    nth0(0, State, AgentDict),
    get_dict(inventory, AgentDict, Bag),
    (get_dict(H, Bag, Value2) -> Value3 is Value2 ; Value3 is 0),
    %Find how many items are there in the agent's inventory
    LastValTemp is Value - Value3,
    ((LastValTemp < 0) -> LastVal is 0; LastVal is LastValTemp),
    %Subtract what we have from what we need and find necessary count
    (
        (H == stick) -> 
            %If the current H is stick
            (
                (LastVal =< 0) -> 
                    %If we have enough stick then do not add anything to CollectList
                    append([], [], CollectList)
                ; 
                    %Else find if we have enough log, if so add nothing, else add tree to CollectList
                    item_info(ItemType, ReqStick, _), get_dict(log, ReqStick, ValueStick),
                    get_dict(inventory, AgentDict, BagStick),
                    (get_dict(log, BagStick, ValueStick2) -> ValueStick3 is ValueStick2 ; ValueStick3 is 0),
                    LastValTempStick is ValueStick - ValueStick3,
                    (
                        (LastValTempStick + 2 =< 0) -> 
                            append([], [], CollectList)
                        ; 
                            append([tree], [], CollectList
                        )
                    )
            )
        ; 
            %Add CollectList to necessary items for current H
            what_to_collect(H, LastVal, [], CollectList)
    ),
    append(CollectList, In, Out2),
    calculate_requirements(State, ItemType, T, Out2, Out).


collect_requirements(State, ItemType, ActionList) :- 
    %Find the list of requirements
    find_requirements(ItemType, Req),
    %Calculate the count of the requirements
    calculate_requirements(State, ItemType, Req, [], Requirements),
    %Go and collect all the requirements
    get_actions_for_item(State, Requirements, ActionListTemp),
    (   
        (ItemType = castle) -> 
            %If the requested item is castle do not craft stick
            append(ActionListTemp, [], ActionList)
        ;   
            %Else if we have enough stick do not craft stick
            %Else craft stick
            nth0(0, State, AgentDict),
            get_dict(inventory, AgentDict, Bag),
            (get_dict(stick, Bag, Value2) -> Value3 is Value2 ; Value3 is 0),
            ((Value3 > 1) -> append(ActionListTemp, [], ActionList) ; append(ActionListTemp, [craft_stick], ActionList))
    ).

% 5 points
find_castle_location(State, XMin, YMin, XMax, YMax) :- 
    width(W2), height(H2), W is W2 - 3, H is H2 - 3, 
    %Iterate from 2,2 to width -3, height -3 and find suitable locations for castle
    %It will find square with centered X, Y. That's why function starts to iterate from 2.
    iterate_map(State, 2, 2, 2, W, H, XMin, YMin, XMax, YMax).

iterate_map(State, XStart, CurrX, CurrY, Width, Height, XMin, YMin, XMax, YMax) :- 
    (
        is_square_not_occupied(CurrX, CurrY, State) -> 
            %If square is not occupied we find our coordinates.
            %The center of the square is CurrX and CurrY. We can easily find XMax, YMax.
            XMin is CurrX - 1, YMin is CurrY - 1, XMax is CurrX + 1, YMax is CurrY + 1, !
        ; %else
            (
                (CurrX < Width) -> 
                    %Increase CurrX by one if it is smaller than width
                    CurrX2 is CurrX + 1, CurrY2 is CurrY
                ; %else
                    (
                        (CurrY < Height) -> 
                            %Increase Y by one.
                            CurrX2 is XStart, CurrY2 is CurrY + 1
                        ; %else
                            %We couldn't find sutibale location 
                            fail
                    )
            ),
    iterate_map(State, XStart, CurrX2, CurrY2, Width, Height, XMin, YMin, XMax, YMax)).
    

is_tile_occupied(X, Y, State) :-
    %Check if there is an item in the given coordinates.
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy, !.

is_tile_not_occupied(X, Y, State) :-
    not(is_tile_occupied(X, Y, State)).

is_square_not_occupied(X, Y, State) :-
    %Check if the 3x3 square with center X,Y is occupied
    WX is X - 1, EX is X + 1, NY is Y - 1, SY is Y + 1,
    is_tile_not_occupied(WX, NY, State), is_tile_not_occupied(X, NY, State), is_tile_not_occupied(EX, NY, State),
    is_tile_not_occupied(WX, Y, State), is_tile_not_occupied(X, Y, State), is_tile_not_occupied(EX, Y, State),
    is_tile_not_occupied(WX, SY, State), is_tile_not_occupied(X, SY, State), is_tile_not_occupied(EX, SY, State).

% 15 points
make_castle(State, ActionList) :- 
    %Collect requirements ie. 9 cobblestones
    collect_requirements(State, castle, ActionList2),
    execute_actions(State, ActionList2, NewState),
    %Find suitable location to build castle
    find_castle_location(NewState, XMin, YMin, XMax, YMax), 
    X2 is (XMin + XMax) / 2, Y2 is (YMin + YMax) / 2,
    %Find path to go to that location
    navigate_to(NewState, X2, Y2, ActionList3, 99999),
    append(ActionList2, ActionList3, Temp),
    append(Temp, [place_nw, place_n, place_ne, place_w, place_c, place_e, place_sw, place_s, place_se], ActionList), !.
