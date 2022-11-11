-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").


get_ship(Shipping_State, Ship_ID) ->
    My_Ship = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    case My_Ship of
        false ->
            error;
        _ ->
            My_Ship
    end.

get_container(Shipping_State, Container_ID) ->
    My_Container = lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers),
    case My_Container of
        false ->
            error;
        _ ->
            My_Container
    end.

get_port(Shipping_State, Port_ID) ->
    My_Port = lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports),
    case My_Port of
        false ->
            error;
        _ ->
            My_Port
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    My_Port = get_port(Shipping_State, Port_ID),
    case My_Port of
        error ->
            error;
        _ ->
            [element(2, X) || X <- Shipping_State#shipping_state.ship_locations, element(1, X) =:= Port_ID]
    end.

get_ship_location(Shipping_State, Ship_ID) ->
    My_Ship_Location = lists:keyfind(Ship_ID, 3, Shipping_State#shipping_state.ship_locations),
    case My_Ship_Location of
        false ->
            error;
        _ ->
            {element(1, My_Ship_Location), element(2, My_Ship_Location)}
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    case Container_IDs of
        [] ->
            0;
        [H | T] ->
            % io:format("Container ID: ~p~n", [H]),
            My_Container = get_container(Shipping_State, H),
            % io:format("Container: ~p~n", [My_Container]),
            case My_Container of
                error ->
                    error;
                _ ->
                    My_Container#container.weight + get_container_weight(Shipping_State, T)
            end
            
    end.
    
get_ship_weight(Shipping_State, Ship_ID) ->
    My_Ship = get_ship(Shipping_State, Ship_ID),
    case My_Ship of
        error ->
            error;
        _ ->
            % io:format("Ship: ~p~n", [Shipping_State#shipping_state.ship_inventory]),
            ShipContainers = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory), 
            get_container_weight(Shipping_State, ShipContainers)
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    % {My_Ship_Location, _} = get_ship_location(Shipping_State, Ship_ID),
    case Container_IDs of
        [] ->
            Shipping_State;
        [H | T] ->
            My_Container = get_container(Shipping_State, H),
            case My_Container of
                error ->
                    error;
                _ ->
                    My_Ship = get_ship(Shipping_State, Ship_ID),
                    case My_Ship of
                        error ->
                            error;
                        _ ->
                            My_Ship_Capacity = My_Ship#ship.container_cap,
                            % io:format("Ship Capacity: ~p~n", [My_Ship_Capacity]),
                            case length(Container_IDs) + length(maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)) > My_Ship_Capacity of
                                true ->
                                    error;
                                false ->
                                    My_Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                                    New_Ship_Inventory = [H | My_Ship_Inventory],
                                    New_Ship_Inventory_Map = maps:put(Ship_ID, New_Ship_Inventory, Shipping_State#shipping_state.ship_inventory),
                                    New_Shipping_State = Shipping_State#shipping_state{ship_inventory = New_Ship_Inventory_Map},
                                    load_ship(New_Shipping_State, Ship_ID, T)
                            end
                    end
            end

    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    My_Ship_Capacity = maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    % io:format("My_Ship_Capacity: ~p~n", [My_Ship_Capacity]),
    unload_ship(Shipping_State, Ship_ID, element(2,My_Ship_Capacity)).

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port, _} = get_ship_location(Shipping_State, Ship_ID),
    Port_Capacity = (get_port(Shipping_State, Port))#port.container_cap,
    My_Ship_Capacity = maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    My_Port_Capacity = maps:find(Port, Shipping_State#shipping_state.port_inventory),
    case(length(element(2,My_Port_Capacity)) + length(Container_IDs) =< Port_Capacity) and is_sublist(element(2,My_Ship_Capacity), Container_IDs) of
        true ->
            New_Ship_Capacity = lists:filter(fun (X) -> not lists:member(X, Container_IDs) end, element(2,My_Ship_Capacity)),
            % io:format("New_Ship_Capacity: ~p~n", [New_Ship_Capacity]),
            New_Port_Capacity =  lists:merge(element(2,My_Port_Capacity), Container_IDs),
            % io:format("New_Port_Capacity: ~p~n", [New_Port_Capacity]),
            #shipping_state{
                        ships = Shipping_State#shipping_state.ships,
                        containers = Shipping_State#shipping_state.containers,
                        ports = Shipping_State#shipping_state.ports,
                        ship_locations = Shipping_State#shipping_state.ship_locations,
                        ship_inventory = maps:put(Ship_ID ,New_Ship_Capacity,Shipping_State#shipping_state.ship_inventory),
                        port_inventory = maps:put(Port, New_Port_Capacity, Shipping_State#shipping_state.port_inventory)
                        };
        false ->
            error
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Docks_With_Ship = [element(2, X) || X <- Shipping_State#shipping_state.ship_locations, element(1, X) =:= Port_ID],
    case lists:member(Dock, Docks_With_Ship) of
        true ->
            error;
        false ->
            My_Ship = get_ship(Shipping_State, Ship_ID),
            case My_Ship of
                error ->
                    error;
                _ ->
                    My_Ship_Capacity = My_Ship#ship.container_cap,
                    My_Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                    case length(My_Ship_Inventory) =< My_Ship_Capacity of
                        true ->
                            New_Shipping_State = Shipping_State#shipping_state{ship_locations = [{Port_ID, Dock, Ship_ID} | Shipping_State#shipping_state.ship_locations]},
                            New_Shipping_State;
                        false ->
                            error
                    end
            end
    end.

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
